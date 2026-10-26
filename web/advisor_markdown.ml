(* CommonMark is parsed by cmarkit; only this closed VDOM vocabulary is rendered.
   Raw HTML stays text. Model links/images never become navigable/fetching nodes. *)
open Bonsai_web
module M = Cmarkit

exception Too_complex

let render source =
  let text = Vdom.Node.text in
  let node tag children = Vdom.Node.create tag children in
  let fallback () = Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "advisor-markdown advisor-plain" ] [ text source ] in
  let remaining = ref 5000 in
  let check depth =
    decr remaining;
    if depth > 48 || !remaining < 0 then raise Too_complex
  in
  let rec inline depth i =
    check depth;
    let child = inline (depth + 1) in
    let open M.Inline in
    match i with
    | M.Inline.Text (s, _) -> text s
    | Inlines (xs, _) -> node "span" (List.map child xs)
    | Break (b, _) -> ( match M.Inline.Break.type' b with `Hard -> node "br" [] | `Soft -> text "\n")
    | Code_span (s, _) -> node "code" [ text (M.Inline.Code_span.code s) ]
    | Emphasis (e, _) -> node "em" [ child (M.Inline.Emphasis.inline e) ]
    | Strong_emphasis (e, _) -> node "strong" [ child (M.Inline.Emphasis.inline e) ]
    | Link (l, _) | Image (l, _) -> child (M.Inline.Link.text l)
    | Autolink (l, _) -> text (fst (M.Inline.Autolink.link l))
    | Raw_html (lines, _) -> text (String.concat "\n" (List.map M.Block_line.tight_to_string lines))
    | Ext_strikethrough (s, _) -> node "del" [ child (M.Inline.Strikethrough.inline s) ]
    | Ext_math_span (m, _) -> node "code" [ text (M.Inline.Math_span.tex m) ]
    | _ -> raise Too_complex
  and block depth b =
    check depth;
    let child = block (depth + 1) in
    let open M.Block in
    match b with
    | M.Block.Blocks (xs, _) -> node "div" (List.map child xs)
    | Paragraph (p, _) -> node "p" [ inline (depth + 1) (M.Block.Paragraph.inline p) ]
    | Heading (h, _) ->
        node
          ("h" ^ string_of_int (min 6 (M.Block.Heading.level h + 1)))
          [ inline (depth + 1) (M.Block.Heading.inline h) ]
    | Block_quote (q, _) -> node "blockquote" [ child (M.Block.Block_quote.block q) ]
    | Code_block (c, _) | Ext_math_block (c, _) ->
        node "pre"
          [ node "code" [ text (String.concat "\n" (List.map M.Block_line.to_string (M.Block.Code_block.code c))) ] ]
    | Html_block (lines, _) -> node "p" [ text (String.concat "\n" (List.map M.Block_line.to_string lines)) ]
    | List (l, _) ->
        let attrs, tag =
          match M.Block.List'.type' l with
          | `Unordered _ -> ([], "ul")
          | `Ordered (n, _) -> ([ Vdom.Attr.create "start" (string_of_int n) ], "ol")
        in
        Vdom.Node.create tag ~attrs
          (List.map (fun (item, _) -> node "li" [ child (M.Block.List_item.block item) ]) (M.Block.List'.items l))
    | Thematic_break _ -> node "hr" []
    | Ext_table (t, _) ->
        let count = M.Block.Table.col_count t in
        if count > 32 then raise Too_complex;
        let row tag cells =
          let cells =
            List.init count (fun i ->
                let content = match List.nth_opt cells i with None -> text "" | Some (i, _) -> inline (depth + 1) i in
                check (depth + 1);
                node tag [ content ])
          in
          node "tr" cells
        in
        let headers, rows =
          List.fold_left
            (fun (hs, rs) ((r, _), _) ->
              match r with
              | `Header xs -> (row "th" xs :: hs, rs)
              | `Data xs -> (hs, row "td" xs :: rs)
              | `Sep _ -> (hs, rs))
            ([], []) (M.Block.Table.rows t)
        in
        Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "advisor-table" ]
          [ node "table" [ node "thead" (List.rev headers); node "tbody" (List.rev rows) ] ]
    | Ext_footnote_definition (f, _) -> child (M.Block.Footnote.block f)
    | Blank_line _ | Link_reference_definition _ -> Vdom.Node.none
    | _ -> raise Too_complex
  in
  if String.length source > 65536 then fallback ()
  else
    try
      let doc = M.Doc.of_string ~strict:false source in
      Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "advisor-markdown" ] [ block 0 (M.Doc.block doc) ]
    with Too_complex | Stack_overflow -> fallback ()
