open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Js_of_ocaml

type merchant = { key : string; owner : string; logo : string option; can_retry : bool; description : string; row_id : string }

let failure locale code =
  match code with
  | "invalid_link" | "unsafe_address" | "invalid_owner" | "invalid_merchant_key" -> Msg.logo_invalid_link locale
  | "unsupported_image" | "too_small_image" | "oversized_body" -> Msg.logo_not_image locale
  | "unreachable_site" | "redirect_limit" -> Msg.logo_unreachable locale
  | "forbidden" -> Msg.logo_forbidden locale
  | _ -> Msg.logo_unreachable locale

let tile_class merchant =
  let color = ((String.hash merchant.description % 5 + 5) % 5) + 1 in
  sprintf "tile c%d logo-tile" color

let image merchant =
  [ Ui.span ~cls:"logo-initial" [ Ui.text (Ui.initial merchant.description) ] ] @
  match merchant.logo with
  | Some url when String.is_prefix url ~prefix:"/api/logos/" ->
      [ Vdom.Node.img ~attrs:[ Vdom.Attr.src url; Vdom.Attr.alt "";
          Vdom.Attr.on_load (fun e ->
            Js.Opt.iter e##.target (fun target ->
              Js.Unsafe.set (Js.Unsafe.get target "style") "display" (Js.string "block"));
            Effect.Ignore);
          Vdom.Attr.on_error (fun e ->
            Js.Opt.iter e##.target (fun target ->
              Js.Unsafe.set (Js.Unsafe.get target "style") "display" (Js.string "none"));
            Effect.Ignore) ] () ]
  | _ -> []

let tile ~locale ~merchant ~open_ () =
  let id = "logo-" ^ Md5.to_hex (Md5.digest_string merchant.row_id) in
  Vdom.Node.button
    ~attrs:[ Vdom.Attr.type_ "button"; Vdom.Attr.class_ (tile_class merchant); Vdom.Attr.id id;
      Vdom.Attr.create "aria-label" (Msg.logo_edit ~merchant:merchant.description locale);
      Vdom.Attr.on_click (fun _ -> open_ (merchant, id)) ]
    (image merchant)

let component ~(ctx : Ctx.t) graph =
  let selected, set_selected = Bonsai.state None graph in
  let draft, set_draft = Bonsai.state "" graph in
  let pending, set_pending = Bonsai.state false graph in
  let error, set_error = Bonsai.state None graph in
  let show_remove, set_show_remove = Bonsai.state false graph in
  let focus =
    let open_ = let%arr selected in Option.is_some selected in
    match%sub open_ with true -> Bonsai_web.Effect.Focus.on_activate () graph | false -> Bonsai.return Vdom.Attr.empty
  in
  let%arr selected and set_selected and draft and set_draft and pending and set_pending and error and set_error and focus
  and show_remove and set_show_remove and locale = ctx.locale and changed = ctx.changed in
  let open_ (merchant, id) = Effect.Many [set_selected (Some (merchant, id)); set_draft ""; set_error None; set_show_remove false] in
  let close id =
    let restore = Effect.of_sync_fun (fun id ->
      Js.Opt.iter (Dom_html.document##getElementById (Js.string id)) (fun element ->
        ignore (Js.Unsafe.meth_call element "focus" [||] : Js.Unsafe.any))) id in
    Effect.Many [set_selected None; set_error None; set_show_remove false; restore]
  in
  let view = match selected with
  | None -> Vdom.Node.none
  | Some (merchant, id) ->
      let args fields = `Assoc (("merchant_key", `String merchant.key) :: ("owner", `String merchant.owner) :: fields) in
      let perform name fields =
        let focus_link = Effect.of_sync_fun (fun () ->
          Js.Opt.iter (Dom_html.document##getElementById (Js.string "logo-link")) (fun element ->
            ignore (Js.Unsafe.meth_call element "focus" [||] : Js.Unsafe.any))) () in
        let%bind.Effect () = focus_link in
        let%bind.Effect () = set_pending true in
        let%bind.Effect result = Api.run_with_code name (args fields) in
        let%bind.Effect () = set_pending false in
        match result with
        | Ok _ -> Effect.Many [changed; close id]
        | Error code -> set_error (Some (failure locale code))
      in
      let preview =
        if Option.is_some merchant.logo then
          Vdom.Node.button
            ~attrs:([ Vdom.Attr.type_ "button"; Vdom.Attr.class_ (tile_class merchant ^ " logo-preview-tile logo-preview-control");
              Vdom.Attr.title (Msg.logo_options locale); Vdom.Attr.create "aria-label" (Msg.logo_options locale);
              Vdom.Attr.create "aria-expanded" (Bool.to_string show_remove);
              Vdom.Attr.on_click (fun _ -> set_show_remove (not show_remove)) ] @ if pending then [Vdom.Attr.disabled] else [])
            (image merchant)
        else Vdom.Node.span ~attrs:[Vdom.Attr.class_ (tile_class merchant ^ " logo-preview-tile")] (image merchant)
      in
      let header = Ui.div ~cls:"logo-header"
        [ Ui.div ~cls:"logo-identity"
            [ Ui.div ~cls:"logo-preview"
                [ preview;
                  (if show_remove && Option.is_some merchant.logo then
                     Ui.div ~cls:"logo-remove"
                       [Ui.icon_button ~label:(Msg.logo_remove locale) ~disabled:pending Trash_2
                          (perform "clear_merchant_logo" ["mode", `String "none"])]
                   else Vdom.Node.none) ];
              Vdom.Node.h2 ~attrs:[Vdom.Attr.class_ "logo-name"] [Ui.text merchant.description];
              (if merchant.can_retry then
                 Ui.div ~cls:"logo-retry" [Ui.icon_button ~label:(Msg.logo_automatic locale) ~disabled:pending Refresh_cw
                   (perform "clear_merchant_logo" ["mode", `String "automatic"])]
               else Vdom.Node.none) ];
          Ui.div ~cls:"logo-close" [Ui.icon_button ~label:(Msg.action_close locale) ~disabled:pending X (close id)] ] in
      let can_submit = not pending && not (String.is_empty (String.strip draft)) in
      let form = Vdom.Node.form
        ~attrs:[Vdom.Attr.class_ "logo-form";
          Vdom.Attr.on_submit (fun _ -> Effect.Many [Effect.Prevent_default;
            if can_submit then perform "set_merchant_logo" ["link", `String draft] else Effect.Ignore])]
        [ Ui.div ~cls:"field"
            [ Vdom.Node.label ~attrs:[Vdom.Attr.class_ "field-label"; Vdom.Attr.create "for" "logo-link"] [Ui.text (Msg.logo_link locale)];
              Ui.div ~cls:"logo-link-row"
                [ Vdom.Node.input ~attrs:[focus; Vdom.Attr.id "logo-link"; Vdom.Attr.type_ "text";
                    Vdom.Attr.create "inputmode" "url"; Vdom.Attr.value_prop draft; Vdom.Attr.placeholder "ica.se";
                    Vdom.Attr.on_input (fun _ value -> set_draft value)] ();
                  Vdom.Node.button
                    ~attrs:([Vdom.Attr.type_ "submit"; Vdom.Attr.class_ "button primary logo-submit";
                      Vdom.Attr.title (Msg.logo_apply locale); Vdom.Attr.create "aria-label" (Msg.logo_apply locale)]
                      @ if can_submit then [] else [Vdom.Attr.disabled])
                    [Feather_icon.svg ~size:(`Px 16) ~stroke:(`Name "currentColor") ~stroke_width:(`Px 2) Arrow_right] ] ] ] in
      Vdom.Node.div ~attrs:[Vdom.Attr.class_ "overlay logo-overlay";
          Vdom.Attr.Global_listeners.keydown (fun e ->
            match Js.Optdef.case e##.key (fun () -> "") Js.to_string with
            | "Escape" when not pending -> Effect.Many [Effect.Prevent_default; close id]
            | "Tab" ->
                let sheet = Dom_html.document##getElementById (Js.string "logo-dialog") in
                let controls = Js.Unsafe.meth_call sheet "querySelectorAll" [|Js.Unsafe.inject (Js.string "input, button:not([disabled])")|] in
                let length : int = Js.Unsafe.get controls "length" in
                if length = 0 then Effect.Prevent_default else
                let first = Js.Unsafe.meth_call controls "item" [|Js.Unsafe.inject 0|] in
                let last = Js.Unsafe.meth_call controls "item" [|Js.Unsafe.inject (length - 1)|] in
                let active = Js.Unsafe.get Dom_html.document "activeElement" in
                let shift : bool = Js.Unsafe.get e "shiftKey" in
                let inside : bool = Js.Unsafe.meth_call sheet "contains" [|Js.Unsafe.inject active|] in
                if not inside || (shift && phys_equal active first) || ((not shift) && phys_equal active last) then (
                  ignore (Js.Unsafe.meth_call (if shift then last else first) "focus" [||] : Js.Unsafe.any);
                  Effect.Prevent_default) else Effect.Ignore
            | _ -> Effect.Ignore) ]
        [Vdom.Node.div ~attrs:[Vdom.Attr.class_ "sheet logo-sheet"; Vdom.Attr.create "role" "dialog";
          Vdom.Attr.create "aria-modal" "true"; Vdom.Attr.create "aria-label" (Msg.logo_edit ~merchant:merchant.description locale);
          Vdom.Attr.id "logo-dialog"]
          [ header; form;
            (match error with None -> Vdom.Node.none | Some message -> Ui.div ~cls:"error" [Ui.text message]) ] ]
  in
  ((fun merchant -> tile ~locale ~merchant ~open_ ()), view)
