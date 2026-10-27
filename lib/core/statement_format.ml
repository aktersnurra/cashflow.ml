(* Bank statement formats. Each is a first-class module behind one signature, so supporting a
   bank is adding one module to [formats]; everything after parsing is shared. *)
type row = {
  booking_date : Date.t;  (** when the bank booked it: the transfer's value date *)
  transaction_date : Date.t option;  (** when the purchase happened, if the bank says *)
  description : string;
  counterparty : string option;
  amount : int64;  (** signed minor units: positive is money in *)
  balance : int64 option;  (** the running balance after this row *)
  reference : string option;  (** the bank's own transaction id, if any *)
}

(* What a file says about the account it was exported from, read from the preamble above the
   header row. Only the last four digits of a number are kept. *)
type identity = { institution : string option; account_name : string option; last4 : string option }

let no_identity = { institution = None; account_name = None; last4 = None }

module type S = sig
  val name : string

  val detect : Csv.table -> bool
  (** Whether a file looks like this format. *)

  val parse : scale:int -> Csv.table -> (row list, string) result

  val identity : Csv.table -> identity
  (** Which account the file says it is for, as far as it says. *)
end

(* Amounts as Swedish and English banks print them: "1 234,56", "-1234.56", "1.234,56", "−12,00". *)
let parse_amount ~scale text =
  let text =
    String.trim text |> Str_compat.replace_all ~sub:"\xE2\x88\x92" ~by:"-" |> Str_compat.replace_all ~sub:"\xC2\xA0" ~by:""
    |> Str_compat.replace_all ~sub:" " ~by:"" |> Str_compat.replace_all ~sub:"kr" ~by:"" |> Str_compat.replace_all ~sub:"SEK" ~by:""
  in
  let last_comma = String.rindex_opt text ',' and last_dot = String.rindex_opt text '.' in
  let decimal_separator =
    match last_comma, last_dot with
    | Some c, Some d -> Some (if c > d then ',' else '.')
    | Some c, None -> if String.length text - c - 1 <= scale then Some ',' else None
    | None, Some d -> if String.length text - d - 1 <= scale then Some '.' else None
    | None, None -> None
  in
  let normalized =
    String.to_seq text
    |> Seq.filter_map (fun c ->
           match decimal_separator with
           | Some sep when c = sep -> Some '.'
           | _ when c = ',' || c = '.' -> None
           | _ -> Some c)
    |> String.of_seq
  in
  Result.to_option (Decimal.parse ~scale normalized)

(* Spreadsheets can store a date as a day number counted from 1899-12-30. *)
let excel_epoch = Option.get (Date.of_ymd 1899 12 30)

let parse_date text =
  let text = String.trim text in
  match Date.of_string text with
  | Ok date -> Some date
  | Error _ when String.length text = 5 && String.for_all (fun c -> c >= '0' && c <= '9') text -> Some (Date.add_days excel_epoch (int_of_string text))
  | Error _ -> (
      (* Also accept YYYYMMDD and YYYY/MM/DD. *)
      let digits = String.concat "" (String.split_on_char '/' text) in
      if String.length digits = 8 && String.for_all (fun c -> c >= '0' && c <= '9') digits then
        Result.to_option (Date.of_string (String.sub digits 0 4 ^ "-" ^ String.sub digits 4 2 ^ "-" ^ String.sub digits 6 2))
      else None)

let normalize_header h = String.lowercase_ascii (String.trim h)

(* Column names as banks write them, in Swedish and English, lowercased. *)
let synonyms =
  [ (`Booking, [ "bokföringsdag"; "bokföringsdatum"; "reskontradatum"; "bokfört"; "datum"; "date"; "booking date"; "posting date" ]);
    (`Transaction, [ "transaktionsdatum"; "transaktionsdag"; "köpdatum"; "transaction date"; "purchase date" ]);
    (`Description, [ "text"; "beskrivning"; "transaktion"; "rubrik"; "specifikation"; "description"; "details"; "memo" ]);
    (`Counterparty, [ "mottagare"; "motpart"; "namn"; "counterparty"; "payee"; "merchant" ]);
    (`Amount, [ "belopp"; "amount"; "summa" ]);
    (`Out, [ "uttag"; "debet"; "debit"; "withdrawal" ]);
    (`In, [ "insättning"; "kredit"; "credit"; "deposit" ]);
    (`Balance, [ "saldo"; "bokfört saldo"; "balance"; "running balance" ]);
    (`Reference, [ "referens"; "verifikationsnummer"; "reference"; "transaction id" ]) ]

(* The first row that names a date and an amount (or in/out columns) is the header; anything
   above it is the bank's preamble (account name, period). *)
let find_header table =
  let column headers kind =
    let names = List.assoc kind synonyms in
    let rec go i = function [] -> None | h :: rest -> if List.mem (normalize_header h) names then Some i else go (i + 1) rest in
    go 0 headers
  in
  let rec go index = function
    | [] -> None
    | headers :: rest ->
        let has kind = Option.is_some (column headers kind) in
        if (has `Booking || has `Transaction) && (has `Amount || (has `Out && has `In)) then Some (index, column headers)
        else go (index + 1) rest
  in
  go 0 table

(* The rows above the header: the bank's preamble. *)
let preamble table =
  let header = match find_header table with Some (i, _) -> i | None -> List.length table in
  List.filteri (fun i _ -> i < header) table |> List.concat |> List.map String.trim |> List.filter (( <> ) "")

let is_digits w = w <> "" && String.for_all (fun c -> c >= '0' && c <= '9') w

(* "Lönekonto ****1234", "Card *1234": a name, then a masked number. *)
let masked_account cell =
  match String.rindex_opt cell '*' with
  | Some star when not (String.contains cell ':') ->
      let digits = String.trim (String.sub cell (star + 1) (String.length cell - star - 1)) in
      let before = String.trim (String.sub cell 0 star) in
      let name = String.trim (String.concat "" (String.split_on_char '*' before)) in
      if String.length digits = 4 && is_digits digits then Some ((if name = "" then None else Some name), digits) else None
  | _ -> None

module Generic : S = struct
  let name = "generic"
  let detect table = Option.is_some (find_header table)

  let identity table =
    match List.find_map masked_account (preamble table) with
    | Some (account_name, last4) -> { no_identity with account_name; last4 = Some last4 }
    | None -> no_identity

  let parse ~scale table =
    match find_header table with
    | None -> Error "no header row with a date and an amount column"
    | Some (header_index, column) ->
        let rows = List.filteri (fun i _ -> i > header_index) table in
        let cell row kind = Option.bind (column kind) (fun i -> List.nth_opt row i) |> Option.map String.trim |> Option.map (fun s -> if s = "" then None else Some s) |> Option.join in
        let amount row =
          match cell row `Amount with
          | Some text -> parse_amount ~scale text
          | None ->
              let get kind = match cell row kind with None -> Some 0L | Some t -> Option.map Int64.abs (parse_amount ~scale t) in
              Option.bind (get `In) (fun i -> Option.map (fun o -> Int64.sub i o) (get `Out))
        in
        let rec go line acc = function
          | [] -> Ok (List.rev acc)
          | row :: rest -> (
              let booking = match cell row `Booking with Some t -> parse_date t | None -> Option.bind (cell row `Transaction) parse_date in
              let transaction = Option.bind (cell row `Transaction) parse_date in
              match booking, amount row with
              | Some booking_date, Some amount ->
                  let balance = Option.bind (cell row `Balance) (parse_amount ~scale) in
                  let description = Option.value (cell row `Description) ~default:"" in
                  let counterparty = cell row `Counterparty in
                  go (line + 1)
                    ({ booking_date; transaction_date = transaction; description; counterparty; amount; balance; reference = cell row `Reference } :: acc)
                    rest
              | None, _ when List.for_all (fun c -> String.trim c = "") row -> go (line + 1) acc rest
              | None, _ -> Error (Printf.sprintf "line %d: no readable date" (header_index + line + 2))
              | _, None -> Error (Printf.sprintf "line %d: no readable amount" (header_index + line + 2)))
        in
        go 0 [] rows
end

(* Handelsbanken's account export (.xlsx, or the same cells as CSV): a preamble with the bank's
   name, the export time, the account's name and number, its type, clearing number, balance and
   period; then Reskontradatum, Transaktionsdatum, Text, Belopp and Saldo, newest first. The
   columns are ones the generic reader knows; what this adds is recognising the file and reading
   the account number, so a file can't be imported into the wrong account. *)
module Handelsbanken : S = struct
  let name = "handelsbanken"
  let first_cell table = List.find_map (function c :: _ when String.trim c <> "" -> Some (String.trim c) | _ -> None) table
  let detect table = first_cell table = Some "Handelsbanken" && Generic.detect table

  (* The preamble line naming the account: its name followed by the number in groups of digits
     ("Lönekonto 123 456 789"). The clearing number has its own labelled cell, so a cell with a
     label (a colon) is never the account. *)
  let identity table =
    let account cell =
      let words = List.filter (( <> ) "") (String.split_on_char ' ' cell) in
      let rec split acc = function w :: rest when is_digits w -> split (w :: acc) rest | rest -> (List.rev rest, acc) in
      let name, groups = split [] (List.rev words) in
      let d = String.concat "" groups in
      if String.contains cell ':' || String.length d < 6 then None
      else Some ((match name with [] -> None | words -> Some (String.concat " " words)), String.sub d (String.length d - 4) 4)
    in
    let account_name, last4 =
      match List.find_map account (preamble table) with Some (name, last4) -> (name, Some last4) | None -> (None, None)
    in
    { institution = Some "Handelsbanken"; account_name; last4 }

  let parse = Generic.parse
end

(* Specific formats go first; the generic reader is the fallback. *)
let formats : (module S) list = [ (module Handelsbanken); (module Generic) ]
let detect table = List.find_opt (fun (module F : S) -> F.detect table) formats
let names = List.map (fun (module F : S) -> F.name) formats
