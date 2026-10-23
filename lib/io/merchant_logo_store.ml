module Analytics = Cashflow_core.Analytics
module Transfer = Cashflow_core.Transfer

type source = User | Llm | None_selected
type status = Resolved | Unknown | Failed
type state = { source : source; status : status; website : string option; logo_id : int option;
               confidence : int option; resolved_at : Ptime.t }
type blob = { content_type : string; bytes : string }

let source = Codec.enum "logo source" ["user", User; "llm", Llm; "none", None_selected]
let status = Codec.enum "logo status" ["resolved", Resolved; "unknown", Unknown; "failed", Failed]
let fields = Sql.(row (source @-> status @-> nullable text @-> nullable int @-> nullable int @-> Codec.time @-> nil)
  (fun source status website logo_id confidence resolved_at -> {source; status; website; logo_id; confidence; resolved_at}))
let get_stmt = Sql.(stmt "SELECT source, status, website, logo_id, confidence, resolved_at FROM merchant_logos WHERE owner_kind=? AND owner_id=? AND merchant_key=?"
  (text @-> text @-> text @-> nil) (opt fields))
let get db owner key = Sql.run db get_stmt (Codec.owner_kind owner) (Codec.owner_id owner) key
let put_stmt = Sql.(stmt "INSERT INTO merchant_logos (owner_kind, owner_id, merchant_key, source, status, website, logo_id, confidence, resolved_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT(owner_kind, owner_id, merchant_key) DO UPDATE SET source=excluded.source, status=excluded.status, website=excluded.website, logo_id=excluded.logo_id, confidence=excluded.confidence, resolved_at=excluded.resolved_at"
 (text @-> text @-> text @-> source @-> status @-> nullable text @-> nullable int @-> nullable int @-> Codec.time @-> nil) exec)
let put db owner key s = Sql.run db put_stmt (Codec.owner_kind owner) (Codec.owner_id owner) key s.source s.status s.website s.logo_id s.confidence s.resolved_at
let delete_stmt = Sql.(stmt "DELETE FROM merchant_logos WHERE owner_kind=? AND owner_id=? AND merchant_key=?" (text @-> text @-> text @-> nil) exec)
let delete db owner key = Sql.run db delete_stmt (Codec.owner_kind owner) (Codec.owner_id owner) key
let insert_blob = Sql.(stmt "INSERT INTO logos (sha256, content_type, width, height, bytes, source_url, fetched_at) VALUES (?, ?, ?, ?, ?, ?, ?) ON CONFLICT(sha256) DO NOTHING"
  (text @-> text @-> int @-> int @-> blob @-> text @-> Codec.time @-> nil) exec)
let blob_id = Sql.(stmt "SELECT id FROM logos WHERE sha256=?" (text @-> nil) (one (row (int @-> nil) Fun.id)))
let save_blob db now image =
 let bytes = Logo_fetch.bytes image in
 let digest = Digestif.SHA256.(to_hex (digest_string bytes)) in
 Sql.run db insert_blob digest (Logo_fetch.content_type image) (Logo_fetch.width image) (Logo_fetch.height image) bytes (Logo_fetch.source_url image) now;
 Sql.run db blob_id digest
let read_blob = Sql.(stmt "SELECT l.content_type, l.bytes FROM logos l JOIN merchant_logos m ON m.logo_id=l.id WHERE l.id=? AND m.owner_kind=? AND m.owner_id=? LIMIT 1"
 (int @-> text @-> text @-> nil) (opt (row (text @-> blob @-> nil) (fun content_type bytes -> {content_type; bytes}))))
let blob_for_owner db owner id = Sql.run db read_blob id (Codec.owner_kind owner) (Codec.owner_id owner)

let owner_transfer_ids = Sql.(stmt
  "SELECT t.id FROM transfers t WHERE EXISTS (SELECT 1 FROM accounts a WHERE a.id IN (t.debit_account_id, t.credit_account_id) AND a.owner_kind=? AND a.owner_id=?) ORDER BY t.seq DESC"
  (text @-> text @-> nil) (many (row (Codec.transfer_id @-> nil) Fun.id)))

let display_name db owner merchant_key =
  let transfers = (Store_sqlite.view db).transfer in
  Sql.run db owner_transfer_ids (Codec.owner_kind owner) (Codec.owner_id owner)
  |> List.find_map (fun id -> match transfers id with
     | Some (transfer : Transfer.t) when Analytics.merchant_key transfer = merchant_key ->
         Some (match transfer.memo.counterparty with
           | Some name when String.trim name <> "" -> name
           | _ -> transfer.memo.description)
     | _ -> None)
