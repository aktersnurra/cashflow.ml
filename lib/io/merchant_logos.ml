module Access = Cashflow_core.Access
module Household = Cashflow_core.Household
module Household_id = Cashflow_core.Household_id
module Owner = Cashflow_core.Owner

type view = { key : string; logo : string option; can_retry : bool }
type error = [ `Forbidden of string | `Invalid of string | `Fetch of Logo_fetch.error ]
type clear_mode = None_selected | Automatic
let error_code = function `Forbidden _ -> "forbidden" | `Invalid _ -> "invalid_merchant_key" | `Fetch e -> Logo_fetch.error_code e
let error_message = function `Forbidden e | `Invalid e -> e | `Fetch e -> Logo_fetch.error_message e
let ( let* ) = Result.bind
let check access owner key =
  if not (Access.can_use access owner) then Error (`Forbidden "you can't use that owner's merchants")
  else if String.trim key = "" then Error (`Invalid "merchant key must not be empty") else Ok ()
let project service key state =
  let logo = Option.bind state (fun (s : Merchant_logo_store.state) ->
    Option.map (fun id -> "/api/logos/" ^ string_of_int id) s.logo_id) in
  let can_retry = Option.is_some service.Service.assistant && Option.is_some service.logo_network &&
    (match state with
     | Some {Merchant_logo_store.logo_id=None; source=(Llm | None_selected); status=(Unknown | Failed); _} -> true
     | _ -> false) in
  {key; logo; can_retry}
let current_access service access owner =
  Access.can_use access owner &&
  (match owner with
   | Owner.User _ -> true
   | Owner.Household id ->
       Option.exists (fun (h : Household.t) -> Household_id.equal h.id id)
         (Directory.active_household service.Service.db (Access.user access).id))
let view service access ~owner ~merchant_key =
  if current_access service access owner then project service merchant_key (Merchant_logo_store.get service.Service.db owner merchant_key)
  else {key=merchant_key; logo=None; can_retry=false}
let blob service access ~owner ~id =
  if current_access service access owner then Merchant_logo_store.blob_for_owner service.Service.db owner id else None
let store_image service owner key ~source ~website ~confidence image =
  let now = service.Service.now () in
  Sql.transaction service.db (fun () ->
    let logo_id = Merchant_logo_store.save_blob service.db now image in
    Merchant_logo_store.put service.db owner key
      {source; status=Resolved; website; logo_id=Some logo_id; confidence; resolved_at=now};
    project service key (Merchant_logo_store.get service.db owner key))
let set service access ~owner ~merchant_key ~link =
  let* () = check access owner merchant_key in
  let* () = if current_access service access owner then Ok () else Error (`Forbidden "membership ended") in
  let* image = match service.Service.logo_network with
    | None -> Error (`Fetch Logo_fetch.Unreachable_site)
    | Some network -> Result.map_error (fun e -> `Fetch e) (Logo_fetch.fetch network link) in
  Sql.transaction service.db (fun () ->
    if current_access service access owner then
      Ok (store_image service owner merchant_key ~source:Merchant_logo_store.User ~website:(Some link) ~confidence:None image)
    else Error (`Forbidden "membership ended"))

let retry_due now (s : Merchant_logo_store.state) =
  match s.source, s.status with
  | Llm, Failed -> (match Ptime.diff now s.resolved_at |> Ptime.Span.to_float_s with
      | seconds -> seconds >= 30. *. 86400.)
  | _ -> false
let eligible now = function None -> true | Some s -> retry_due now s

let proposals reply =
  match Llm.json_of_reply reply with
  | Some (`Assoc fields) -> (match List.assoc_opt "merchants" fields with
      | Some (`List entries) -> Some entries | _ -> None)
  | _ -> None
let proposal name entries =
  List.find_map (function
    | `Assoc fields -> (match List.assoc_opt "name" fields, List.assoc_opt "domain" fields, List.assoc_opt "confidence" fields with
        | Some (`String n), Some (`String domain), Some (`Int confidence)
          when n = name && confidence >= 70 && confidence <= 100 && Logo_fetch.hostname domain -> Some (domain, confidence)
        | _ -> None)
    | _ -> None) entries

let resolve service access ~owner names =
  if current_access service access owner then match service.Service.assistant, service.logo_network with
  | Some llm, Some network ->
    let seen = Hashtbl.create 64 in
    let names = List.filter (fun (key, name) ->
      if String.trim key = "" || String.trim name = "" || Hashtbl.mem seen key then false
      else (Hashtbl.add seen key (); true)) names in
    let pending = List.filter_map (fun (key, name) ->
      let state = Merchant_logo_store.get service.db owner key in
      if eligible (service.now ()) state then Some (key, name, state) else None) names in
    let rec batches = function
      | [] -> ()
      | pending ->
        let rec split n acc rest = match n, rest with
          | 0, _ | _, [] -> List.rev acc, rest
          | _, x :: xs -> split (n-1) (x :: acc) xs in
        let batch, rest = split 60 [] pending in
        let user = Yojson.Safe.to_string (`Assoc ["merchants", `List (List.map
          (fun (_, name, _) -> `String name) batch)]) in
        let system = "Suggest a bare public website hostname for each merchant name. Reply JSON only: {\"merchants\":[{\"name\":\"name\",\"domain\":\"domain.tld\",\"confidence\":70}]}." in
        let reply = llm.Llm.complete ~system ~user () in
        Service.log_llm_call service access ~purpose:"logos" ~items:(List.length batch) ~ok:(Result.is_ok reply);
        let entries = Option.bind (Result.to_option reply) proposals in
        List.iter (fun (key, name, prior) ->
            let candidate = Option.bind entries (proposal name) in
            let fetched = match candidate with
              | None -> None
              | Some (domain, confidence) -> Some (domain, confidence, Logo_fetch.fetch network domain) in
            Sql.transaction service.db (fun () ->
              if Merchant_logo_store.get service.db owner key = prior && current_access service access owner then (
                let now = service.now () in
                let status, website, confidence, logo_id = match entries, fetched with
                  | None, _ -> Merchant_logo_store.Failed, None, None, None
                  | Some _, None -> Merchant_logo_store.Unknown, None, None, None
                  | Some _, Some (domain, confidence, Error _) -> Failed, Some domain, Some confidence, None
                  | Some _, Some (domain, confidence, Ok image) -> Resolved, Some domain, Some confidence,
                      Some (Merchant_logo_store.save_blob service.db now image) in
                Merchant_logo_store.put service.db owner key
                  {source=Llm; status; website; confidence; logo_id; resolved_at=now})) ) batch;
        batches rest
    in batches pending
  | _ -> ()

let clear service access ~owner ~merchant_key ~mode =
  let* () = check access owner merchant_key in
  let* () = if current_access service access owner then Ok () else Error (`Forbidden "membership ended") in
  let display_name = match mode with
    | None_selected -> None
    | Automatic -> Merchant_logo_store.display_name service.db owner merchant_key in
  let* () = Sql.transaction service.db (fun () ->
    if not (current_access service access owner) then Error (`Forbidden "membership ended")
    else (
      (match mode with
       | None_selected -> Merchant_logo_store.put service.db owner merchant_key
           {source=None_selected; status=Unknown; website=None; logo_id=None; confidence=None; resolved_at=service.now ()}
       | Automatic -> Merchant_logo_store.delete service.db owner merchant_key);
      Ok ())) in
  (match mode, display_name with
   | Automatic, Some name -> resolve service access ~owner [merchant_key, name]
   | _ -> ());
  Ok (view service access ~owner ~merchant_key)
