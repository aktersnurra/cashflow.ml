type t = { code : string; scale : int }

let make ~code ~scale =
  if String.length code <> 3 || not (String.for_all (fun c -> c >= 'A' && c <= 'Z') code) then
    Error (`Invalid_currency (Printf.sprintf "currency code %S must be three uppercase letters" code))
  else if scale < 0 || scale > 4 then
    Error (`Invalid_currency (Printf.sprintf "currency scale %d must be between 0 and 4" scale))
  else Ok { code; scale }

let code currency = currency.code
let scale currency = currency.scale
let equal a b = String.equal a.code b.code
