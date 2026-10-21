open Base

let key ~description ~counterparty =
  let text = match counterparty with Some c when not (String.is_empty (Stdlib.String.trim c)) -> c | _ -> description in
  String.lowercase text |> String.map ~f:(fun c -> if Char.(c >= '0' && c <= '9') then ' ' else c)
  |> String.split ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s)) |> String.concat ~sep:" "
