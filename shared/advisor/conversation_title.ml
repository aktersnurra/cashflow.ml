let is_space value =
  let n = Uchar.to_int value in
  n <= 0x20 || n = 0x7f || n = 0x85 || n = 0xa0 || n = 0x1680
  || (n >= 0x2000 && n <= 0x200a) || n = 0x2028 || n = 0x2029
  || n = 0x202f || n = 0x205f || n = 0x3000

let of_first_message source =
  let output = Buffer.create 128 in
  let rec take offset count pending_space =
    if offset = String.length source then Buffer.contents output
    else
      let decoded = String.get_utf_8_uchar source offset in
      let value = Uchar.utf_decode_uchar decoded in
      let next = offset + Uchar.utf_decode_length decoded in
      if is_space value then take next count (count > 0)
      else
        let gap = if pending_space then 1 else 0 in
        if count + gap + 1 > 80 then Buffer.contents output ^ "…"
        else (
          if pending_space then Buffer.add_char output ' ';
          Buffer.add_utf_8_uchar output value;
          take next (count + gap + 1) false)
  in
  take 0 0 false
