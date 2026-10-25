val of_first_message : string -> string
(** Collapse whitespace and keep at most 80 Unicode scalar values, followed by an
    ellipsis only when meaningful text remains. Returns valid UTF-8 even for
    malformed input. This is literal display text, never generated or HTML. *)
