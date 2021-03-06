(* Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

module type S = sig
  include Dict.S

  val v : ?fresh:bool -> ?readonly:bool -> ?capacity:int -> string -> t
end

module Make (Io_version : IO.VERSION) = struct
  include Dict.Make (Io_version) (IO.Unix)

  (* Add IO caching around Dict.v *)
  let IO.Cache.{ v } =
    let v_no_cache ~fresh ~readonly = v ~fresh ~readonly in
    IO.Cache.memoize ~clear ~valid
      ~v:(fun capacity -> v_no_cache ~capacity)
      Layout.dict

  let v ?fresh ?readonly ?(capacity = 100_000) root =
    v capacity ?fresh ?readonly root
end
