type 'a fixed_width_fmt = 'a Fmt.t * int

(** Pretty-printer for byte counts *)
let pp_bytes : Int63.t fixed_width_fmt =
  (* Round down to the nearest 0.1 *)
  let trunc f = Float.trunc (f *. 10.) /. 10. in
  let pp ppf i =
    match Int63.to_float i with
    | n when n < 1024. -> Fmt.pf ppf "%6.1f B  " (trunc n)
    | n when n < 1024. ** 2. -> Fmt.pf ppf "%6.1f KiB" (trunc (n /. 1024.))
    | n when n < 1024. ** 3. ->
        Fmt.pf ppf "%6.1f MiB" (trunc (n /. (1024. ** 2.)))
    | n -> Fmt.pf ppf "%6.1f GiB" (trunc (n /. (1024. ** 3.)))
  in
  (pp, 10)

module Progress : sig
  type t

  val counter :
    total:Int63.t ->
    sampling_interval:int ->
    ?columns:int ->
    message:string ->
    ?pp_count:Int63.t fixed_width_fmt ->
    ?ppf:Format.formatter ->
    unit ->
    t * (Int63.t -> unit)
  (** Renders a progress bar of the form:

      [<msg> <count> MM:SS \[########..............................\] XX%]

      @param ppf Defaults to {!Format.err_formatter} *)

  val finalise : t -> unit

  val increment :
    ?ppf:Format.formatter ->
    unit ->
    t * ((unit -> unit) * (unit -> unit) * (unit -> unit))
end = struct
  type t = { ppf : Format.formatter; update : unit -> unit }

  let bar width percentage =
    let filled =
      Float.to_int (Float.of_int (width - 2) *. percentage /. 100.)
    in
    let not_filled = width - 2 - filled in
    "["
    ^ String.init filled (fun _ -> '#')
    ^ String.init not_filled (fun _ -> '.')
    ^ "]"

  let counter ~total ~sampling_interval ?(columns = 80) ~message
      ?pp_count:(pp_count, count_width = (Fmt.nop, 0))
      ?(ppf = Format.err_formatter) () =
    let count = ref Int63.zero in
    let percentage i =
      min (Float.trunc (Int63.to_float i *. 100. /. Int63.to_float total)) 100.
    in
    let start_time = Mtime_clock.counter () in
    let should_update : unit -> bool =
      let ticker = ref 0 in
      fun () ->
        ticker := (!ticker + 1) mod sampling_interval;
        !ticker = 0
    in
    let bar_width = columns - String.length message - count_width - 16 in
    if bar_width < 3 then invalid_arg "Not enough space for a progress bar";
    let update ~first =
      let seconds = Mtime_clock.count start_time |> Mtime.Span.to_s in
      let percentage = percentage !count in
      if first then Format.pp_open_box ppf 0 else Fmt.pf ppf "\r";
      Fmt.pf ppf "%s  %a  %02.0f:%02.0f  %s %3.0f%%%!" message pp_count !count
        (Float.div seconds 60.) (Float.rem seconds 60.)
        (bar bar_width percentage) percentage
    in
    let progress i =
      count := Int63.add !count i;
      if should_update () then update ~first:false
    in
    update ~first:true;
    ({ ppf; update = (fun () -> update ~first:false) }, progress)

  let increment ?(ppf = Format.err_formatter) () =
    let nb_commits = ref 0 in
    let nb_nodes = ref 0 in
    let nb_contents = ref 0 in
    let update ~first =
      if first then Format.pp_open_box ppf 0 else Fmt.pf ppf "\r";
      Fmt.pf ppf "\t%dk contents / %dk nodes / %dk commits%!"
        (!nb_contents / 1000) (!nb_nodes / 1000) (!nb_commits / 1000)
    in
    let progress count =
      incr count;
      if !count mod 1000 = 0 then update ~first:false
    in
    let commits () = progress nb_commits in
    let nodes () = progress nb_nodes in
    let contents () = progress nb_contents in
    update ~first:true;
    ( { ppf; update = (fun () -> update ~first:false) },
      (contents, nodes, commits) )

  let finalise { ppf; update } =
    update ();
    Fmt.pf ppf "@,@]%!"
end
