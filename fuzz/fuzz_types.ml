open Crowbar
module C = Crowbar
module T = Irmin.Type

type 'a ty =
  | Unit : unit ty
  | Bool : bool ty
  | Char : char ty
  | Int : int ty
  | Int32 : int32 ty
  | Int64 : int64 ty
  | Float : float ty
  | String : string ty
  | Bytes : bytes ty
  | List : 'a ty -> 'a list ty
  (* | Array : 'a ty -> 'a array ty *)
  | Option : 'a ty -> 'a option ty
  | Pair : 'a ty * 'b ty -> ('a * 'b) ty
  | Triple : 'a ty * 'b ty * 'c ty -> ('a * 'b * 'c) ty
  | Result : 'a ty * 'b ty -> ('a, 'b) result ty

type any = Any : 'a ty -> any

let ty_gen =
  fix (fun ty_gen ->
      choose
        [ const (Any Unit);
          const (Any Bool);
          const (Any Char);
          const (Any Int);
          const (Any Int32);
          const (Any Int64);
          const (Any Float);
          const (Any String);
          const (Any Bytes);
          map [ ty_gen ] (fun (Any t) -> Any (List t));
          map [ ty_gen ] (fun (Any t) -> Any (Option t));
          map [ ty_gen; ty_gen ] (fun (Any t1) (Any t2) -> Any (Pair (t1, t2)));
          map [ ty_gen; ty_gen; ty_gen ] (fun (Any t1) (Any t2) (Any t3) -> Any (Triple (t1, t2, t3)));
          map [ ty_gen; ty_gen ] (fun (Any t1) (Any t2) -> Any (Result (t1, t2)));
        ] )

let rec ty_to_irmin : type a. a ty -> a T.ty = function
  | Unit -> T.unit
  | Bool -> T.bool
  | Char -> T.char
  | Int -> T.int
  | Int32 -> T.int32
  | Int64 -> T.int64
  | Float -> T.float
  | String -> T.string
  | Bytes -> T.bytes
  | List ty -> T.list (ty_to_irmin ty)
  | Option ty -> T.option (ty_to_irmin ty)
  | Pair (ty1, ty2) -> T.pair (ty_to_irmin ty1) (ty_to_irmin ty2)
  | Triple (ty1, ty2, ty3) -> T.triple (ty_to_irmin ty1) (ty_to_irmin ty2) (ty_to_irmin ty3)
  | Result (ty1, ty2) -> T.result (ty_to_irmin ty1) (ty_to_irmin ty2)

let ty_to_irmin_gen t = C.const (ty_to_irmin t)

let triple a b c =
  map [a; b; c] (fun a b c -> (a, b, c))

let rec ty_to_value_gen : type a. a ty -> a gen = function
  | Unit -> const ()
  | Bool -> bool
  | Char -> char
  | Int -> int
  | Int32 -> int32
  | Int64 -> int64
  | Float -> float
  | String -> string
  | Bytes -> bytes
  | List ty -> list (ty_to_value_gen ty)
  | Option ty  -> option (ty_to_value_gen ty)
  | Pair (ty1, ty2) -> pair (ty_to_value_gen ty1) (ty_to_value_gen ty2)
  | Triple (ty1, ty2, ty3) -> triple (ty_to_value_gen ty1) (ty_to_value_gen ty2)(ty_to_value_gen ty3)
  | Result (ty1, ty2) -> result (ty_to_value_gen ty1) (ty_to_value_gen ty2)

type pair = Pair : 'a T.t * 'a -> pair

let pair_gen : pair gen =
  C.dynamic_bind ty_gen @@ fun (Any t) ->
  let itype_gen = ty_to_irmin_gen t in
  let val_gen = ty_to_value_gen t in
  map [ itype_gen; val_gen ] (fun t v -> Pair (t, v))

let check_pair (Pair (t, v)) =
  let encoded = T.to_bin_string t v in
  match T.of_bin_string t encoded with
  | Error _ -> fail "incorrect deserialization"
  | Ok v' -> C.check_eq v v'

let () = add_test ~name:"Irmin.Type" [ pair_gen ] check_pair
