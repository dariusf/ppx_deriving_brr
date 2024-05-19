type v =
  | A of string * bool
  | B of float
  | C
[@@deriving brr, show { with_path = false }]

type r = {
  x : v;
  y : int;
}
[@@deriving brr, show { with_path = false }]

type 'a t =
  | Nil
  | Cons of 'a * 'a t
[@@deriving brr, show { with_path = false }]

type s1 = S1 of int t [@@deriving brr]
type 'a s = S of 'a t [@@deriving brr]

let () =
  let p x = Brr.Console.log [x] in
  let data = { x = A ("hi", false); y = 2 } in
  p (r_to_jv data);
  p (r_to_jv { x = B 1.; y = 3 });
  p (r_to_jv { x = C; y = 4 });
  let xs = Cons (1, Nil) in
  p (t_to_jv Jv.of_int xs);
  p (s1_to_jv (S1 xs));
  p (s_to_jv Jv.of_int (S xs));
  let rt = t_of_jv Jv.to_int (t_to_jv Jv.of_int xs) in
  Format.printf "%a@." (pp Format.pp_print_int) rt;
  let rt1 = r_of_jv (r_to_jv data) in
  Format.printf "%a@." pp_r rt1
