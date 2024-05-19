type v =
  | A of string * bool
  | B of float
  | C
[@@deriving brr]

type r = {
  x : v;
  y : int;
}
[@@deriving brr]

type 'a t =
  | Nil
  | Cons of 'a * 'a t
[@@deriving brr, show { with_path = false }]

type s1 = S1 of int t [@@deriving brr]
type 'a s = S of 'a t [@@deriving brr]

let () =
  let p x = Brr.Console.log [x] in
  p (jv_of_r { x = A ("hi", false); y = 2 });
  p (jv_of_r { x = B 1.; y = 3 });
  p (jv_of_r { x = C; y = 4 });
  let xs = Cons (1, Nil) in
  p (jv_of_t Jv.of_int xs);
  p (jv_of_s1 (S1 xs));
  p (jv_of_s Jv.of_int (S xs));
  let rt = jv_to_t Jv.to_int (jv_of_t Jv.of_int xs) in
  Format.printf "%a@." (pp Format.pp_print_int) rt
