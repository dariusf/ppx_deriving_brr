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
type s2 = int list [@@deriving brr]
type 'a s3 = 'a list [@@deriving brr]

let () =
  let p x = Brr.Console.log [x] in
  let data = { x = A ("hi", false); y = 2 } in
  p (r_to_jv data);
  p (r_to_jv { x = B 1.; y = 3 });
  p (r_to_jv { x = C; y = 4 });
  let xs = Cons (1, Nil) in
  p (to_jv Jv.of_int xs);
  p (s1_to_jv (S1 xs));
  p (s_to_jv Jv.of_int (S xs));
  let rt = of_jv Jv.to_int (to_jv Jv.of_int xs) in
  Format.printf "%a@." (pp Format.pp_print_int) rt;
  let rt1 = r_of_jv (r_to_jv data) in
  Format.printf "%a@." pp_r rt1;
  p (s2_to_jv [1; 2; 3]);
  p (s3_to_jv Jv.of_int [1; 2; 3])

let rec append a b =
  match a with Nil -> b | Cons (x, xs) -> Cons (x, append xs b)

module%brr Example = struct
  let concat : int t -> int t -> int t = append
end

let eval s = Brr.Console.log [Jv.call Jv.global "eval" [| Jv.of_string s |]]
let () = eval {|Example.concat(['Cons', 1, ['Nil']], ['Cons', 2, ['Nil']])|}
