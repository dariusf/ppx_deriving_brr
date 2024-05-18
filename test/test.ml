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

(* type 'a t =
      | Nil
      | Cons of 'a * 'a t
   [@@deriving brr]


   type 'a s1 =
      | S1 of int t
   [@@deriving brr]

   type 'a s =
      | S of 'a t
   [@@deriving brr] *)

(* type 'a t =
     | Nil
     | Cons of 'a * 'a t
   [@@deriving show]

   type v =
     | A of int t
     | B
   [@@deriving show] *)

let () =
  Brr.Console.log
    [
      jv_of_r { x = A ("hi", false); y = 2 };
      jv_of_r { x = B 1.; y = 3 };
      jv_of_r { x = C; y = 4 };
    ]
