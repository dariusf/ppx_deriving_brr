
# ppx_deriving_brr

1. Derives [conversion functions](https://erratique.ch/software/brr/doc/ffi_cookbook.html#iface_class) to and from Brr's [Jv](https://erratique.ch/software/brr/doc/Jv/index.html) and user-defined OCaml types
2. Derives an FFI module from a [ctypes-like](https://github.com/dbuenzli/brr/issues/16) specification which uses the above conversions to [expose OCaml functions to JS](https://erratique.ch/software/brr/doc/ffi_cookbook.html#export)

Use case: writing OCaml libraries which can be called from JS, where values created on both the JS and OCaml sides are passed back and forth.

The goal is to make FFI using js_of_ocaml/Brr as seamless as with Melange.

## Usage

```ocaml
type v =
  | A of string * bool
  | B of float
  | C
[@@deriving brr]
```

This generates the following (partial) functions, which are suitable for implementing [Jv.CONV](https://erratique.ch/software/brr/doc/Jv/module-type-CONV/index.html).

```ocaml
val v_of_jv : Jv.t -> v
val v_to_jv : v -> Jv.t
```

Recursive and polymorphic types work.

```ocaml
type 'a t =
  | Nil
  | Cons of 'a * 'a t
[@@deriving brr]
```

If the type is named `t`, the conversion functions are not prefixed.

```ocaml
val to_jv : ('a -> Jv.t) -> 'a t -> Jv.t
val of_jv : (Jv.t -> 'a) -> Jv.t -> 'a t
```

Say we have some OCaml functions operating on these datatypes.

```ocaml
let subst v s1 =
  match v with
  | A (s, b) when s = s1 -> A (s1, b)
  | A _ | C | B _ -> v

let rec append a b =
  match a with
  | Nil -> b
  | Cons (x, xs) -> Cons (x, append xs b)
```

We can generate an FFI for them as follows.


```ocaml
module%brr Example = struct
  let subst : v -> string -> v = subst

  (* Polymorphic functions must (currently) be monomorphized. *)
  let concat : int t -> int t -> int t = append
end
```

This produces the following code, using the earlier-generated conversions to expose the OCaml module as a JS object.

```ocaml
let () =
  Jv.set Jv.global "Example"
    (Jv.obj
       [|("subst",
           (Jv.callback ~arity:2
              (fun v0 ->
                fun v1 ->
                  v_to_jv (subst (v_of_jv v0)
                    (Jv.to_string v1)))));
         ("concat",
           (Jv.callback ~arity:2
              (fun v0 ->
                fun v1 ->
                   to_jv Jv.of_int
                     (append (of_jv Jv.to_int v0)
                       (of_jv Jv.to_int v1)))))|])
```

The module can then be used from JS.

```
$ node
Welcome to Node.js v16.20.2.
Type ".help" for more information.
> require('./_build/default/example/example.bc.js')
...
> Example.concat(['Cons', 1, ['Nil']], ['Cons', 2, ['Nil']])
[ 'Cons', 1, [ 'Cons', 2, [ 'Nil' ] ] ]
```

## Conversion Scheme

Uses [Jv's conversion functions](https://erratique.ch/software/brr/doc/Jv/index.html) if possible.
For types not covered, mostly follows what [Yojson](https://ocaml-community.github.io/yojson/yojson/Yojson/Safe/index.html) does, with some refinements.

Not all are implemented yet.

| OCaml      | JS                            |
| ---------- | ----------------------------- |
| option     | value or null                 |
| list       | array                         |
| record     | object                        |
| tuple      | array                         |
| variant    | tagged array/string constants |
| String map | object                        |
| Int map    | Map                           |
| Set        | Set                           |
