
# ppx_deriving_brr

1. Derives [conversion functions](https://erratique.ch/software/brr/doc/ffi_cookbook.html#iface_class) to and from Brr's [Jv](https://erratique.ch/software/brr/doc/Jv/index.html) and user-defined OCaml types
2. Derives a [ctypes-like](https://github.com/dbuenzli/brr/issues/16) FFI module which uses these functions to [expose OCaml functions to JS](https://erratique.ch/software/brr/doc/ffi_cookbook.html#export)

The use case is writing OCaml libraries which can be called from JS, where values created on both the JS and OCaml sides are passed back and forth.

The goal is to make FFI using js_of_ocaml/Brr as seamless as with Melange.

## Usage

```ocaml
type v =
  | A of string * bool
  | B of float
  | C
[@@deriving brr]
```

This can be used as follows.

```js
let a = 1;
```

## Conversion Scheme

Uses [Jv's conversion functions](https://erratique.ch/software/brr/doc/Jv/index.html) if possible.
For types not covered, mostly follows what [Yojson](https://ocaml-community.github.io/yojson/yojson/Yojson/Safe/index.html) does, with some refinements.

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
