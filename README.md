# ppx_deriving_cconv

A [ppx_deriving](https://github.com/whitequark/ppx_deriving) plugin
for [CConv](https://github.com/c-cube/cconv). The point is to obtain 
many serializers/deserializers in one stroke.

## Usage

```ocaml
#require "ppx_deriving_cconv";;

type t = {
    x : int;
    y : int;
    color : string;
    prev : t option; (* previous position, say *)
} [@@deriving show, cconv] ;;

type t =
    | Var of string
    | App of t * t
    | Lambda of string * t
[@@deriving show, cconv] ;;


(* on the fly converter *)
#require "cconv.yojson";;

let json = CConvYojson.encode [%encode: int list] [1;2;3];;

let l = CConvYojson.decode [%decode: float list] json;;

```

Soon: there should be annotations `[@encoder e]`, `[@decoder d]` and
`[@cconv.ignore_with v]`
to specify a decoder `d`, an encoder `e`, or ignore a field at
encoding to be filled with `v` at decoding.

## Install

For now:

    $ opam pin add ppx_deriving_cconv -k git \
        https://github.com/c-cube/ppx_deriving_cconv.git

For releases, the official opam repository will be used.
