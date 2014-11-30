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
} [@@deriving cconv] ;;
type t = ...
val encode : t CConv.Encode.encoder = ...
val decode : t CConv.Decode.decoder = ...

type term =
    | Var of string
    | App of term * term
    | Lambda of string * term
[@@deriving cconv] ;;
type term = ...
val encode_term : term CConv.Encode.encoder = ...
val decode_term : term CConv.Decode.decoder = ...

(* encoders/decoders can be used with several backend *)
#require "cconv.yojson";;

CConvYojson.encode encode_term;;
- : term -> Yojson.Basic.json = <fun>

CConvYojson.decode decode_term;;
- : Yojson.Basic.json -> [`Ok of term | `Error of string] = <fun>

#require "cconv.bencode";;
CConvBencode.encode encode_term;;
- : term -> Bencode.t = <fun>

CConvBencode.decode decode_term;;
- : Bencode.t -> [`Ok of term | `Error of string] = <fun>

#require "cconv.sexp";;
CConvSexp.encode encode;;
- : t -> Sexplib.Sexp.t = <fun>

CConvSexp.decode decode;;
- : Sexplib.Sexp.t -> [`Ok of t | `Error of string] = <fun>

(* on the fly converter *)

let json = CConvYojson.encode [%encode: int list] [1;2;3];;
val json : CConvYojson.t = `List [`Int 1; `Int 2; `Int 3]

let l = CConvYojson.decode [%decode: float list] json;;
val l : float list CConvYojson.or_error = `Ok [1.; 2.; 3.]

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
