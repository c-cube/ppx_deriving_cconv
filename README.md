# ppx_deriving_cconv

A [ppx_deriving](https://github.com/whitequark/ppx_deriving) plugin
for [CConv](https://github.com/c-cube/cconv). The point is to obtain
many serializers/deserializers in one stroke.

## Usage

After installation, on `OCaml >= 4.02.1`, use the library `ppx_deriving_cconv`
(depends on `ppx_deriving` of course). The following attributes can be used
on type declarations:

- `[@@deriving cconv]` derives an encoder and a decoder for the type
- `[@@deriving encode]` derives only an encoder (print values)
- `[@@deriving decode]` derives only a decoder (parse values)

Example:

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

### Options

Attributes can modify the behavior of `ppx_deriving_cconv`. They are as follows:

- `[@encoder e]` to specify an encoder for a record field of variant argument
- `[@decoder d]` to specify a decoder for a record field or variant argument
- `[@cconv.ignore]` to ignore the field for encoding (decoding will still require it)

## Install

For now:

    $ opam pin add ppx_deriving_cconv -k git \
        https://github.com/c-cube/ppx_deriving_cconv.git

For releases, the official opam repository will be used.
