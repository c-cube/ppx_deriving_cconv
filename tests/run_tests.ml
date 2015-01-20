
open OUnit

module type S = sig
  type t [@@deriving cconv,show]
  val name : string
  val examples : t list
end

module type TEST = sig
  val suite : OUnit.test
end

module Make(X : S) : TEST = struct
  let bij_json ex () =
    let j = CConvYojson.encode X.encode ex in
    match CConvYojson.decode X.decode j with
    | `Ok ex' ->
      assert_equal ~printer:X.show ex' ex
    | `Error msg -> assert_failure msg

  let bij_bencode ex () =
    let j = CConvBencode.encode X.encode ex in
    match CConvBencode.decode X.decode j with
    | `Ok ex' ->
      assert_equal ~printer:X.show ex' ex
    | `Error msg -> assert_failure msg
  
  let suite_of_example ex =
    [ "bij_json" >:: bij_json ex
    ; "bij_bencode" >:: bij_bencode ex
    ]

  let suite =
    X.name >::: (List.map suite_of_example X.examples |> List.flatten)
end

module M1 = Make(struct
  type t = {
    x : int;
    y : int;
    color : string;
    prev : t option; (* previous position, say *)
  } [@@deriving show, cconv]

  let name = "point"
  let p = { x=1; y=2; color="red"; prev=None; }
  let p' = {x=1; y=3; color="yellow"; prev=Some p; }
  let examples = [p; p']
end)

module M2 = Make(struct
  type t =
    | Var of string
    | App of t * t
    | Lambda of string * t
  [@@deriving show, cconv]

  let name = "lambda-term"
  let t1 = Lambda ("x", App (Lambda ("y", App (Var "y", Var "x")), Var "x"))
  let examples = [t1]
end)

module M3 = Make(struct
  type t = {
    i : int [@encoder CConv.Encode.(map string_of_int string)];
    j : int [@decoder CConv.Decode.(map int_of_string string)];
  }
  [@@deriving show, cconv]

  let name = "record_@encoder"
  let t1 = { i=1; j=42 }
  let t2 = { i=10; j=0 }
  let t3 = { i=0; j=11 }
  let examples = [t1; t2; t3]
end)

type record_ignore = {
  x : int;
  y : int [@ignore];
} [@@deriving show, cconv]

let test_record_ignore () =
  let r = { x=1; y=2} in
  let json = CConvYojson.encode encode_record_ignore r in
  OUnit.assert_equal ~print:(Yojson.Basic.pretty_to_string ~std:true)
    `Assoc ["x", `Int 1] json;
  ()

let suite =
  "cconv" >:::
    [ M1.suite
    ; M2.suite
    ; M3.suite
    ; "record_ignore" >:: test_record_ignore
    ]

let _ =
  OUnit.run_test_tt_main suite

