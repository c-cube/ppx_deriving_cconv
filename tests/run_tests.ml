
open OUnit

module type S = sig
  type t [@@deriving ppx_deriving_cconv,show]
  val name : string
  val examples : t list
end

module Make(X : S) = struct
  let bij_json ex =
    let j = CConvYojson.encode X.encode ex in
    let ex' = CConvYojson.decode C.decode j in
    assert_equal ~printer:X.to_string ex' ex

  let bij_bencode ex =
    let j = CConvBencode.encode X.encode ex in
    let ex' = CConvBencode.decode C.decode j in
    assert_equal ~printer:X.to_string ex' ex
  
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

let suite =
  "cconv" >:::
    [ M1.suite; M2.suite ]

let _ =
  OUnit.run_test_tt_main suite

