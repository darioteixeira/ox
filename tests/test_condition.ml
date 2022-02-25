module Environment = Oxen.Environment
module Sensors = Oxen.Sensors
module Binary = Sensors.Binary
module Ternary = Sensors.Ternary

module Sensors_def1 = struct
  type sensors = Binary.t array * unit

  let sensors = Sensors.[ binary ]
end

module Sensors_def2 = struct
  type sensors = Binary.t array * (Ternary.t array * unit)

  let sensors = Sensors.[ binary; ternary ]
end

module C1 = Oxen__Condition.Make (Sensors_def1)

module C2 = Oxen__Condition.Make (Sensors_def2)

let testable_c1 = Alcotest.testable (fun fmt c1 -> Format.pp_print_string fmt @@ C1.to_string c1) C1.equal

let testable_c2 = Alcotest.testable (fun fmt c2 -> Format.pp_print_string fmt @@ C2.to_string c2) C2.equal

let test_serialisation () =
  let intra_group_separator = ',' in
  let inter_group_separator = '/' in
  let s0 = "01#" in
  Alcotest.(check string) "case0" s0 C1.(s0 |> of_string |> to_string);
  let s1 = "0,1,#" in
  Alcotest.(check string) "case1" s1 C1.(s1 |> of_string ~intra_group_separator |> to_string ~intra_group_separator);
  let s2 = "01#" in
  Alcotest.(check string) "case2" s2 C1.(s2 |> of_string ~inter_group_separator |> to_string ~inter_group_separator);
  let s3 = "0,1,#" in
  Alcotest.(check string) "case3" s3 C1.(s3 |> of_string ~intra_group_separator ~inter_group_separator |> to_string ~intra_group_separator ~inter_group_separator);
  let s4 = "01#;01_#" in
  Alcotest.(check string) "case4" s4 C2.(s4 |> of_string |> to_string);
  let s5 = "0,1,#;0,1,_,#" in
  Alcotest.(check string) "case5" s5 C2.(s5 |> of_string ~intra_group_separator |> to_string ~intra_group_separator);
  let s6 = "01#/01_#" in
  Alcotest.(check string) "case6" s6 C2.(s6 |> of_string ~inter_group_separator |> to_string ~inter_group_separator);
  let s7 = "0,1,#/0,1,_,#" in
  Alcotest.(check string) "case7" s7 C2.(s7 |> of_string ~intra_group_separator ~inter_group_separator |> to_string ~intra_group_separator ~inter_group_separator)

let test_genericity () =
  Alcotest.(check int) "case0" 0 C1.("010" |> of_string |> genericity);
  Alcotest.(check int) "case1" 1 C1.("01#" |> of_string |> genericity);
  Alcotest.(check int) "case2" 3 C1.("###" |> of_string |> genericity);
  Alcotest.(check int) "case3" 0 C2.("010;01_01" |> of_string |> genericity);
  Alcotest.(check int) "case4" 3 C2.("01#;01_##" |> of_string |> genericity);
  Alcotest.(check int) "case5" 8 C2.("###;#####" |> of_string |> genericity)

let test_matches () =
  let env1 = Environment.[ [| false; true |] ] in
  Alcotest.(check bool) "case0" true C1.(matches (of_string "01") env1);
  Alcotest.(check bool) "case1" true C1.(matches (of_string "0#") env1);
  Alcotest.(check bool) "case2" false C1.(matches (of_string "10") env1);
  Alcotest.(check bool) "case3" false C1.(matches (of_string "1#") env1);
  let env2 = Environment.[ [| false; true |]; Ternary.[| False; True; Unknown |] ] in
  Alcotest.(check bool) "case4" true C2.(matches (of_string "01;01_") env2);
  Alcotest.(check bool) "case5" true C2.(matches (of_string "0#;01#") env2);
  Alcotest.(check bool) "case6" false C2.(matches (of_string "10;01_") env2);
  Alcotest.(check bool) "case7" false C2.(matches (of_string "1#;10#") env2)

let test_is_more_general () =
  Alcotest.(check bool) "case0" true C1.(is_more_general ~than:(of_string "0101") (of_string "010#"));
  Alcotest.(check bool) "case1" true C1.(is_more_general ~than:(of_string "01#1") (of_string "#10#"));
  Alcotest.(check bool) "case2" false C1.(is_more_general ~than:(of_string "0101") (of_string "0101"));
  Alcotest.(check bool) "case3" false C1.(is_more_general ~than:(of_string "010#") (of_string "010#"));
  Alcotest.(check bool) "case4" false C1.(is_more_general ~than:(of_string "####") (of_string "####"));
  Alcotest.(check bool) "case5" true C2.(is_more_general ~than:(of_string "0101;01_") (of_string "010#;01_"));
  Alcotest.(check bool) "case6" true C2.(is_more_general ~than:(of_string "01#1;01_") (of_string "#101;#1_"));
  Alcotest.(check bool) "case7" false C2.(is_more_general ~than:(of_string "0101;01_") (of_string "0101;01_"));
  Alcotest.(check bool) "case8" false C2.(is_more_general ~than:(of_string "010#;01#") (of_string "010#;#1_"));
  Alcotest.(check bool) "case9" false C2.(is_more_general ~than:(of_string "####;###") (of_string "####;###"))

let test_make_from_environment () =
  let env1 = Environment.[ [| false; true |] ] in
  Alcotest.check testable_c1 "case0" C1.(of_string "01") C1.(make_from_environment ~wildcard_probability:0. env1);
  Alcotest.check testable_c1 "case1" C1.(of_string "##") C1.(make_from_environment ~wildcard_probability:1. env1);
  let env2 = Environment.[ [| false; true |]; Ternary.[| False; True; Unknown |] ] in
  Alcotest.check testable_c2 "case2" C2.(of_string "01;01_") C2.(make_from_environment ~wildcard_probability:0. env2);
  Alcotest.check testable_c2 "case3" C2.(of_string "##;###") C2.(make_from_environment ~wildcard_probability:1. env2)

let test_clone_with_mutation () =
  Alcotest.check testable_c1 "case0" C1.(of_string "01#") C1.("01#" |> of_string |> clone_with_mutation ~mutation_probability:0. ~wildcard_probability:0.);
  Alcotest.check testable_c1 "case1" C1.(of_string "10") C1.("01" |> of_string |> clone_with_mutation ~mutation_probability:1. ~wildcard_probability:0.);
  Alcotest.check testable_c1 "case2" C1.(of_string "01#") C1.("01#" |> of_string |> clone_with_mutation ~mutation_probability:0. ~wildcard_probability:1.);
  Alcotest.check testable_c1 "case3" C1.(of_string "##") C1.("01" |> of_string |> clone_with_mutation ~mutation_probability:1. ~wildcard_probability:1.)

let test_cases = [
  Alcotest.test_case "to_string / of_string" `Quick test_serialisation;
  Alcotest.test_case "genericity" `Quick test_genericity;
  Alcotest.test_case "matches" `Quick test_matches;
  Alcotest.test_case "is_more_general" `Quick test_is_more_general;
  Alcotest.test_case "make_from_environment" `Quick test_make_from_environment;
  Alcotest.test_case "clone_with_mutation" `Quick test_clone_with_mutation;
]
