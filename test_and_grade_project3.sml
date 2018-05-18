use "your_file_name.sml";


val test1 = if only_capitals ["A","B","C"] = ["A","B","C"] then 1 else 0;
val test2 = if longest_string1 ["A","bc","C"] = "bc" then 1 else 0;
val test3 = if longest_string2 ["A","","C"] = "C" then 1 else 0;
val test4 = if longest_string3 ["A","bc","C"] = "bc" then 1 else 0;
val test5 = if longest_string3 ["A","","C"] = "A" then 1 else 0;
val test6 = if longest_string4 ["A","bc","C"] = "bc" then 1 else 0;
val test7 = if longest_string4 ["A","B","C"] = "C" then 1 else 0;
val test8 = if longest_capitalized ["A","bc","C"] = "A" then 1 else 0;
val test9 = if rev_string "abc" = "cba" then 1 else 0;
val test10 = if first_answer (fn x => if x = "a" then SOME x else NONE) ["v", "A", "a", "cs"] = "a" then 1 else 0;
val test11 = if all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE then 1 else 0;
val test12 = if count_wildcards (TupleP [Variable("a"), Variable("b")]) = 0 then 1 else 0;
val test13 = if count_wild_and_variable_lengths (TupleP [Variable("abc"), Wildcard]) = 4 then 1 else 0;
val test14 = if count_some_var ("x", TupleP [Variable("x"), Variable("y"), Variable("x")]) = 2 then 1 else 0;
val test15 = if check_pat (TupleP [Variable("x"), Variable("y"), Variable("x")]) = false then 1 else 0;
val test16 = if check_pat (ConstructorP ("hi",TupleP[Variable "x",ConstructorP ("yo",TupleP[Variable "x",UnitP])])) = false then 1 else 0;
val test17 = if match (Const(1), UnitP) = NONE then 1 else 0;
val test18 = if match (Tuple [Unit, Const 3, Const 2], TupleP [Variable "1", ConstP 3, Wildcard]) = SOME [("1",Unit)] then 1 else 0;
val test19 = if first_match (Const 3) [UnitP, Variable "f"] = SOME [("f", Const 3)] then 1 else 0;
val test20 = if first_match (Const 3) [UnitP, TupleP[]] = NONE then 1 else 0;


val obtained = test1 + test2 + test3 + test4 + test5 + test6 + test7 + test8 + test9 + test10 + test11 + test12 + test13 + test14 + test15 + test16 + test17 + test18 + test19 + test20;
val final_grade = (obtained * 100) div 20;
