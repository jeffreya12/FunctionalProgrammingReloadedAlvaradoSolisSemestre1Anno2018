(* 1 *)
fun only_capitals (xs)=
  (* List.filter recibe una función anónima, la cual consulta a Char.isUpper
     si el elemento que devuelve String.sub es mayúscula. String.sub retorna
     el valor en la posición 0 del String. List.filter aplica dicha función
     a todos los valores de la lista *)
  List.filter(fn x => Char.isUpper(String.sub(x, 0))) (xs)

(* 2 *)
fun longest_string1 (xs)=
  


val test1 = only_capitals ["A","B","C", "lower"] (* = ["A","B","C"] *)

(*

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test3 = longest_string2 ["A","bc","C"] = "bc"
val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4b = longest_string4 ["A","B","C"] = "C"
val test5 = longest_capitalized ["A","bc","C"] = "A"
4
val test6 = rev_string "abc" = "cba"
val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test8 = all_answers (fn x => if x <> 2 then SOME [x] else NONE) [2,3,4] = NONE
val test9a = count_wildcards Wildcard = 1
val pattern1 = TupleP([Variable "var", Wildcard, TupleP([Variable "var", Wildcard,
TupleP([Variable "var", Wildcard])])]);
val test9a_2 = count_wildcards pattern1 = 3;
val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val pattern2 = TupleP([Wildcard, Variable("abc")])
val test9b_2 = count_wild_and_variable_lengths pattern2 = 4
val pattern3 = TupleP([Variable("x"), Wildcard, Variable("x")])
val test9c = count_some_var ("x", pattern3) = 2
val test10 = check_pat (Variable("x")) = true
val pattern4 = TupleP([ConstP 12, Variable "var1", Variable "var2",
ConstructorP("constr1", Wildcard)]);
val test10_2 = check_pat pattern4;
val test11 = match (Const(1), UnitP) = NONE
val test11_2 = match(Unit, UnitP) = SOME [];
val pattern_t = Tuple([Const 12, Constructor("blah", Unit), Constructor("constr1",
Tuple([]))]);
val pattern_tp = TupleP([ConstP 12, Variable "var1", ConstructorP("constr1", Wildcard)]);
val test11_3 = match(pattern_t, pattern_tp) = SOME [("var1", Constructor("blah", Unit))];
val test12 = first_match Unit [UnitP] = SOME []

*)
