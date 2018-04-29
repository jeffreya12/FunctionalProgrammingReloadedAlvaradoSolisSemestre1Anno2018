exception NoAnswer

datatype pattern = Wildcard
                   | Variable of string
                   | UnitP
                   | ConstP of int
                   | TupleP of pattern list
                   | ConstructorP of string * pattern

datatype valu = Const of int
                | Unit
                | Tuple of valu list
                | Constructor of string * valu

fun g f1 f2 p =
  let
    val r = g f1 f2
  in
    case p of
      Wildcard              => f1 ()
      | Variable x          => f2 x
      | TupleP ps           => List.foldl (fn (p,i) => (r p) + i) 0 ps
      | ConstructorP (_,p)  => r p
      | _                   => 0
  end

(* 1 *)
fun only_capitals (xs)=
  (* List.filter recibe una función anónima, la cual consulta a Char.isUpper
     si el elemento que devuelve String.sub es mayúscula. String.sub retorna
     el valor en la posición 0 del String. List.filter aplica dicha función
     a todos los valores de la lista *)
  List.filter(fn x => Char.isUpper(String.sub(x, 0))) (xs)

(* 2 *)
fun longest_string1 (xs)=
  (* foldl aplica una función anónima a toda la lista, desde el último elementos
     hasta el primero. La función pregunta si el tamaño del elemento es mayor o
     igual al tamaño del acumulador y devuelve el elemento de mayor tamaño.
     Si hay empate, acumula el último que encontró*)
  foldl(fn (acc, x) => if String.size(acc) <= String.size(x) then x else acc ) "" xs

(* 3 *)
fun longest_string2 (xs)=
  (* foldl aplica una función anónima a toda la lista, desde el último elementos
     hasta el primero. La función pregunta si el tamaño del elemento es mayor
     al tamaño del acumulador y devuelve el elemento de mayor tamaño.
     Si hay empate, acumula el primero que encontró *)
  foldl(fn (acc, x) => if String.size(acc) < String.size(x) then x else acc ) "" xs

(* 4 *)
fun longest_string_helper (f)=
(* foldl aplica una función anónima a toda la lista, desde el último elementos
   hasta el primero. Dependiendo del resultado de la función parámetro f,
   retorna el valor o el acumulador *)
  foldl(fn (acc, x) => if f(String.size(acc), String.size(x)) then x else acc) ""

(* Valor que llama a la función longest_string_helper con una función anónima
   que devuelve la expresión que compara el tamaño del acumulador con el
   elemento actual de la lista. Devuelve true si es mayor o igual, acumulando
   así el último elemento de la lista que encuentra *)
val longest_string3 = longest_string_helper (fn (x, y) => x <= y)

(* Valor que llama a la función longest_string_helper con una función anónima
   que devuelve la expresión que compara el tamaño del acumulador con el
   elemento actual de la lista. Devuelve true si es mayor, acumulando
   así el primer elemento de la lista que encuentra *)
val longest_string4 = longest_string_helper (fn (x, y) => x < y)

(* 5 *)
(* Valor que llama a las funciones longest_string1 y only_capitals. Primero
   only_capitals retorna una lista con todos los elementos que inicien con
   mayúscula y luego longest_string1 retorna el más largo de éstos *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
(* Primero String.explode convierte el String en una lista de Char. luego
   rev invierte el orden de dicha lista y luego String.implode genera un
   String con los valores de la lista invertida *)
val rev_string = String.implode o rev o String.explode

(* 7 *)
fun first_answer f xs=
  case xs of
    (* Si llega al final de la lista, entonces levanta la excepción NoAnswer *)
    [] => raise NoAnswer
    | hd::tl => case f(hd) of
                  (* Si hay algo en la lista, aplica la función a su primer
                     elemento *)
                  NONE => first_answer f tl (* Si retorna NONE, busca el siguiente *)
                  | SOME v => v (* Si retorna SOME v, el valor de la función es v *)

(* 8 *)
fun all_answers f xs=
  let
    (* Valor que define si existen o no valores NONE en la lista *)
    val none_exists = List.exists (fn x => f(x) = NONE)
    (* Resultado de concatenar la lista *)
    val result = List.foldr (fn (x, acc)  => (case f(x) of
                                              (* Solo se llama si no hay NONE,
                                                 por lo que sólo contiene un
                                                 caso *)
                                                SOME v => v@acc ))
  in
    case xs of
      (* Si la lista es vacía, retorna SOME [] *)
      [] => SOME []
      | _ => if none_exists xs
             (* Si hay NONE en la lista, retorna NONE *)
             then NONE
             (* Si no hay NONE en la lista, retorna SOME con la nueva lista
                creada. *)
             else SOME (result [] xs)
  end

(* 9 (a) *)
(* la función crea dos funciones anónimas: f1 retorna 1, ya que es la que
   se ejecuta cuando hay un Wildcard, y f2 que retorna 0, ya que es la que
   se ejecuta cuando NO hay un Wildcard *)
fun count_wildcards p = g (fn _ => 1) (fn _ => 0) p

(* 9 (b) *)
(* la función crea dos funciones anónimas: f1 retorna 1, ya que es la que
   se ejecuta cuando hay un Wildcard, y f2 que retorna el tamaño del string,
   ya que es la que se ejecuta cuando hay un Variable *)
fun count_wild_and_variable_lengths p = g (fn _ => 1) (fn x => String.size x) p

(* 9 (c) *)
(* la función crea dos funciones anónimas: f1 retorna 0, ya que es la que
   se ejecuta cuando hay un Wildcard, y f2 que retorna 1 en caso que haga macth
   con el String, 0 en caso contrario *)
fun count_some_var (text, p) = g (fn _ => 0) (fn x => if x = text then 1 else 0) p

(* 10 *)
fun check_pat p =
  let
    fun variable_content p =
      case p of
        (* Si el patrón es una variable, retorna una lista con el contenido *)
        Variable v => [v]
        (* Si es una tupla, llama recursivamente a la funcion con el elemento
           actual y lo concatena con el acumulador *)
        | TupleP tp => foldl (fn (x, y) => variable_content(x)@y) [] tp
        (* Cualquier otro caso, retorna una lista vacía *)
        | _ => []
    fun check_repeat xs =
      case xs of
        (* Si la lista está vacía, retorna true *)
        [] => true
        | hd::tl => if List.exists(fn x => x = hd ) tl
                    (* Si el primer elemento existe en el resto de la lista,
                       retorna false *)
                    then false
                    (* Si no existe, llama recursivamente a la funcion con la
                       cola de la lista *)
                    else check_repeat tl
  in
    check_repeat(variable_content p)
  end

(* 11 *)
fun match (v, p)=
  case (v, p) of
    (* Wildcard hace match con todo y retorna una lista vacía *)
    (_, Wildcard) => SOME []
    (* Variable hace match con todo y retorna una lista con una tupla
       String * Valu *)
    | (_, Variable s) => SOME [(s, v)]
    (* UnitP hace match con Unit y retorna una lista vacía *)
    | (Unit, UnitP) => SOME []
    (* ConstP hace match con Const *)
    | (Const c, ConstP cp) => if c = cp
                              (* Si son iguales, retorna una lista vacía *)
                              then SOME []
                              (* Si no son iguales, no hay match *)
                              else NONE
    (* TupleP hace match con Tuple *)
    | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                               (* Si son del mismo tamaño *)
                               then
                                  (* ListPair.zip => Combina ambas listas en pares
                                     match => recursivamente valida el match de
                                              cada par
                                     all_answers => contiene una lista con todos
                                                    los pares que hacen match *)
                                  case all_answers match (ListPair.zip(vs,ps)) of
                                    SOME tp => SOME tp
                                    (* Si el par no hace match, retorna NONE *)
                                    | _ => NONE
                               (* Si no son del mismo tamaño, retorna NONE *)
                               else NONE
    (* ConstructorP hace match con Constructor *)
    | (Constructor(s2, vc), ConstructorP(s1, pc)) => if s1 = s2
                                                     (* Si son del mismo tamaño,
                                                        deben hacer match con
                                                        sus valores*)
                                                     then match(vc, pc)
                                                     (* Si no son del mismo
                                                        tamaño, retorna NONE *)
                                                     else NONE
    (* Cualquier otro caso, no hay match *)
    | (_, _) => NONE

(* 12 *)
fun first_match v ps=
  (* Retorna el primer match de la lista *)
  SOME (first_answer(fn x => match(v, x)) ps)
  (* Si no hay un match, maneja la excepcion NoAnswer y devuelve NONE *)
  handle NoAnswer => NONE

val test1 = only_capitals ["A","B","C", "lower"] = ["A","B","C"]
val test2 = longest_string1 ["A","bc","C","ef"] = "bc"
val test3 = longest_string2 ["A","bc","C","ef"] = "ef"
val test4a = longest_string3 ["A","bc","C","ef", "ab"] = "bc"
val test4b = longest_string4 ["A","B","C","D","E"] = "E"
val test5 = longest_capitalized ["A","bc","C"] = "A"
val test6 = rev_string "abc" = "cba"
val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test8a = all_answers (fn x => if x > 2 then SOME [x] else NONE) [2,3,4] = NONE
val test8b = all_answers (fn x => if x >= 2 then SOME [x] else NONE) [2,3,4] = SOME [2,3,4]
val test9a = count_wildcards Wildcard = 1
val pattern1 = TupleP([Variable "var", Wildcard, TupleP([Variable "var", Wildcard, TupleP([Variable "var", Wildcard])])])
val test9a_2 = count_wildcards pattern1 = 3
val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val pattern2 = TupleP([Wildcard, Variable("abc")])
val test9b_2 = count_wild_and_variable_lengths pattern2 = 4
val pattern3 = TupleP([Variable("x"), Wildcard, Variable("x")])
val test9c = count_some_var ("x", pattern3) = 2
val test10 = check_pat (Variable("x")) = true
val pattern4 = TupleP([ConstP 12, Variable "var1", Variable "var2", ConstructorP("constr1", Wildcard)])
val test10_2 = check_pat pattern4
val test11 = match (Const(1), UnitP) = NONE
val test11_2 = match(Unit, UnitP) = SOME []
val pattern_t = Tuple([Const 12, Constructor("blah", Unit), Constructor("constr1", Tuple([]))])
val pattern_tp = TupleP([ConstP 12, Variable "var1", ConstructorP("constr1", Wildcard)])
val test11_3 = match(pattern_t, pattern_tp) = SOME [("var1", Constructor("blah", Unit))]
val test12 = first_match Unit [UnitP] = SOME []
val test12_2 = first_match Unit [ConstP 51] = NONE
