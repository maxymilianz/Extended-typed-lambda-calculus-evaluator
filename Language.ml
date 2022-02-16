open Types


type expression =
    (* typed lambda calculus *)
    | Variable of string
    | Abstraction of ((string * type_type) * expression)
    | Application of (expression * expression)

    (* naturals *)
    | Natural of int
    | Add of (expression * expression)
    | Multiply of (expression * expression)
    | Subtract of (expression * expression)
    | Divide of (expression * expression)
    | Equal of (expression * expression)

    (* booleans *)
    | True
    | False
    | If of (expression * expression * expression)

    (* recursion *)
    | Fix of expression

    (* exceptions *)
    | Exception of (string * type_type * expression)  (* name * parameter type * target expression *)
    | Throw of (string * expression * type_type)  (* name * argument * target type *)
    | Try of (expression * (handler list))
    (* ^ throwing expression * (handler list) *)
and handler = string * string * expression
(* ^ (exception name * parameter name * target expression) *)


let rec expression_to_string = function
    | Variable variable_name ->
        variable_name
    | Abstraction ((parameter_name, _), body) ->
        parameter_name ^ " -> " ^ (expression_to_string body)
    | Application (abstraction, argument) ->
        "(" ^ (expression_to_string abstraction) ^ ") (" ^ (expression_to_string argument) ^ ")"

    | Natural number ->
        string_of_int number
    | Add (natural_0, natural_1) ->
        "(" ^ (expression_to_string natural_0) ^ ") + (" ^ (expression_to_string natural_1) ^ ")"
    | Multiply (natural_0, natural_1) ->
        "(" ^ (expression_to_string natural_0) ^ ") * (" ^ (expression_to_string natural_1) ^ ")"
    | Subtract (natural_0, natural_1) ->
        "(" ^ (expression_to_string natural_0) ^ ") - (" ^ (expression_to_string natural_1) ^ ")"
    | Divide (natural_0, natural_1) ->
        "(" ^ (expression_to_string natural_0) ^ ") / (" ^ (expression_to_string natural_1) ^ ")"
    | Equal (natural_0, natural_1) ->
        "(" ^ (expression_to_string natural_0) ^ ") = (" ^ (expression_to_string natural_1) ^ ")"

    | True ->
        "true"
    | False ->
        "false"
    | If (condition, then_branch, else_branch) ->
        "if (" ^ (expression_to_string condition) ^ ") then {\n" ^
        (expression_to_string then_branch) ^ "\n" ^
        "}\n" ^
        "else {\n" ^
        (expression_to_string else_branch) ^ "\n" ^
        "}"

    | Fix expression -> "fix (" ^ expression_to_string expression ^ ")"

    | Exception (exception_name, _, target_expression) ->
        "exception " ^ exception_name ^ " in (" ^ (expression_to_string target_expression) ^ ")"
    | Throw (exception_name, argument_expression, _) ->
        "throw " ^ exception_name ^ " (" ^ (expression_to_string argument_expression) ^ ")"
    | Try (throwing_expression, handlers) ->
        "try (" ^ (expression_to_string throwing_expression) ^ ")\n" ^
        "catch " ^ (handlers_to_string handlers)
and handlers_to_string = function
    | [] -> ""
    | (exception_name, parameter_name, target_expression) :: handlers_tail ->
        "| " ^ exception_name ^ " " ^ parameter_name ^ " -> (" ^
        (expression_to_string target_expression) ^ ")\n" ^
        handlers_to_string handlers_tail


let maybe_expression_to_string maybe_expression =
    match maybe_expression with
    | None -> "None"
    | Some expression -> expression_to_string expression


let assert_equal_expressions ~actual ~expected =
    if actual = expected then
        ()
    else
        failwith ("Got:\n" ^
                  (expression_to_string actual) ^ "\n" ^
                  "Expected:\n" ^
                  (expression_to_string expected) ^ "\n")
