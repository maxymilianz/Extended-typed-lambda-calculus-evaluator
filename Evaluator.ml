open Language
open Types


module VariableToValue = Map.Make(String)


let subtract_saturating number_0 number_1 =
    if number_0 < number_1 then
        0
    else
        number_0 - number_1


let create_boolean = function
    | true -> True
    | false -> False


exception GenericException of string * expression  (* for evaluated language exceptions *)

exception ExceptionScopeExit of string  (* exception trying to exit its scope *)


let rec find_handler desired_exception_name = function
    | [] -> None
    | (exception_name, parameter_name, target_expression) :: handlers ->
        if exception_name = desired_exception_name then
            Some (parameter_name, target_expression)
        else
            find_handler desired_exception_name handlers


let rec substitute_for_variables variable_to_value expression =
    match expression with
    | Variable variable_name ->
        (match VariableToValue.find_opt variable_name variable_to_value with
         | None -> expression
         | Some value -> value)
    | Abstraction ((parameter_name, parameter_type), body) ->
        let new_variable_to_value = VariableToValue.remove parameter_name variable_to_value in
        Abstraction ((parameter_name, parameter_type),
                     substitute_for_variables new_variable_to_value body)
    | Application (function_expression, argument_expression) ->
        Application (substitute_for_variables variable_to_value function_expression,
                     substitute_for_variables variable_to_value argument_expression)

    | Natural _ -> expression
    | Add (expression_0, expression_1) ->
        Add (substitute_for_variables variable_to_value expression_0,
             substitute_for_variables variable_to_value expression_1)
    | Multiply (expression_0, expression_1) ->
        Multiply (substitute_for_variables variable_to_value expression_0,
                  substitute_for_variables variable_to_value expression_1)
    | Subtract(expression_0, expression_1) ->
        Subtract (substitute_for_variables variable_to_value expression_0,
                  substitute_for_variables variable_to_value expression_1)
    | Divide (expression_0, expression_1) ->
        Divide (substitute_for_variables variable_to_value expression_0,
                substitute_for_variables variable_to_value expression_1)
    | Equal (expression_0, expression_1) ->
        Equal (substitute_for_variables variable_to_value expression_0,
               substitute_for_variables variable_to_value expression_1)

    | True
    | False -> expression
    | If (condition, then_branch, else_branch) ->
        If (substitute_for_variables variable_to_value condition,
            substitute_for_variables variable_to_value then_branch,
            substitute_for_variables variable_to_value else_branch)

    | Fix expression ->
        Fix (substitute_for_variables variable_to_value expression)

    | Exception (exception_name, parameter_type, target_expression) ->
        Exception (exception_name, parameter_type,
                   substitute_for_variables variable_to_value target_expression)
    | Throw (exception_name, argument_expression, target_type) ->
        Throw (exception_name, substitute_for_variables variable_to_value argument_expression,
               target_type)
    | Try (throwing_expression, handlers) ->
        let process_handler (exception_name, parameter_name, target_expression) =
            let new_variable_to_value = VariableToValue.remove parameter_name variable_to_value in
            (exception_name, parameter_name,
             substitute_for_variables new_variable_to_value target_expression) in
        let new_handlers = List.map process_handler handlers in
        Try (substitute_for_variables variable_to_value throwing_expression, new_handlers)


let evaluate expression =
    let rec aux variable_to_value expression =
        match expression with
        | Variable variable_name ->
            VariableToValue.find variable_name variable_to_value
        | Abstraction ((parameter_name, parameter_type), body) ->
            let new_variable_to_value = VariableToValue.remove parameter_name variable_to_value in
            Abstraction ((parameter_name, parameter_type),
                         substitute_for_variables new_variable_to_value body)
        | Application (function_expression, argument_expression) ->
            let evaluated_argument = aux variable_to_value argument_expression in
            (match aux variable_to_value function_expression with
             | Abstraction ((parameter_name, _), body) ->
                let new_variable_to_value =
                    VariableToValue.add parameter_name evaluated_argument variable_to_value in
                aux new_variable_to_value body
             | expression ->
                failwith ("Not an abstraction: " ^ (expression_to_string expression)))

        | Natural _ -> expression
        | Add (expression_0, expression_1) ->
            (match aux variable_to_value expression_0,
                   aux variable_to_value expression_1 with
             | Natural number_0, Natural number_1 -> Natural (number_0 + number_1)
             | expression_0, expression_1 ->
                failwith ("Not natural: " ^ (expression_to_string expression_0) ^
                          " or " ^ (expression_to_string expression_1)))
        | Multiply (expression_0, expression_1) ->
            (match aux variable_to_value expression_0,
                   aux variable_to_value expression_1 with
             | Natural number_0, Natural number_1 -> Natural (number_0 * number_1)
             | expression_0, expression_1 ->
                failwith ("Not natural: " ^ (expression_to_string expression_0) ^
                          " or " ^ (expression_to_string expression_1)))
        | Subtract (expression_0, expression_1) ->
            (match aux variable_to_value expression_0,
                   aux variable_to_value expression_1 with
             | Natural number_0, Natural number_1 -> Natural (subtract_saturating number_0 number_1)
             | expression_0, expression_1 ->
                failwith ("Not natural: " ^ (expression_to_string expression_0) ^
                          " or " ^ (expression_to_string expression_1)))
        | Divide (expression_0, expression_1) ->
            (match aux variable_to_value expression_0,
                   aux variable_to_value expression_1 with
             | Natural number_0, Natural number_1 -> Natural (number_0 / number_1)
             | expression_0, expression_1 ->
                failwith ("Not natural: " ^ (expression_to_string expression_0) ^
                          " or " ^ (expression_to_string expression_1)))
        | Equal (expression_0, expression_1) ->
            (match aux variable_to_value expression_0,
                   aux variable_to_value expression_1 with
             | Natural number_0, Natural number_1 -> create_boolean (number_0 = number_1)
             | expression_0, expression_1 ->
                failwith ("Not natural: " ^ (expression_to_string expression_0) ^
                          " or " ^ (expression_to_string expression_1)))

        | True
        | False -> expression
        | If (condition, then_branch, else_branch) ->
            (match aux variable_to_value condition with
             | True ->
                aux variable_to_value then_branch
             | False ->
                aux variable_to_value else_branch
             | expression ->
                failwith ("Neither True nor False: " ^ (expression_to_string expression)))

        | Fix expression ->
            (match aux variable_to_value expression with
             | Abstraction ((_, AbstractionType (parameter_type, body_type)), _) as evaluated_expression ->
                let aux_expression = Abstraction (
                    ("f", parameter_type),
                    (* type not important in Evaluator, because typechecking happens earlier *)
                    Abstraction (
                        ("x", parameter_type),
                        Application (
                            Application (
                                Variable "f'",
                                Application (
                                    Variable "f",
                                    Variable "f"
                                )
                            ),
                            Variable "x"
                        )
                    )
                ) in
                let fix_expression = Abstraction (
                    ("f'", AbstractionType (AbstractionType (parameter_type, body_type),
                                            AbstractionType (parameter_type, body_type))),
                    Application (
                        aux_expression,
                        aux_expression
                    )
                ) in
                aux variable_to_value (Application (fix_expression, evaluated_expression))
             | _ -> failwith ("Not a proper abstraction: " ^ (expression_to_string expression)))

        | Exception (exception_name, _, target_expression) ->
            (try
                aux variable_to_value target_expression
             with
                GenericException (actual_exception_name, _) as generic_exception ->
                    if exception_name = actual_exception_name then
                        raise (ExceptionScopeExit exception_name)
                    else
                        raise generic_exception)
        | Throw (exception_name, argument_expression, _) ->
            let evaluated_argument = aux variable_to_value argument_expression in
            raise (GenericException (exception_name, evaluated_argument))
        | Try (throwing_expression, handlers) ->
            try
                aux variable_to_value throwing_expression
            with
                GenericException (exception_name, argument) as generic_exception ->
                    (match find_handler exception_name handlers with
                     | None -> raise generic_exception
                     | Some (parameter_name, target_expression) ->
                        let new_variable_to_value =
                            VariableToValue.add parameter_name argument variable_to_value in
                        aux new_variable_to_value target_expression) in
    aux VariableToValue.empty expression
