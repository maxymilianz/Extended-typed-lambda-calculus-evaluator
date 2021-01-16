open Language


module ExceptionNames = Set.Make(String)
module ExceptionToParameterType = Map.Make(String)
module VariableToType = Map.Make(String)


let check_type expression =
    let rec aux exception_to_parameter_type variable_to_type = function
        | Variable variable_name ->
            (match VariableToType.find_opt variable_name variable_to_type with
             | None -> None
             | Some type_value -> Some (type_value, ExceptionNames.empty))
        | Abstraction ((parameter_name, parameter_type), body) ->
            (match aux exception_to_parameter_type
                   (VariableToType.add parameter_name parameter_type variable_to_type) body with
             | None -> None
             | Some (body_type, exception_names) ->
                Some (AbstractionType (parameter_type, body_type), exception_names))
        | Application (function_expression, argument_expression) ->
            (match aux exception_to_parameter_type variable_to_type function_expression with
             | Some (AbstractionType (parameter_type, body_type), function_exception_names) ->
                (match aux exception_to_parameter_type variable_to_type argument_expression with
                 | Some (argument_type, argument_exception_names) ->
                    if parameter_type = argument_type then
                        Some (body_type,
                              ExceptionNames.union function_exception_names
                              argument_exception_names)
                    else
                        None
                 | _ -> None)
             | _ -> None)

        | Natural _ -> Some (NaturalType, ExceptionNames.empty)
        | Add (expression_0, expression_1)
        | Multiply (expression_0, expression_1)
        | Subtract (expression_0, expression_1)
        | Divide (expression_0, expression_1) ->
            (match aux exception_to_parameter_type variable_to_type expression_0,
                   aux exception_to_parameter_type variable_to_type expression_1 with
             | (Some (NaturalType, exception_names_0)), (Some (NaturalType, exception_names_1)) ->
                Some (NaturalType, ExceptionNames.union exception_names_0 exception_names_1)
             | _ -> None)
        | Equal (expression_0, expression_1) ->
            (match aux exception_to_parameter_type variable_to_type expression_0,
                   aux exception_to_parameter_type variable_to_type expression_1 with
             | (Some (NaturalType, exception_names_0)), (Some (NaturalType, exception_names_1)) ->
                Some (BoolType, ExceptionNames.union exception_names_0 exception_names_1)
             | _ -> None)

        | True
        | False -> Some (BoolType, ExceptionNames.empty)
        | If (condition, then_branch, else_branch) ->
            (match aux exception_to_parameter_type variable_to_type condition with
             | Some (BoolType, condition_exception_names) ->
                (match aux exception_to_parameter_type variable_to_type then_branch with
                 | None -> None
                 | Some (then_type, then_exception_names) ->
                    (match aux exception_to_parameter_type variable_to_type else_branch with
                     | Some (else_type, else_exception_names) ->
                        if then_type = else_type then
                            Some (then_type,
                                  ExceptionNames.(union
                                  (union condition_exception_names then_exception_names)
                                  else_exception_names))
                        else
                            None
                     | None -> None))
             | _ -> None)

        | Fix expression ->
            (match aux exception_to_parameter_type variable_to_type expression with
             | Some (AbstractionType (AbstractionType (parameter_type_0, body_type_0),
                                      AbstractionType (parameter_type_1, body_type_1)),
                     exception_names) ->
                if parameter_type_0 = parameter_type_1 && body_type_0 = body_type_1 then
                    Some (AbstractionType (parameter_type_0, body_type_1), exception_names)
                else
                    None
             | _ -> None)

        | Exception (exception_name, parameter_type, target_expression) ->
            (match aux (ExceptionToParameterType.add exception_name
                       parameter_type exception_to_parameter_type)
                      variable_to_type target_expression with
             | Some (type_value, exception_names) ->
                if ExceptionNames.mem exception_name exception_names then
                    None
                else
                    Some (type_value, exception_names)
             | None -> None)
        | Throw (exception_name, argument, target_type) ->
            (match ExceptionToParameterType.find_opt exception_name exception_to_parameter_type with
             | None -> None
             | some_parameter_type ->
                (match aux exception_to_parameter_type variable_to_type argument with
                 | Some (parameter_type, exception_names) ->
                    Some (target_type, ExceptionNames.add exception_name exception_names)
                 | None -> None))
        | Try (throwing_expression, handlers) ->
            (match aux exception_to_parameter_type variable_to_type throwing_expression with
             | None -> None
             | Some (final_type, exception_names) ->
                let rec handlers_to_handled_exception_names = function
                    | [] -> ExceptionNames.empty
                    | (exception_name, _, _) :: handlers_tail ->
                        ExceptionNames.add exception_name
                        (handlers_to_handled_exception_names handlers_tail) in
                let handled_exception_names = handlers_to_handled_exception_names handlers in
                (match handlers_to_exception_names handlers final_type
                   exception_to_parameter_type variable_to_type with
                 | Some thrown_exception_names ->
                    Some (final_type, ExceptionNames.(union (diff exception_names
                                                             handled_exception_names)
                                                      thrown_exception_names))
                 | None -> None))
    and handlers_to_exception_names handlers final_type
        exception_to_parameter_type variable_to_type =
        let rec handlers_aux = function
        | [] -> Some ExceptionNames.empty
        | (exception_name, parameter_name, target_expression) :: handlers_tail ->
            (match ExceptionToParameterType.find_opt exception_name exception_to_parameter_type with
             | None -> None
             | Some parameter_type ->
                let new_variable_to_type =
                    VariableToType.add parameter_name parameter_type variable_to_type in
                (match aux exception_to_parameter_type new_variable_to_type target_expression with
                 | None -> None
                 | Some (handler_type, exception_names) ->
                    if handler_type = final_type then
                        (match handlers_aux handlers_tail with
                         | None -> None
                         | Some tail_exception_names ->
                            Some (ExceptionNames.union exception_names tail_exception_names))
                    else
                        None)) in
        handlers_aux handlers in
    match aux ExceptionToParameterType.empty VariableToType.empty expression with
    | None -> None
    | Some (type_value, exception_names) ->
        if ExceptionNames.is_empty exception_names then
            Some type_value
        else
            None
