open Language


module ExceptionToParameterType = Map.Make(String)
module VariableToType = Map.Make(String)


let check_type expression =
    let rec aux exception_to_parameter_type variable_to_type = function
        | Variable variable_name ->
            VariableToType.find_opt variable_name variable_to_type
        | Abstraction ((parameter_name, parameter_type), body) ->
            (match aux exception_to_parameter_type
                   (VariableToType.add parameter_name parameter_type variable_to_type) body with
             | None -> None
             | Some body_type -> Some (AbstractionType (parameter_type, body_type)))
        | Application (function_expression, argument_expression) ->
            (match aux exception_to_parameter_type variable_to_type function_expression with
             | Some (AbstractionType (parameter_type, body_type)) ->
                if aux exception_to_parameter_type variable_to_type argument_expression =
                   Some parameter_type then
                    Some body_type
                else
                    None
             | _ -> None)

        | Natural _ -> Some NaturalType
        | Add (expression_0, expression_1)
        | Multiply (expression_0, expression_1)
        | Subtract (expression_0, expression_1)
        | Divide (expression_0, expression_1) ->
            (match aux exception_to_parameter_type variable_to_type expression_0,
                   aux exception_to_parameter_type variable_to_type expression_1 with
             | Some NaturalType, Some NaturalType -> Some NaturalType
             | _ -> None)
        | Equal (expression_0, expression_1) ->
            (match aux exception_to_parameter_type variable_to_type expression_0,
                   aux exception_to_parameter_type variable_to_type expression_1 with
             | Some NaturalType, Some NaturalType -> Some BoolType
             | _ -> None)

        | True
        | False -> Some BoolType
        | If (condition, then_branch, else_branch) ->
            (match aux exception_to_parameter_type variable_to_type condition with
             | Some BoolType ->
                (match aux exception_to_parameter_type variable_to_type then_branch with
                 | None -> None
                 | some_type_value ->
                    if aux exception_to_parameter_type variable_to_type else_branch =
                       some_type_value then
                        some_type_value
                    else
                        None)
             | _ -> None)

        | Fix expression ->
            (match aux exception_to_parameter_type variable_to_type expression with
             | Some (AbstractionType (AbstractionType (parameter_type_0, body_type_0),
                                      AbstractionType (parameter_type_1, body_type_1))) ->
                if parameter_type_0 = parameter_type_1 && body_type_0 = body_type_1 then
                    Some (AbstractionType (parameter_type_0, body_type_1))
                else
                    None
             | _ -> None)

        | Exception (exception_name, parameter_type, target_expression) ->
            aux (ExceptionToParameterType.add exception_name
                 parameter_type exception_to_parameter_type)
                variable_to_type target_expression
        | Throw (exception_name, argument, target_type) ->
            (match ExceptionToParameterType.find_opt exception_name exception_to_parameter_type with
             | None -> None
             | some_parameter_type ->
                if aux exception_to_parameter_type variable_to_type argument =
                   some_parameter_type then
                    Some target_type
                else
                    None)
        | Try (throwing_expression, handlers) ->
            (match aux exception_to_parameter_type variable_to_type throwing_expression with
             | None -> None
             | Some final_type ->
                if handlers_have_type handlers final_type
                   exception_to_parameter_type variable_to_type then
                    Some final_type
                else
                    None)
    and handlers_have_type handlers final_type exception_to_parameter_type variable_to_type =
        let rec handlers_aux = function
        | [] -> true
        | (exception_name, parameter_name, target_expression) :: handlers_tail ->
            (match ExceptionToParameterType.find_opt exception_name exception_to_parameter_type with
             | None -> false
             | Some parameter_type ->
                let new_variable_to_type =
                    VariableToType.add parameter_name parameter_type variable_to_type in
                (match aux exception_to_parameter_type new_variable_to_type target_expression with
                 | None -> false
                 | Some handler_type ->
                    if handler_type = final_type then
                        handlers_aux handlers_tail
                    else
                        false)) in
        handlers_aux handlers in
    aux ExceptionToParameterType.empty VariableToType.empty expression
