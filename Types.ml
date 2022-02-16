type type_type =
    | BoolType
    | NaturalType
    | AbstractionType of (type_type * type_type)


let rec type_to_string = function
    | BoolType -> "Bool"
    | NaturalType -> "Natural"
    | AbstractionType (parameter_type, body_type) ->
        "(" ^ (type_to_string parameter_type) ^ ") -> " ^ (type_to_string body_type)


let maybe_type_to_string = function
    | None -> "None"
    | Some type_value -> type_to_string type_value
