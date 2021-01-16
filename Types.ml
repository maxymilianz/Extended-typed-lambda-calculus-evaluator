type type_type =
    | BoolType
    | NaturalType
    | AbstractionType of (type_type * type_type)


let maybe_type_to_string maybe_type =
    let rec aux = function
        | BoolType -> "Bool"
        | NaturalType -> "Natural"
        | AbstractionType (parameter_type, body_type) ->
            "(" ^ (aux parameter_type) ^ ") -> " ^ (aux body_type) in
    match maybe_type with
    | None -> "None"
    | Some type_value -> aux type_value
