%{
    open Language
    open Types
%}


%token LEFT_PARENTHESIS
%token RIGHT_PARENTHESIS

%token BOOL_TYPE
%token NATURAL_TYPE

%token <string> ID
%token FUNCTION
%token COLON
%token ARROW

%token <int> NATURAL
%token PLUS
%token STAR
%token MINUS
%token SLASH
%token EQUAL

%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE

%token FIX

%token EXCEPTION
%token OF
%token IN
%token RAISE
%token AS
%token TRY
%token CATCH
%token PIPE

%token END_OF_FILE

%start <expression option> parse_program


%%


parse_program:
    | END_OF_FILE { None }
    | value_token = parse_value; END_OF_FILE { Some value_token };

parse_value:
    | LEFT_PARENTHESIS; value_token = parse_value; RIGHT_PARENTHESIS
        { value_token }

    | variable_name = ID
        { Variable variable_name }
    | FUNCTION; parameter_name = ID; COLON; type_value = parse_type; ARROW; body = parse_value
        { Abstraction ((parameter_name, type_value), body) }
    | LEFT_PARENTHESIS; abstraction = parse_value; RIGHT_PARENTHESIS; argument = parse_value
        { Application (abstraction, argument) }

    | natural = NATURAL
        { Natural natural }
    | PLUS; LEFT_PARENTHESIS; expression_0 = parse_value; RIGHT_PARENTHESIS; expression_1 = parse_value
        { Add (expression_0, expression_1) }
    | STAR; LEFT_PARENTHESIS; expression_0 = parse_value; RIGHT_PARENTHESIS; expression_1 = parse_value
        { Multiply (expression_0, expression_1) }
    | MINUS; LEFT_PARENTHESIS; expression_0 = parse_value; RIGHT_PARENTHESIS; expression_1 = parse_value
        { Subtract (expression_0, expression_1) }
    | SLASH; LEFT_PARENTHESIS; expression_0 = parse_value; RIGHT_PARENTHESIS; expression_1 = parse_value
        { Divide (expression_0, expression_1) }
    | EQUAL; LEFT_PARENTHESIS; expression_0 = parse_value; RIGHT_PARENTHESIS; expression_1 = parse_value
        { Equal (expression_0, expression_1) }

    | TRUE
        { True }
    | FALSE
        { False }
    | IF; condition = parse_value; THEN; then_branch = parse_value; ELSE; else_branch = parse_value
        { If (condition, then_branch, else_branch) }

    | FIX; expression = parse_value
        { Fix expression }

    | EXCEPTION; exception_name = ID; OF; type_value = parse_type; IN; expression = parse_value
        { Exception (exception_name, type_value, expression) }
    | RAISE; exception_name = ID; argument = parse_value; AS; type_value = parse_type
        { Throw (exception_name, argument, type_value) }
    | TRY; expression = parse_value; CATCH; handlers = parse_handlers
        { Try (expression, handlers) }

parse_type:
    | BOOL_TYPE
        { BoolType }
    | NATURAL_TYPE
        { NaturalType }
    | LEFT_PARENTHESIS; parameter_type = parse_type; ARROW; body_type = parse_type; RIGHT_PARENTHESIS
        { AbstractionType (parameter_type, body_type) }

parse_handlers:
    | handler = parse_handler
        { [handler] }
    | handler = parse_handler; PIPE; handlers = parse_handlers
        { handler :: handlers }

parse_handler:
    | exception_name = ID; parameter_name = ID; ARROW; LEFT_PARENTHESIS; expression = parse_value; RIGHT_PARENTHESIS
        { exception_name, parameter_name, expression }
