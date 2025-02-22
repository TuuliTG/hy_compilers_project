import compiler
from compiler.domain import L, SourceLocation, Token, TokenType
import compiler.ast as ast
from compiler.types import Bool, Int, Type, Unit

"""
If the tokens starting at pos match the things that the parsing function wants to parse,
then move pos forward past the matching tokens and
return an AST representing what was parsed.

Otherwise, raise an error.
"""

left_associative_binary_operators = [
    ['or'],
    ['and'],
    ['==', '!='],
    ['<', '<=', '>', '>='],
    ['+', '-'],
    ['*', '/', '%'],
]

unary_operators = [
    ['not', '-']
]

right_associative_operators = [
    ['=']
]

precedence_order_list = (
    right_associative_operators
    + left_associative_binary_operators
    + unary_operators
)


def parse(tokens: list[Token], pos: int = 0) -> ast.Expression | None:

    def peek() -> Token:
        if tokens:
            if pos < len(tokens):
                return tokens[pos]
            else:
                return Token(
                    loc=tokens[-1].loc,
                    type=TokenType.END,
                    text="",
                )
        else:
            raise IndexError("Array of tokens was empty")

    def look_back() -> Token:
        if tokens and pos > 0:
            return tokens[pos-1]
        else:
            raise IndexError(f"Cannot look back to pos {pos - 1}")

    def _validate_that_expression_ends_with_semicolumn_if_needed(
        tokens: list[Token],
        new_position: int,
        latest_expression: ast.Expression
    ) -> None:
        if len(tokens) > new_position and not tokens[new_position].text == ';' and not look_back().text == '}':
            raise Exception(f"""
            {tokens[new_position-1].loc}: Expected \';\', but found '{tokens[new_position-1].text}'
            """
            )

    def consume(expected: str | list[str] | None = None) -> Token:
        nonlocal pos
        token = peek()

        if isinstance(expected, str) and token.text != expected:
            raise Exception(f'{token.loc}: expected "{expected}"')
        if isinstance(expected, list) and token.text not in expected:
            comma_separated = ", ".join([f'"{e}"' for e in expected])
            raise Exception(f'{token.loc}: expected one of: {comma_separated}')

        pos += 1
        return token

    def parse_int_literal() -> ast.Literal:
        if peek().type != TokenType.INT_LITERAL:
            raise Exception(f'{peek().loc}: expected an integer literal')
        token = consume()
        return ast.Literal(location=token.loc, value=int(token.text))

    def parse_identifier() -> ast.Expression:
        if peek().type != TokenType.IDENTIFIER:
            raise Exception(f'{peek().loc}: expected an identifier')
        elif peek().text == 'var':
            raise Exception(
                f"Variable declaration is only allowed in top level: {peek().loc}")
        elif peek().text == 'true':
            token = consume()
            return ast.BooleanLiteral(token.loc, True)
        elif peek().text == 'false':
            token = consume()
            return ast.BooleanLiteral(token.loc, False)
        elif peek().text == 'while':
            return parse_while_loop()
        else:
            token = consume()
            if peek().text == '(':
                return parse_function_call(function_name=token.text, location=token.loc)
            return ast.Identifier(location=token.loc, name=token.text)

    def parse_factor() -> ast.Expression | None:
        if peek().text == '(':
            return parse_parenthesized()
        elif peek().text == "if":
            return parse_if_expression()
        elif peek().text == '{':
            return parse_block()
        elif peek().type == TokenType.INT_LITERAL:
            return parse_int_literal()
        elif peek().type == TokenType.IDENTIFIER:
            return parse_identifier()
        elif peek().type == TokenType.PUNCTUATION:
            return parse_punctuation()
        else:
            raise Exception(
                f'{peek().loc}: expected "(", an integer literal or an identifier but got {peek().text}, {peek().type}')

    def parse_punctuation() -> ast.Expression | None:
        token = consume()
        if token.text == ';':
            return None
        else:
            return None

    def parse_parenthesized() -> ast.Expression:
        consume('(')
        expr = parse_binary_operator_level(0)
        consume(')')
        return expr

    def parse_while_loop() -> ast.Expression:
        token = consume('while')
        while_condition = parse_binary_operator_level(0)
        consume('do')
        if peek().text == '{':
            do_expression = parse_block()
        else:
            do_expression = parse_binary_operator_level(0)
        return ast.WhileLoop(location=token.loc, while_condition=while_condition, do_expression=do_expression)

    def parse_block() -> ast.Expression:
        token = consume('{')
        expressions: list[ast.Expression] = []
        if peek().text == '}':
            consume('}')
            return ast.BlockExpression(location=token.loc, expressions=[ast.Literal(location=token.loc, value=None)])
        while True:
            if peek().text == "var":
                expression = parse_variable_declaration()
            else:
                expression = parse_binary_operator_level(0)
            expressions.append(expression)
            ends_with_semicolon = False
            if peek().text == ';':
                consume(';')
                ends_with_semicolon = True

            # ; not needed after curly braces and before end of the block. Otherwise raise an error
            elif peek().type != TokenType.END and tokens[pos-1].text != '}' and peek().text != "}":
                raise Exception(f"""
                  {tokens[pos].loc}: Expected \';\', but found '{tokens[pos].text}' """
                )
            if peek().type == TokenType.END:
                raise Exception("Expected } but found end-of-file")
            # Block ends. If latest expression ended with semicolon, add Literal value None
            if peek().text == '}':
                consume('}')
                if ends_with_semicolon:
                    expressions.append(ast.Literal(
                        location=expressions[0].location, value=None))
                return ast.BlockExpression(token.loc,
                                           expressions=expressions
                                           )

    def parse_if_expression() -> ast.Expression:
        if_token = consume('if')
        condition_branch = parse_binary_operator_level(0)
        consume('then')
        then_branch = parse_binary_operator_level(0)

        if (peek().text == 'else'):
            consume('else')
            else_branch = parse_binary_operator_level(0)
            return ast.IfExpression(
                if_token.loc,
                condition_branch=condition_branch,
                then_branch=then_branch,
                else_branch=else_branch
            )
        return ast.IfExpression(if_token.loc, condition_branch=condition_branch, then_branch=then_branch, else_branch=None)

    def parse_function_call(function_name: str, location: SourceLocation) -> ast.Expression:
        consume('(')
        args: list[ast.Expression] = []
        if peek().text == ')':
            consume(')')
            return ast.FunctionExpression(location=location, function_name=function_name, args=args)
        while True:
            args.append(parse_binary_operator_level(0))
            token = consume([',', ')'])
            if token.text == ',':
                continue
            else:
                break
        return ast.FunctionExpression(location=location, function_name=function_name, args=args)

    def parse_unary_operation(level: int, current_level_tokens: list[str]) -> ast.Expression:
        if peek().text in current_level_tokens:
            operator_token = consume()
            operator = ast.Operator(
                location=operator_token.loc, token=operator_token.text
            )
            if operator_token.text == '-':
                factor = parse_factor()
                if factor is not None:
                    operand = factor
                else:
                    raise Exception("Unary operand was None")
            else:
                bo = parse_binary_operator_level(0)
                if bo is not None:
                    operand = bo
                else:
                    raise Exception("Unary operand was None")

            return ast.UnaryExpression(location=operator_token.loc, operator=operator, operand=operand)
        else:
            return parse_binary_operator_level(level=level+1)

    def parse_assignment(variable_name: ast.Identifier, level: int) -> ast.Expression:
        consume("=")
        initializer = parse_binary_operator_level(level)
        return ast.Assignment(location=variable_name.location, variable_name=variable_name, initializer=initializer)

    def parse_right_associative_operation(level: int, current_level_tokens: list[str]) -> ast.Expression:
        left = parse_binary_operator_level(level + 1)
        if peek().text in current_level_tokens:
            if peek().text == '=' and isinstance(left, ast.Identifier):
                return parse_assignment(left, level)
            else:
                operator_token = consume()
                operator = ast.Operator(
                    location=operator_token.loc, token=operator_token.text)
                right = parse_binary_operator_level(
                    level)
                return ast.BinaryOp(location=left.location, left=left, op=operator, right=right)
        return left

    def parse_left_associative_operation(level: int, current_level_tokens: list[str]) -> ast.Expression:
        left = parse_binary_operator_level(level + 1)
        while peek().text in current_level_tokens:
            operator_token = consume()
            operator = ast.Operator(
                location=operator_token.loc, token=operator_token.text)
            right = parse_binary_operator_level(level + 1)
            left = ast.BinaryOp(
                location=left.location,
                left=left, op=operator, right=right
            )
        return left

    def parse_binary_operator_level(level: int) -> ast.Expression:
        if level == len(precedence_order_list):
            factor = parse_factor()
            if factor is not None:
                return factor
            else:
                raise Exception("Factor was None")

        current_level_tokens = precedence_order_list[level]

        if current_level_tokens in right_associative_operators:
            return parse_right_associative_operation(level, current_level_tokens)

        elif current_level_tokens in left_associative_binary_operators:
            return parse_left_associative_operation(level, current_level_tokens)

        elif current_level_tokens in unary_operators:
            return parse_unary_operation(
                level=level, current_level_tokens=current_level_tokens)

        raise Exception(
            f"Unsupported operator type at precedence level {level}")

    def parse_type() -> Type:
        type = consume().text
        match type:
            case "Int":
                return Int
            case "Bool":
                return Bool
            case _:
                raise Exception(f"Unkown type {type}")

    def parse_variable_declaration() -> ast.Expression:
        location = peek().loc
        consume("var")
        if peek().type != TokenType.IDENTIFIER:
            raise Exception(f'{tokens[pos].loc}: expected an identifier')
        variable_name = consume().text
        if peek().text == ':':
            consume(':')
            variable_type = parse_type()
            consume("=")
            initializer = parse_binary_operator_level(0)
            return ast.VariableDeclaration(location=location, variable_name=variable_name, type=variable_type, initializer=initializer)
        consume("=")
        initializer = parse_binary_operator_level(0)
        return ast.VariableDeclaration(location=location, variable_name=variable_name, initializer=initializer)

    def parse_expressions() -> ast.Expression | None:
        ends_with_semicolon = False
        expressions = []
        if not tokens:
            return None
        while (pos < len(tokens)):
            if peek().text == "var":
                expression = parse_variable_declaration()
            else:
                expression = parse_binary_operator_level(0)
            ends_with_semicolon = False

            _validate_that_expression_ends_with_semicolumn_if_needed(
                tokens, pos, expression
            )

            if peek().text == ';':
                ends_with_semicolon = True
                consume(';')

            expressions.append(expression)

        if len(expressions) == 1 and isinstance(expressions[0], ast.BlockExpression):
            return expressions[0]

        if _should_append_unit_in_the_end(expressions, ends_with_semicolon):
            expressions.append(ast.Literal(location=L, value=None))

        return ast.BlockExpression(location=expressions[0].location, expressions=expressions)

    return parse_expressions()


def _should_append_unit_in_the_end(expressions: list[ast.Expression], ends_with_semicolon: bool) -> bool:
    if not isinstance(expressions[-1], ast.BlockExpression):
        if ends_with_semicolon:
            return True
        else:
            return False
    else:
        return False
