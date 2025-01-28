import compiler
from compiler.domain import Token, TokenType
import compiler.ast as ast

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


def parse(tokens: list[Token], pos: int = 0) -> tuple[ast.Expression, int]:

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
        return ast.Literal(int(token.text))

    def parse_identifier() -> ast.Expression:
        if peek().type != TokenType.IDENTIFIER:
            raise Exception(f'{peek().loc}: expected an identifier')
        else:
            token = consume()
            if peek().text == '(':
                return parse_function_call(function_name=token.text)
            return ast.Identifier(token.text)

    def parse_factor() -> ast.Expression:
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
            return

    def parse_parenthesized() -> ast.Expression:
        consume('(')
        expr = parse_binary_operator_level(0)
        consume(')')
        return expr

    def parse_block() -> ast.Expression:
        consume('{')
        expressions: list[ast.Expression] = []
        while True:
            expressions.append(parse_binary_operator_level(0))
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
                    expressions.append(ast.Literal(value=None))
                return ast.BlockExpression(
                    expressions=expressions
                )

    def parse_if_expression() -> ast.Expression:
        consume('if')
        condition_branch = parse_binary_operator_level(0)
        consume('then')
        then_branch = parse_binary_operator_level(0)

        if (peek().text == 'else'):
            consume('else')
            else_branch = parse_binary_operator_level(0)
            return ast.IfExpression(condition_branch, then_branch=then_branch, else_branch=else_branch)
        return ast.IfExpression(condition_branch, then_branch=then_branch, else_branch=None)

    def parse_function_call(function_name):
        consume('(')
        args: list[ast.Expression] = []
        while True:
            args.append(parse_binary_operator_level(0))
            token = consume([',', ')'])
            if token.text == ',':
                continue
            else:
                break
        return ast.FunctionExpression(function_name=function_name, args=args)

    def parse_unary_operation(level, current_level_tokens) -> ast.Expression:
        if peek().text in current_level_tokens:
            operator_token = consume()
            operator = operator_token.text
            operand = parse_binary_operator_level(0)
            return ast.UnaryExpression(operator=operator, operand=operand)
        else:
            return parse_binary_operator_level(level=level+1)

    def parse_right_associative_operation(level, current_level_tokens) -> ast.Expression:
        left = parse_binary_operator_level(level + 1)
        if peek().text in current_level_tokens:
            operator_token = consume()
            operator = operator_token.text
            right = parse_binary_operator_level(
                level)
            return ast.BinaryOp(left, operator, right)
        return left

    def parse_left_associative_operation(level, current_level_tokens) -> ast.Expression:
        left = parse_binary_operator_level(level + 1)
        while peek().text in current_level_tokens:
            operator_token = consume()
            operator = operator_token.text
            right = parse_binary_operator_level(level + 1)
            left = ast.BinaryOp(left, operator, right)
        return left

    def parse_binary_operator_level(level: int) -> ast.Expression:
        if level == len(precedence_order_list):
            return parse_factor()

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

    return parse_binary_operator_level(0), pos


def parse_expressions(tokens: list[Token]) -> ast.Expression | None:
    ends_with_semicolon = False
    expressions = []
    pos = 0
    if not tokens:
        return None
    while (pos < len(tokens)):
        expression, new_pos = parse(tokens, pos)
        ends_with_semicolon = False

        _validate_that_expression_ends_with_semicolumn_if_needed(
            tokens, new_pos, expression
        )

        if len(tokens) > new_pos and tokens[new_pos].text == ';':
            ends_with_semicolon = True

        pos = new_pos + 1
        expressions.append(expression)

    if len(expressions) == 1 and isinstance(expressions[0], ast.BlockExpression):
        return expressions[0]

    if _should_append_unit_in_the_end(expressions, ends_with_semicolon):
        expressions.append(ast.Literal(value=None))

    return ast.BlockExpression(expressions=expressions)


def _validate_that_expression_ends_with_semicolumn_if_needed(tokens, new_position, latest_expression):
    if len(tokens) > new_position and not tokens[new_position].text == ';':
        if not isinstance(latest_expression, ast.BlockExpression):
            raise Exception(f"""
              {tokens[new_position-1].loc}: Expected \';\', but found '{tokens[new_position-1].text}' """
            )


def _should_append_unit_in_the_end(expressions, ends_with_semicolon) -> bool:
    if not isinstance(expressions[-1], ast.BlockExpression):
        if ends_with_semicolon:
            return True
    else:
        return False
