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


def parse(tokens: list[Token]) -> ast.Expression:
    pos = 0

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
        elif peek().type == TokenType.INT_LITERAL:
            return parse_int_literal()
        elif peek().type == TokenType.IDENTIFIER:
            return parse_identifier()
        else:
            raise Exception(
                f'{peek().loc}: expected "(", an integer literal or an identifier')

    def parse_parenthesized() -> ast.Expression:
        consume('(')
        expr = parse_binary_operator_level(0)
        consume(')')
        return expr

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

    def parse_binary_operator_level(level: int) -> ast.Expression:
        if level == len(left_associative_binary_operators):
            return parse_factor()

        left = parse_binary_operator_level(level + 1)

        while peek().text in left_associative_binary_operators[level]:
            operator_token = consume()
            operator = operator_token.text

            right = parse_binary_operator_level(level + 1)
            left = ast.BinaryOp(left, operator, right)

        return left

    return parse_binary_operator_level(0)
