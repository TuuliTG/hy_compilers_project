from compiler.domain import Token, TokenType
import compiler.ast as ast

"""
If the tokens starting at pos match the things that the parsing function wants to parse,
then move pos forward past the matching tokens and 
return an AST representing what was parsed.

Otherwise, raise an error.
"""


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

    def parse_identifier() -> ast.Identifier:
        if peek().type != TokenType.IDENTIFIER:
            raise Exception(f'{peek().loc}: expected an identifier')
        token = consume()
        return ast.Identifier((token.text))

    def parse_term() -> ast.Expression:
        left = parse_factor()
        while peek().text in ['*', '/']:
            operator_token = consume()
            operator = operator_token.text
            right = parse_factor()
            left = ast.BinaryOp(
                left,
                operator,
                right
            )
        return left

    def parse_factor() -> ast.Expression:
        if peek().text == '(':
            return parse_parenthesized()
        elif peek().type == TokenType.INT_LITERAL:
            return parse_int_literal()
        elif peek().type == TokenType.IDENTIFIER:
            return parse_identifier()
        else:
            raise Exception(
                f'{peek().loc}: expected "(", an integer literal or an identifier')

    def parse_parenthesized() -> ast.Expression:
        consume('(')
        expr = parse_expression()
        consume(')')
        return expr

    def parse_expression() -> ast.Expression:
        left = parse_term()
        while peek().text in ['+', '-']:
            operator_token = consume()
            operator = operator_token.text

            right = parse_term()
            left = ast.BinaryOp(
                left,
                operator,
                right
            )

        return left

    return parse_expression()
