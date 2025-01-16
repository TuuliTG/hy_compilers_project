import re
from compiler.domain import Token, SourceLocation, L, TokenType

# While we’ve not reached the end of the input:
# 1. For each regex: try to match the input at the current location.
# 2. If the whitespace regex matches, skip over the whitespace.
# 3. If the comment regex matches, skip over the comment.
# 4. If some token regex matches, add the token to the result and then skip over it.
# 5. Otherwise raise an error.


identifier_regex = re.compile(r'[a-zA-Z_][a-zA-Z0-9_]*')
integer_literal_regex = re.compile(r'(?<![a-zA-Z])\d+(?![a-zA-Z])')
whitespace_regex = re.compile(r'\s')
new_line_regex = re.compile(r'\n')
operators_regex = re.compile(r'==|!=|<=|>=|\+|\*|\-|/|=|<|>')
punctuation_regex = re.compile(r'[)(}{,;]')
comment_regex = re.compile(r'#|//')
multiline_comment_start_regex = re.compile(r'/[*]')
multiline_comment_end_regex = re.compile(r'[*]/')


def tokenize(source_code: str) -> list[Token]:
    tokens: list[Token] = []

    index = 0
    line = 1
    line_start = 0

    while index < len(source_code):
        print(f"Evaluating from index {index}")

        whitespace = whitespace_regex.match(source_code, index)
        if whitespace is not None:
            line, line_start, index = _handle_white_space(
                whitespace, line, line_start, index)
            continue

        comment = comment_regex.match(source_code, index)
        if comment is not None:
            print("found comment")
            index = _skip_comment(source_code, index)
            continue

        multiline_comment_start = multiline_comment_start_regex.match(
            source_code, index)
        if multiline_comment_start is not None:
            print("Found multiline comment start")
            lines_to_add, index = _find_closing_multiline_comment(
                source_code, index)
            line += lines_to_add
            continue

        identifier = identifier_regex.match(source_code, index)

        if identifier is not None:
            token, index = _handle_integer_operator_or_identifier(
                TokenType.IDENTIFIER, identifier, line, line_start, index)
            tokens.append(token)
            continue

        operator = operators_regex.match(source_code, index)
        if operator is not None:
            token, index = _handle_integer_operator_or_identifier(
                TokenType.OPERATOR, operator, line, line_start, index)
            tokens.append(token)
            continue

        integer = integer_literal_regex.match(source_code, index)
        if integer is not None:
            token, index = _handle_integer_operator_or_identifier(
                TokenType.INT_LITERAL, integer, line, line_start, index)
            tokens.append(token)
            continue

        punctuation = punctuation_regex.match(source_code, index)
        if punctuation is not None:
            token, index = _handle_integer_operator_or_identifier(
                TokenType.PUNCTUATION, punctuation, line, line_start, index)
            tokens.append(token)
            continue

        print("No match for regex found")
        raise ValueError("Invalid character or token")
    return tokens


def _skip_comment(source_code: str, index: int) -> int:
    pattern = r"\n"
    match = re.search(pattern, source_code[index:])
    if match:
        return index + match.start()
    return -1


def _handle_white_space(whitespace: re.Match, line: int, line_start: int, index: int) -> tuple[int, int, int]:
    print(f"found whitespace {whitespace}")
    if whitespace.group() == "\n":
        line += 1
        line_start = whitespace.end()
    index += 1

    return line, line_start, index


def _handle_integer_operator_or_identifier(type: TokenType, match: re.Match, line: int, line_start: int, index: int) -> tuple[Token, int]:
    print(f"found token type: {type.value} {match}")
    column = index - line_start
    index = index + len(match.group())
    token = Token(SourceLocation(
        "dummy", line, column), type=type, text=match.group()
    )
    return token, index


def _find_closing_multiline_comment(input_code: str, start_index: int = 0) -> tuple[int, int]:
    lines_to_add = 0

    for index, char in enumerate(input_code[start_index:], start=start_index):
        if char == '\n':
            lines_to_add += 1

        # Check for closing `*/`
        if input_code[index:index + 2] == '*/':
            return lines_to_add, index + 2

    # If we reach here, closing `*/` was not found
    raise ValueError("The multiline comment is not closed")
