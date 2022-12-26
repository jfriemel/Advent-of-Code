import os


def snafu_to_decimal(snafu: str) -> int:
    dec = 0
    for digit in snafu:
        value = int(digit.replace('-', '-1').replace('=', '-2'))
        dec = 5 * dec + value
    return dec


def decimal_to_snafu(decimal: int) -> str:
    mapping = {-2: '=', -1: '-', 0: '0', 1: '1', 2: '2'}
    snafu = ''
    while decimal > 0:
        mod = decimal % 5
        if mod > 2:
            mod -= 5
            decimal += 5
        snafu = mapping[mod] + snafu
        decimal //= 5
    return snafu


def part_a(puzzle_input: str) -> str:
    snafu_numbers = puzzle_input.strip().split('\n')
    return decimal_to_snafu(sum(map(snafu_to_decimal, snafu_numbers)))


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input25.txt')).read()
    print(f'Part A: {part_a(input_str)}')
