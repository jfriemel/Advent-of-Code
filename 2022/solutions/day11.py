import math
import os
import re
from dataclasses import dataclass
from functools import reduce


@dataclass
class Monkey:
    items: list[int]
    operation: str
    divisor: int
    recipients: (int, int)
    inspections: int = 0


def parse_monkeys(monkey_input: str) -> list[Monkey]:
    monkey_blocks = map(lambda s: s.split('\n'), monkey_input.strip().split('\n\n'))
    monkeys = []
    for m_block in monkey_blocks:
        items = list(map(int, re.findall(r'\d+', m_block[1])))
        operation = m_block[2].split('new = ')[1]
        divisor = int(m_block[3].split('divisible by ')[1])
        recipients = (int(m_block[4].split('to monkey ')[1]), int(m_block[5].split('to monkey ')[1]))
        monkeys.append(Monkey(items, operation, divisor, recipients))
    return monkeys


def get_monkey_business(monkeys: list[Monkey], num_rounds: int, worry_divisor: int) -> int:
    # Use lcm of all divisors to keep worry level manageable
    lcm = reduce(lambda d1, d2: math.lcm(d1, d2), [m.divisor for m in monkeys])

    for _ in range(num_rounds):
        for monkey in monkeys:
            monkey.inspections += len(monkey.items)
            while len(monkey.items) > 0:
                old = monkey.items.pop()  # old is used in monkey.operation
                new = (eval(monkey.operation) // worry_divisor) % lcm
                if new % monkey.divisor == 0:
                    monkeys[monkey.recipients[0]].items.append(new)
                else:
                    monkeys[monkey.recipients[1]].items.append(new)

    monkeys.sort(reverse=True, key=lambda m: m.inspections)
    return monkeys[0].inspections * monkeys[1].inspections


def part_a(puzzle_input: str) -> str:
    monkeys = parse_monkeys(puzzle_input)
    return str(get_monkey_business(monkeys, 20, 3))


def part_b(puzzle_input: str) -> str:
    monkeys = parse_monkeys(puzzle_input)
    return str(get_monkey_business(monkeys, 10_000, 1))


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input11.txt')).read()
    print(f'Part A: {part_a(input_str)}')
    print(f'Part B: {part_b(input_str)}')
