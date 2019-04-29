#!/usr/bin/env python3

import random

template = '''
fn main():
    print(String({expr}))
'''

def gen_expr(target, prev_operator=None, depth=1):
    if depth > 5:
        return literal_expr(target, depth)
    options = [
        literal_expr,
        times_expr,
        divide_expr,
        paren_expr,
    ]
    if prev_operator not in ('*', '/'):
        options += [minus_expr, plus_expr]
    option = random.choice(options)
    return option(target, depth+1)

def literal_expr(target, depth):
    return str(target)

def plus_expr(target, depth):
    delta = random.randint(-1000, 1000)
    left = gen_expr(target-delta, '+', depth)
    right = gen_expr(delta, '+', depth)
    return left + ' + ' + right

def minus_expr(target, depth):
    rhs = random.randint(-1000, 1000)
    left = gen_expr(target+rhs, '-', depth)
    right = literal_expr(rhs, depth)
    return left + ' - ' + right

def times_expr(target, depth):
    factor = random.choice(factorize(target))
    left = gen_expr(target//factor, '*', depth)
    right = literal_expr(factor, depth)
    return left + ' * ' + right

def divide_expr(target, depth):
    multiplicand = random.randint(1, 100)
    left = gen_expr(target*multiplicand, '/', depth)
    right = literal_expr(multiplicand, depth)
    return left + r' / ' + right

def paren_expr(target, depth):
    inner = gen_expr(target, None, depth)
    return '(' + inner + ')'

def factorize(n):
    factors = []
    factor = 1
    while True:
        if n % factor == 0:
            factors.append(factor)
            k = n / factor
            if k != factor:
                factors.append(factor)
        factor += 1
        if factor*factor > n:
            break
    return factors


def main():
    expr = gen_expr(target=42)
    result = template.format(expr=expr).strip()
    print(result)


if __name__ == '__main__':
    main()
