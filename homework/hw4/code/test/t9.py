def fact(n):
    i = 1
    r = 1
    while i <= n:
        r *= i
        i += 1
    return r

def factorial(n): return fact(n)

print(factorial(10))
