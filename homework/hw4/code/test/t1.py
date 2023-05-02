x = 1
y = 2
print(x, y)

c = d = 1 
print("c =", c, ", d =", d)

x, y = 1, 2
print(x, y)
x, y = y, x
print(x, y)

t = y, x
x, y = t
print(x, y)

a, b = 1, 2
print(a+b)

((a, b), c) = ((1, 2), 3)
print(a, b, c)

[a, b] = [1, 2]
print(a, b)

[a, b] = 1, 2
print(a, b)

[a, [b, (c, d)]] = ["a", [1, (2, 3)]]
print(a, b, c, d)

a, b = [1, 2]
print(a, b)

a, b = "12"
print(a, b)

[a, b] = "12"
print(a, b)

(a, (b, c)) = [1, [2, "abc"]]
print(a, b, c)
