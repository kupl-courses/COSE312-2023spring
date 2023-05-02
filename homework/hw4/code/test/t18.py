def change(a, b, c, d, e):
  a, b, c, d, e = c, e, a, b, d
  return a, b, c, d, e

print (change (1, 2, 3, 4, 5)) # (3, 5, 1, 2, 4)
print (change (5, 4, 3, 2, 1)) # (3, 1, 5, 4, 2)
print (change (5, 2, 4, 4, 1)) # (4, 1, 5, 2, 4)