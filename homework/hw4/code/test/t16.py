lst = [n for n in range(10)]
print(lst)

lst = [n * n for n in range(1, 20) if n % 2 == 0 if n % 3 == 0]
print(lst)

lst = [n * m for n in range(10) if n < 5 or n > 8 for m in range(10, 20) if m % 2 == 1]
print(lst)

lst = [[1, 2, 3], 
       [4, 5, 6],
       [7, 8, 9]
      ]

print(lst)
print([[n ** 2 for n in row] for row in lst])