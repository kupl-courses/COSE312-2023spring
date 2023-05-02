def string_of_stars(n):
  res = ""
  for i in range(1, n+1):
    for j in range(1, n+1):
      if i == 1 or i == n or j == 1 or j == n or i == j or j == n - i + 1:
        res += "*"
      else:
        res += " "
    res += "\n"
  return res

print(string_of_stars(5))
print(string_of_stars(6))
print(string_of_stars(9))
print(string_of_stars(20))