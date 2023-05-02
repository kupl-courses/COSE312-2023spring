x = int(input())
y = int(input())

if y == 0:
  while y <= 100:
    y += 1

  if y == 101:
    x = "100"  

print(x+y) # TypeError (str + int)