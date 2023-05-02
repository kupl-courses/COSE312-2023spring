x = int(input())
a = input()
b = input()
c = int(input())

if isinstance(x, int):
  c = a + b
  a = int(input())
  b = int(input())
  
d = a + b
e = c + d # TypeError (str + int)