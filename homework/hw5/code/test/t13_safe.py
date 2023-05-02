x = int(input())
a = input()
b = input()
c = input()
d = input()

if x > 0 :
  a = int(input())
  b = int(input())

  if a == 42 and b < 0:
    c = a+b     # THEN -  c : int
                # ELSE -  c : str

if x > 0 and a == 42 and b < 0:
    d = a+b     # THEN -  d : int
                # ELSE -  d : str

c+d             # Safe -  (int + int) or (str + str)