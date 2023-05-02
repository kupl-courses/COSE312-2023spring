x = int(input())
a = input()
b = input()
c = input()

if x > 0 :
  a = int(input())
  b = int(input())

  if a == 42 and b < 0:
    c = a+b     # THEN -  c : int
                # ELSE -  c : str

d = a+b         # IF x > 0 -  d : int
                # ELSE     -  d : str

c+d      # TypeError -  {int, str} + {int, str}