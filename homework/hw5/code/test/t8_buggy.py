x = int(input())
c = 2

while c > 0:
  if isinstance(x, int):
    x = input()
  if isinstance(x, str):
    x = int(input())
  c -= 1

x + 'world'   # Buggy