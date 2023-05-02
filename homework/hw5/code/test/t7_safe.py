def foo(x):
  if isinstance(x, str):
    return len(x)
  return x

def goo(x):
  if isinstance(x, int):
    if x < 100:
      return 50
  return 'hello'

t1 = goo(int(input()))       # type(t1) = {int, str}
t2 = goo(int(input()))       # type(t2) = {int, str}
t3 = foo(t1)          # type(t3) = {int}
t4 = foo(t2)          # type(t4) = {int}
t3 + t4               # Safe