def foo(x):
  return x

def goo(x):
  if isinstance(x, int):
    if x < 100:
      return 50
  return 'hello'

t1 = goo(int(input()))       # type(t1) = {int, str}
t2 = goo(int(input()))       # type(t2) = {int, str}
t3 = foo(t1)          # type(t3) = {int, str}
t4 = foo(t2)          # type(t3) = {int, str}
t3 + t4               # TypeError (int + str, str + int)