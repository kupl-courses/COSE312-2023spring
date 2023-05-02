def foo(x):
  return [x, x, x]

a = foo(0)
b = foo('hello')
a[0] + b[2]   # Bug -  0 + 'hello'