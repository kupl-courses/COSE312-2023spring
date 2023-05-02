def fib(x):
  if x < 2:
    return 1
  elif x < 3:
    return 'hello'
  else:
    return fib(x-1) + fib(x-2)    # TypeError (int + str)

fib(10)