def buggy(x):
  return x + "1" # TypeError (int + str)

f = buggy
f(0) 