def collatz (n):
  steps = 0
  while n != 1:
    if n % 2 == 0: n = n // 2
    else: n = 3 * n + 1
    steps += 1 
  return steps

print (collatz(3)) # 7
print (collatz(4)) # 2
print (collatz(5)) # 5
print (collatz(13)) # 9
print (collatz(25)) # 23
print (collatz(100)) # 25
print (collatz(2022)) # 63
print (collatz(12345)) # 50
