def even(n):
  if n == 0: return True
  elif n == 1: return False
  else: return odd(n-1)
  
def odd(n):
  if n == 0: return False
  elif n == 1: return True
  else: return even(n-1)
  
assert even(10)
assert odd(even(512))
print(even(100))
print(odd(101))