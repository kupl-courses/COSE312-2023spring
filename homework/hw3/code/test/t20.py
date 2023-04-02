def even(n): return n % 2 == 0
def odd(n):  return n % 2 == 1

def smaller(x, y): 
  if x < y: return x 
  else: return y

def smallest(x,y,z):
  return smaller(smaller(x, y), z)

def smaller_even(x, y): 
  if even(x) and even(y): return smaller(x, y)
  elif even(x) and odd(y): return x 
  elif odd(x) and even(y): return y
  else: return None  

def smallest_even(x,y,z):
  if smaller_even (x, y) == None: 
    if even(z): return z 
    else: return None 
  else: 
    return smaller_even(smaller_even (x, y), z)

print (smallest_even(2, 4, 6)) # 2
print (smallest_even(1, 4, 6)) # 4
print (smallest_even(2, -4, -5)) # -4
print (smallest_even(10, -3, -1)) # 10
print (smallest_even(1, -3, -1)) # None