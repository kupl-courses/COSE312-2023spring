def triangledown(n : int) -> str:
  s = ""
  for i in range(1, n+1):
    s = s + " " * (i-1)
    s = s + "*" * ((n-i)*2+1)   
    s = s + "\n" 
  return s    

print(triangledown(4))
print(triangledown(9))
print(triangledown(15))