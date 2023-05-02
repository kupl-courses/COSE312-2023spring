def sum_evens(n):
  sum = 0
  for i in range(1, n+1):
    if i % 2 == 0: 
      sum += i
  return sum 

print(sum_evens(10))    
print(sum_evens(100))   
