x = int(input())
y = int(input())

while y <= 100:
  y += 1
  
if x < 50 :
  if x < y :
    x = "SmallNumber"
    
x+1 # TypeError (str + int)