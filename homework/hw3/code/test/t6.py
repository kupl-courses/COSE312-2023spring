lst = [0, 1, 2]
i = 3

while i < 10:
  lst.append(i)
  i += 1
  
print(lst)

i = 0
while i < 10:
  print(lst[i]) 
  i += 1
  
i = 0
while i < 100:
  lst[i] *= 2
  i += 1
  if i < 10: continue
  if i >= 10: break

print(lst)

i = 0
j = 0
while i < 100:
  while j < 200:
    j += 1
    if i + j > 100: continue
    if j > 88:
      break
  i += 1

print(i)
print(j)