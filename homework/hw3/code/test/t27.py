def collect(a):
  if a == []: return []
  else:
    hd = a[0]
    res = []
    for x in a:
      if x == hd:
        res.append(x)
      else:
        break
    return res

print(collect([1,1,1,2]))
print(collect([1,1,2]))
print(collect([1]))
print(collect([2,2,2,3,3]))