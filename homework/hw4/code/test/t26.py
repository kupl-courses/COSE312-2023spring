def perm(a, n):
    if n == 0: return []
    if n == 1: return [[x] for x in a]
    if n > 1: 
        b = perm(a, n-1)
        l = []
        return [[x] + y for x in a for y in b]
      
print(perm([1, 2, 3, 4], 1))
print(perm([1, 2, 3, 4], 2))
print(perm([1, 2, 3, 4], 3))
print(perm([1, 2, 3, 4], 4))
