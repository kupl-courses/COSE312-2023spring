def deep_flatten(a):
  res = []
  for x in a:
    if isinstance(x, int):
      res.append(x)
    elif isinstance(x, list):
      for t in deep_flatten(x):
        res.append(t)
  return res 

a = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
b = [[1, 2, 3], 4, [[5, 6], 7, [[8, [9, 10]], 11]]]
print(deep_flatten(a))
print(deep_flatten(b))
print(deep_flatten([[[1, [1, 2, 3]], [2]], [[3], [4], [[1, 2, 3], [4, 5, 6, [[8, [9, 10]], 11]], [7, 8, 9]]]]))