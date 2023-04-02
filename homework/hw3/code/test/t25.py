def drop(f, a):
    skip = True
    result = []
    for x in a:
        if not f(x): skip = False 
        if skip: 
            pass
        else:
            result.append(x)
    return result
  
print(drop(lambda x: x < 7, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
print(drop(lambda x: x % 5 == 0, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))