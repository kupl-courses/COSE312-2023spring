def even_squares(a):
    squares = []
    for x in a:
        if x % 2 == 0:
            squares.append(x**2)
    return squares 
  
print(even_squares([1,2,3,4,5]))