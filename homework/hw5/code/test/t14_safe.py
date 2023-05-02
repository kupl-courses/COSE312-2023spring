def is_one_digit(x) :
  if (x >= 0) and (x < 10) :
    return True
  else :
    return None

keysym = int(input())

if keysym >= 0 :
  if keysym < 16 :
    if int(input()) :
      if keysym >= 10 : pass
      else :
        key = is_one_digit(keysym) - 1
    else :
      if keysym >= 10 : pass
      else :
        key = is_one_digit(keysym) + 1