def sum(list):
    total = 0
    for num in list:
      total = total + num
    return total

assert (sum([1,2,3,4]) == 10)
print("Success")