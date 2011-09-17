# Fibonacci series:
# the sum of two elements defines the next
#  a, b = 0, 1
#  while b < 100:
#      print b,
#      a, b = b, a+b

#  print

#  x = int(raw_input("Please enter a number: "))
#  if x < 0:
#      x = 0
#      print 'Negative changed to zero'
#  elif x == 0:
#      print 'Zero'
#  elif x == 1:
#      print 'Single'
#  else:
#      print 'More'

#  # measure some strings
#  a = ['cat', 'window', 'defenestrate']
#  for x in a:
#      print x, len(x)

#  for n in range(2, 10):
#      for x in range(2, n):
#          if n % x == 0:
#              print n, 'equals', x, '*', n/x
#              break
#          else:
#              print n, 'is a prime number'


def fib(n):                             # write Fibonacci series up to n
    "Print a Fibonacci series up to n"
    a, b = 0, 1
    while b < n:
        print b,
        a, b = b, a+b

# fib(2000)
    
def fib2(n):
    result = []
    a, b = 0, 1
    while b < n:
        result.append(b)
        a, b = b, a+b
    return result

# f2000 = fib2(2000)
# print f2000

def ask_ok(prompt, retries=4, complaint='Yes or no, please!'):
    while 1:
        ok = raw_input(prompt)
        if ok in ('y', 'ye', 'yes'): return 1
        if ok in ('n', 'no', 'nop', 'nope'): return 0
        retries = retries - 1
        if retries < 0: raise IOError, 'refusenik user'
        print complaint

# ask_ok('Do you really want to quit?')
    
def add(x, y):
    return x+y

foo = reduce(add, range(1, 11))
print foo
    
        
