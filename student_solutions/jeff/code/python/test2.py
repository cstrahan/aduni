x = 10 * 3.14
y = 200*200
s = 'The value of x is ' + `x` + ', and y is ' + `y` + '...'
print s

import string
for x in range(1, 11):
    print string.rjust(`x`, 2), string.rjust(`x*x`, 3),
    print string.rjust(`x*x*x`, 4)


for x in range(1, 11):
    print '%2s %3d %4d' % (x, x*x, x*x*x)

table = {'Sjoeard': 4127, 'Jack': 4098, 'Dcab': 7678}
for name, phone in table.items():
    print '%-10s ==> %10d' % (name, phone)


