# cyk.rb

# Author:   Jeffrey Radcliffe
#   Date:   Mon Feb 26, 2001 12:23 PM

# S -> AB | BC
# A -> BA | 0
# B -> CC | 1
# C -> AB | 0

s = "10010"
n = s.length
table = [] 
(n + 1).times do  table << []  end
for x in 1..n
  table[x][1] = 
end
for y in 2..n		        # y = length of substring
  for x in 1..(n - y + 1) 
    table[x][y] = []
    for k in 1..(y-1)		# k = length of 1st part of substring
      table[x][y] = [table[x][y], table[x][k], table[x+k][y-k]].union
    end
  end
end

puts table
