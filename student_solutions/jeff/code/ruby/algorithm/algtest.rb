$: << "/home/jeffrey/incoming/ruby/lib/"

require 'algorithm/sort'
array = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

display(array)
sort = HeapSort.new
sort.sort(array)
display(array)
puts "foo"
