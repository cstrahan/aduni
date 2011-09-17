#!/usr/local/bin/ruby
# bubble sort, written in ruby

$passes = 0


def bubbleSort(array)
  for i in 0..(array.length - 1)
    switch = false
    for j in 0..(array.length - 2)
      if array[j] > array[j+1]
	swap(array, j, j+1)
	switch = true
      end
    end
    $passes += 1
    return if switch == false
  end
end

def insertionSort(array)
  for j in 0..(array.length-1)
    key = array[j]
    i = j - 1 
    while (i >= 0 && array[i] > key)
      array[i+1] = array[i]
      i -= 1
    end
    array[i+1] = key
    display(array)
  end
end

def swap(array, a, b)
  temp = array[a]
  array[a] = array[b]
  array[b] = temp
end

def display(array)
  print array.join(" "), "\n"
end  

def main
# array = [4 , 1 , 5, 10 , 6, 7, 2, 3, 8, 9]
# array = [12, 10, 3, 37, 57, 2, 23, 9]
  array = 
      [ 503, 87, 512, 61, 908, 170, 897, 275, 653, 426, 154, 509,
      612,677, 765, 703 ]
 
#  display(array)
#  bubbleSort(array)
  insertionSort(array)
  display(array)
  print "total passes: #{$passes}\n"
end

main

