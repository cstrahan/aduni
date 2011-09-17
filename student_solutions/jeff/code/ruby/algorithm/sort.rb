# ----------------------------
#        BUBBLE SORT
# ----------------------------
class BubbleSort
  def sort(array)
    bound = array.length - 2
    for i in 0..(array.length - 1)
      t = 0
      for j in 0..bound
	if array[j] > array[j+1]
	  swap(array, j, j+1)
	  t = j
	end
      end
      return if t == 0
      bound = t
    end
  end
end
# ----------------------------
#        COUNTING SORT
# ----------------------------
class CountingSort
  def sort(array)
    counter = []
    for i in 0..(array.max)
      counter[i] = 0
    end
    for i in 0..(array.length - 1)
      counter[array[i]] += 1
    end
    j = 0
    for i in 0..(counter.length - 1)
      for k in 0..counter[i]
	array[j] = i
	j += 1
      end
    end
  end
end

# ----------------------------
#        HEAP SORT
# ----------------------------
class Heap
  # a few utility methods
  def parent(i) 
    return i / 2 
  end

  def left(i)
    return (2 * i)
  end

  def right(i)
    return (2 * i) + 1
  end

  def value(i)
    return @anArray[i - 1]
  end

  def heapify(array, i)
    left = left(i)
    right = right(i)

    if left <= @heapsize and value(left) > value(i)
      largest = left
    else
      largest = i
    end
    if right <= @heapsize and value(right) > value(largest)
      largest = right
    end
    if largest != i
      swap (array, i - 1, largest - 1)
      heapify(array, largest)
    end
  end
end

class HeapSort < Heap
  @heapsize = 0
  @anArray = []

  def sort(array)
    @anArray = array
    buildheap(@anArray)
    (array.length).downto(2) do |i|
      swap(@anArray, 0, i - 1)
      @heapsize -= 1
      heapify(@anArray, 1)
    end
  end

  def buildheap(array)
    @heapsize = array.length
    (array.length / 2).downto(1) do |i|
      heapify(array, i)
    end
  end
end

# ----------------------------
#       INSERTION  SORT
# ----------------------------
class InsertionSort
  def sort(array)
    for j in 0..(array.length-1)
      key = array[j]
      i = j - 1 
      while (i >= 0 && array[i] > key)
	array[i+1] = array[i]
	i -= 1
      end
      array[i+1] = key
    end
  end
end

# ----------------------------
#       MAX  SORT
# ----------------------------
class MaxSort
  def sort(array)
    (array.length - 1).downto(0) do |bound|
      max = 0
      for i in 1..bound
	max = i if array[i] > array[max]
      end
      swap(array, max, bound)
    end
  end
end

# ----------------------------
#       MERGE SORT
# ----------------------------
class MergeSort
  def sort(array)
    mSort(array, 0, (array.length - 1))
  end
  
  # the recursive part of merge sort
  def mSort(array, p, r)
    if p < r
      q = (p+r) / 2
      mSort(array, p, q)
      mSort(array, q + 1, r)
      merge(array, p, q + 1, r + 1)
    end
  end

  # the merging routine
  def merge(array, p, q, r)
    temp = []
    left = p
    left_count = q - p
    right = q 
    right_count = r - q 
    i = 0;
    
    # while there are elements in both halves
    while left_count != 0 && right_count != 0
      if array[left] > array[right]
	temp[i] = array[right]
	i += 1 ; right_count -= 1 ; right += 1
      else
	temp[i] = array[left]
	i += 1 ; left_count -= 1 ; left += 1
      end
    end

    # while there are only elements in one half
    while left_count > 0
      temp[i] = array[left]
      i += 1 ; left_count -= 1 ; left += 1
    end
    while right_count > 0
      temp[i] = array[right]
      i += 1 ; right_count -= 1 ; right += 1
    end
    
    # copy the newly sorted values back into the array
    i = 0
    for j in p..r - 1
      array[j] = temp[i]
      i += 1
    end
  end
end

# ----------------------------
#       QUICKSORT
# ----------------------------
class QuickSort
  # this method is the front end. recursion
  # is used in the qSort
  def sort(array)
    @anArray = array
    qSort(@anArray, 1, array.length)
  end

  def qSort(array, p, r)
    if p < r
      q = partition(array, p, r)
      qSort(array, p, q)
      qSort(array, q+1, r)
    end
  end

  def partition(array, p, r)
    # set key and pointers
    x = value(p)
    i = p - 1
    j = r + 1
    while true
      j -= 1 ; j -= 1 until value(j) <=  x
      i += 1 ; i += 1 until value(i) >= x
      if i < j
	swap(array, i - 1, j - 1)
      else
	return j
      end
    end
  end

  def value(i)
    return @anArray[i - 1]
  end

  @anArray = []
end

# ----------------------------
#       RADIX SORT
# ----------------------------
class RadixSort
  def sort(array)
    # determine the maximum digits of the array
    1.upto(findMax(array)) do |d|
      iSort(array, -d)
    end
  end

  # a stable insertion sort, done using only one
  # digit at a time (no small task!)
  def iSort(array, d)
    for j in 0..(array.length-1)
      key = value(array[j], d)
      keyValue = array[j]	# the total value of array[j]
      i = j - 1 
      while (i >= 0 && value(array[i], d) > key)
	array[i+1] = array[i]
	i -= 1
      end
      array[i+1] = keyValue
    end
  end

  # finds the maximum number of digits of any element in
  # an array
  def findMax(array)
    max = 0
    for i in 0..(array.length - 1)
      temp = array[i].to_s.length
      max = temp if temp > max
    end
    return max
  end

  # returns the value of the particular *digit* of a number,
  # or nil if there is nothing there
  def value(element, d)
    temp = element.to_s
    return 0.to_s if temp.to_s[d] == nil
    return temp.to_s[d].chr
  end
end

# ----------------------------
#       UTILITY METHODS
# ----------------------------
def swap(array, a, b)
  temp = array[a]
  array[a] = array[b]
  array[b] = temp
end

def display(array)
  array.each do |i|
    print i, " "
  end
  print "\n"  
end  
