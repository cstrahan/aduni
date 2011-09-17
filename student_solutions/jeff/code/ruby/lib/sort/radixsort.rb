# radixsort.rb
# Jeffrey Radcliffe
# Sat Feb  3, 2001  8:00 PM

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
