# mergesort.rb
# author Jeffrey Radcliffe
# date Sat Feb  3, 2001 10:14 PM

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
