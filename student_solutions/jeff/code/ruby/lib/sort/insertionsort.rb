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
