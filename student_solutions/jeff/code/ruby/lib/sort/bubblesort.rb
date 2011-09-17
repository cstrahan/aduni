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
