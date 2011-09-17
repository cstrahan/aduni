# quicksort.rb
# author: Jeffrey Radcliffe
# date: Sat Feb  3, 2001  6:34 PM

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
