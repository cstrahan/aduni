# maxsort.rb
# Jeffrey Radcliffe

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
