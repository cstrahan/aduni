load "./heapsort.rb"		# contains Heap
load "./sortutils.rb"		# swap, display, etc.

class BuildHeap < Heap
  # iterative method O(n log n)
  def build1(array)
    @anArray = array
    1.upto(@anArray.length) { |i| loop1(@anArray, i) }
  end

  def loop1(array, i)
    p = parent(i)
    if value(i) > value(p)
      swap(array, i-1, p-1)
      loop1(array, p) if parent(p) > 0
    end
  end

  # iterative method O(n)
  def build2(array)
    @anArray = array
    @anArray.length.downto(1) {|i| loop2(@anArray, i) }
  end
  
  def loop2(array, i)
    l =  left(i) ; r = right(i)
    return if value(left(i)) == nil
    if value(right(i)) == nil
      max = l
    else
      value(l) > value(r) ? max = l : max = r
    end
    if value(i) < value(max)
      swap(array, i-1, max-1)
      loop2(array, max)
    end
  end
end

heap = BuildHeap.new

array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

# display(array)
heap.build1(array) if ARGV[0].to_i == 1
heap.build2(array) if ARGV[0].to_i == 2
display(array)
