# heapsort.rb
# author JMR
# date Sat Feb  3, 2001  3:49 PM

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
