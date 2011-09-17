load("../sort/mergesort.rb")
load("../sort/sortutils.rb")
array = [12, 10, 3, 37, 57, 2, 23, 9, 2, 0, 31, 249, 1, 4]
target = ARGV[0].to_i

sort = MergeSort.new
sort.sort(array)
display(array)

class BinarySearch
  def search(array, target)
    # first, find a place close to the middle of the array.
    # Ruby will return nil if we are out of index.
    middle = 1
    middle *= 2 while array[middle] != nil
    return binarySearch(array, 0, middle, target)
  end
  
  # basic binary search, assumes sorted array
  def binarySearch(array, p, r, target)
    q = (r + p) / 2
    return q if array[q] == target
    if target < array[q]
      binarySearch(array, p,  q, target)
    else
      binarySearch(array, q, r, target)
    end
  end
end

search = BinarySearch.new

puts "searching for #{target}"
puts "index of #{target} is #{search.search(array, target)}"
    
