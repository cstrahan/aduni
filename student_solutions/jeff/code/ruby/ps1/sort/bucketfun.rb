# bucketfun.rb

require 'algorithm/sort'

# this takes O(n) time
class Bucket
  def sort(array, n)
    bucketsort(array, n, 1)
    bucketsort(array, n, 0)
  end
  
  def bucketsort(array, n, part) # part is div or mod
    counter = []
    for i in 0..n
      counter[i] = []
    end

    # now sort by div ...
    for i in 0..(array.length - 1)
      t = array[i].divmod(n) 
      counter[t[part]].push(array[i])
    end

    # ... and copy back into the array
    k = 0
    for i in 0..(counter.length - 1)
      for j in 0..(counter[i].length - 1)
	array[k] = counter[i][j] ; k += 1 
      end
    end
  end
end  

def test
  a = [9, 2 ,4, 10, 5, 2, 7, 8]
  b = [10, 3, 4, 5, 6, 7, 8, 9]
  c = []

  # construct new array
  for i in 0..(a.length - 1)
    c[i] = a[i] * b[i]
  end

  sort = Bucket.new
  display(c)
  sort.sort(c, 10)
  display(c)
end

test
