class CountingSort
  def sort(array)
    counter = []
    for i in 0..(array.max)
      counter[i] = 0
    end
    for i in 0..(array.length - 1)
      counter[array[i]] += 1
    end
    j = 0
    for i in 0..(counter.length - 1)
      for k in 0..counter[i]
	array[j] = i
	j += 1
      end
    end
  end
end

