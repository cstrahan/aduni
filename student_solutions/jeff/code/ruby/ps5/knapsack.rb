#!/usr/local/bin/ruby
# knapsack.rb

class Box
  attr_reader :weight, :value, :maximum
  def initialize(w, v)
    @weight = w
    @value = v
    @maximum = 10000000000000	# !@#$ kludge!
  end
end
#===========================================================================
class State
  attr_reader :value, :number, :maximum, :weight
  attr_writer :number
  def initialize(value, number)
    @value = @weight = value
    @maximum = number
  end
end

def makeElectoralCollege
  return [
    State.new(3, 9), State.new(4, 6), State.new(5, 4),
    State.new(6, 2), State.new(7, 3), State.new(8, 6),
    State.new(9, 2), State.new(10, 2), State.new(11, 4),
    State.new(15, 1), State.new(18, 1), State.new(21, 1),
    State.new(22, 1), State.new(23, 1), State.new(25, 1),
    State.new(32, 1), State.new(33, 1), State.new(54, 1) ]
end
#===========================================================================
class Knapsack
  attr_reader :max
  attr_writer :max

  def initialize(max)
    @max = max
  end

  def fill(bins)
    sack = [] ; bins.length.times { sack << [] }
    bins.each_with_index do |box, i| 
      num = 0
      for w in 0..@max
	num += 1 if (num + 1) * box.weight <= w and (num + 1) <= box.maximum
	value = num * box.value
	p = w - (box.weight * num) ; p = 0 if p < 0
	if i == 0 then sack[i][w] = value else
	  sack[i][w] = [value + sack[i-1][p], sack[i-1][w]].max   # !@#$ this is BAD! 
	end
      end
    end
    return sack.clone
  end
end
#===========================================================================
def test
#   bins = [Box.new(2, 3), Box.new(3, 8), Box.new(5, 14)]
#   max = 49
  bins = makeElectoralCollege
  max = 269

  knapsack = Knapsack.new(max)
  result = knapsack.fill(bins)
  result.each_index { |i| puts result[i].join(" ") }
end

test				# run the test




