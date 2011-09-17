# coin.rb

# a simple coin class, stores a string descriptor and a value
class Coin
  attr_reader :value
  def initialize(name, value)
    @name = name
    @value = value
  end

  def <=> (other)
    return -1 if @value < other.value
    return  1 if @value > other.value
    return  0
  end

  def to_s
    return @name
  end
end

class Sack
  def initialize(coins)
    @coins = coins
  end
  def to_s
    return @coins.join(" ")
  end
end

# a recursive algorithm to find change
def coin_count(coins, amount, stack)
  if amount == 0
    $numWays += 1
    $ways << stack
  end
  return if amount <= 0 or coins.length == 0                        # base case
  stack << coins.last
  coin_count(coins.clone, amount - coins.last.value, stack.clone)   # C(i, j - value(i) )  
  coins.pop			# take off the highest coin...
  stack.pop			# and take it off the path too
  coin_count(coins.clone, amount, stack.clone)                      # C(i - 1, j) 
end


# the dynamic programming way... formatting it so that we get
# actual coinage amounts is just icing, which I'll not do at 
# present.

# Time complexity for this is 0(number of coins * amount)
def coin_count_dynamic(coins, amount)
  ways = []
  coins.each do ways << [] end
  coins = coins.sort

  for i in 0...coins.length
    numCoins = 0
    v = coins[i].value
    for j in 0..amount
      value = 0
      if ((numCoins + 1) * v) <= j
	numCoins += 1 
	value = 1
      end
      if (j - coins[i].value) > 0 and i != 0
	value += ways[i][j - coins[i].value] 
      end
      value += ways[i-1][j] unless i == 0
      ways[i][j] = value
    end
  end
  $numWays = ways[coins.length - 1].last
  # print out the table
  ways.each { |row| puts row.join("\t") }
end
  

# a test method to try out the recursion
def test
  penny = Coin.new("P", 1)
  nickel = Coin.new("N", 5)
  dime = Coin.new("D", 10)
  quarter = Coin.new("Q", 25)
  halfDollar = Coin.new("H", 50)
  n = 20000

  $numWays = 0
  $ways =[]
  stack = []
  coins = [penny, nickel, dime, quarter, halfDollar]

  coin_count(coins.clone, n, stack)
#  coin_count_dynamic(coins.clone, n)
  
  # print results
  puts "There are #{$numWays} different ways to make #{n} units in change, using { #{coins.join(', ')} }."
  puts "-------------------------------------------------------------------------------"
  $ways.each {|way| puts way.join(" ") }
end

test				# run the puppy
