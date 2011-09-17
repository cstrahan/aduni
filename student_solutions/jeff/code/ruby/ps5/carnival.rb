# carnival.rb

# a simple node to store value information as well as parent information
class Node
  attr_reader :val, :mark, :parent
  attr_writer :parent, :mark, :val
  def initialize(val)
    @val = val
  end
  def <=> (other)
    return -1 if (@val > other.val)
    return  1 if (@val < other.val)
    return  0
  end
  
  def to_s
    message = @val.to_s
    message += "*" if @mark == 1
    return message
  end
end

# initialize the array
array = [
  # start
  [10, 1, 9, 2],		
  [3, 14, 7, 4],		
  [8, 4, 9, 1],			
  [2, 5, 3, 6],			
  [7, 2, 12, 7],
  [7, 13, 1, 11],
  [3, 16, 5, 9]
  # finish
  ]

# initialize path array
path = [] ; array.length.times { path << [] }

# carry out the dynamic programming algorithm
for i in 0...array.length
  for j in 0...array[0].length
    # set the parent of this node as the min of previous choices, 
    # and the value as the value of this number plus that of the  parent
    path[i][j] = Node.new(array[i][j]) 
    next if i == 0
    a = j + 1 ; a = 0 if j + 1 == (array[0].length)
    min = [path[i-1][j-1].val, path[i-1][j].val, path[i-1][a].val].min
    path[i][j].val += min
    if path[i-1][j-1].val == min
      parent = path[i-1][j-1]
    elsif path[i-1][j].val == min
      parent = path[i-1][j]
    else
      parent = path[i-1][a]
    end
    path[i][j].parent = parent
  end
end

# highlight the best path
best = path.last.max
best.mark = 1
while best.parent != nil
  best = best.parent
  best.mark = 1
end

# print out the array
path.each { |x| puts x.join("\t") }


