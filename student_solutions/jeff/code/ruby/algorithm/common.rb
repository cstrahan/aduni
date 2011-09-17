# common.rb - contains vertex classes and sundry
# Jeffrey M Radcliffe
# Thu Feb  8, 2001  9:28 AM

class Node
  attr_reader :key, :connections, :indegree, :parent, :color, :startOrder, :endOrder, :mark
  attr_writer :indegree, :parent, :color, :startOrder, :endOrder, :mark, :key

  def initialize(key, connections)
    @key = key
    @connections = connections
  end

  def display
    message = "Node #{key}:"
    message += " (#{@color})" 
    @connections.each { |i| message += " --> #{i}" }
    puts message
  end

  def remove(val)
    @connections.delete(val)
    @connections.compact!
  end

  def calcInDegree(array)
    @connections.each {|i| array[i] += 1 }
  end
end

class TreeNode < Node
  attr_reader :left, :right
  attr_writer :left, :right
  
  def initialize(key)
    super(key, [])
    @left = @right = nil
  end
end

#-------------------------------------------------
# these classes are for weighted graphs
# an edge has weight and a target
class Edge
  attr_reader :target, :weight
  attr_writer :target, :weight

  def initialize(target, weight)
    @target = target
    @weight = weight
  end
end

# a vertex object that uses weighted edges 
# !@#$ Do this better... get a (x, y) edge parameter.
class WeightedNode< Node
  def display
    message = "Vertex #{key}:"
    @connections.each do |i| 
      message += " (#{i.weight})-> #{i.target}"
    end
    puts message
  end

  def remove(val)
    @connections.delete_if {|x| x.target == val }
    @connections.compact
  end

  def calcInDegree(array)
    @connections.each {|i| 
      return if i == nil
      array[i.target] += 1 }
  end
end
#-------------------------------------------------

