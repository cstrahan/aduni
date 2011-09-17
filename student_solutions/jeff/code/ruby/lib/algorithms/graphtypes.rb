# adjacencyarray.rb
# Jeffrey M. Radcliffe
# Wed Feb  7, 2001  8:44 PM

# a simple graph abstraction
class Graph
  attr_reader :nodes

  def initialize
    @nodes = []
    @name = "Graph"
  end
  
  def add(new)
    new.each {|i| @nodes.push(i) }
  end

  def remove(val)
    @nodes.each { |i| i.remove(val) }
    @nodes.delete_if { |x| x.key == val }
    @nodes.compact
  end
  
  def nodeAt(val)
    @nodes.each {|x| return x if x.key == val }
  end

  def size
    @nodes.length
  end

  def display
    puts "\n#{@name}\n------------------------"
    @nodes.each {|i| i.display }
  end
end


# a special sort of graph that does topological things. =)
class TopologicalGraph < Graph
  # finds the first vertex with a 0 indegree in removes it,
  # making sure it unlinks to the other vertices
  def removeFirstZeroInDegree
    @array.each do |i| 
      if i.indegree == 0 
	unlink(i)
	remove(i.key) 
	return i.key
      end
    end
  end

  def calcInDegree
    ind = []
    0.upto(@nodes.length - 1) { ind.push(0) }
    @nodes.each {|i| i.calcInDegree(ind) }
    0.upto(@nodes.length - 1) do |i|
      @nodes[i].indegree = ind[i]
    end
  end

  def showind
    @nodes.each {|i| puts "indegree of vertex #{i.key}: #{i.indegree}" }
  end

  def unlink(foo)
    # this method will subtract from indegrees of other vertices
    temp = foo.connections.each do |i| 
      @nodes.each { |x| x.indegree -= 1 if x.key == i }
    end
  end
end

