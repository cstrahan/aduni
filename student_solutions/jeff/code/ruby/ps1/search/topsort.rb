$: << "/opt/home/jeff/code/ruby/lib/"
require 'algorithms'

# topsort.rb - a topological sorting algorithm
# Jeffrey M. Radcliffe
# Thu Feb  8, 2001  9:16 AM

class TopologicalSort
  attr_reader :graph ; attr_writer :graph

  def initialize(graph)
    @graph = graph
  end

  def sort
    graph = @graph		# a copy
    graph.calcInDegree          # calculate indgree array
    path = []			# to store the path

    while graph.size > 0	# do the sort
      path.push(graph.removeFirstZeroInDegree)
      graph.display if $VERBOSE
    end

    #display information
    puts "path is #{path.join(' -> ')}"
  end
end

def main


  # create a test graph, and populate it with some vertices
  graph = TopologicalGraph.new("simple graph")
  graph.add([ Node.new(0, [1, 3]),
	      Node.new(1, [2]),
	      Node.new(2, [4, 5]),
	      Node.new(3, [1, 4]),
	      Node.new(4, [5]),
	      Node.new(5, [])
	    ])

  topsort = TopologicalSort.new(graph)
  topsort.sort
end

main


