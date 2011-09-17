# dfsdriver
$LOAD_PATH << "/home/jeffrey/incoming/ruby/lib/"

require 'algorithm/search'
require 'algorithm/graphs'

def topsearch
  graph = Graph.new
  graph.add([ Node.new(0, [1, 3]),
	      Node.new(1, [2]),
	      Node.new(2, [4, 5]),
	      Node.new(3, [1, 4]),
	      Node.new(4, [5]),
	      Node.new(5, [])
	    ])
  s = TopologicalSearch.new
  graph.display
  foo = s.search(graph, graph.nodes.first)
  foo = foo.map { |x| x.key }
  puts foo.join(" | ")
end

class GameGraph < Graph
  def isSurrounded(node)
    if node.color = 'black' 
      opposite = 'white'
    elsif node.color = 'white'
      opposite = 'black'
    else return false	
    end
    print "checking to see if this node of color #{node.color} is ",
      "surrounded by color #{opposite}\n"
  end
end

def loadGraph(graph, name)
  f = File.open(name, "r")
  a = []
  f.each do |i|
    if i.slice(0).chr != '#'
      i = i.split(" ")
      n = Node.new(i[0] , i[2..(i.length-1)])
      c = i[1]
      n.color = 'black' if c == 'b' 
      n.color = 'white'if c == 'w'
      n.color = 'empty' if c == 'e' 
      a.push(n)
    end
  end
  graph.add(a)
end

graph = GameGraph.new
loadGraph(graph, "game.txt")
graph.display
