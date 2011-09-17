# search.rb
# Jeffrey Radcliffe
# Thu Feb  8, 2001 11:51 AM


class AbstractSearch
  def search(graph, node)
    @startOrder = @endOrder = 1	
    @path = []
    @graph = graph		# make a copy
    graph.nodes.each do |i| 
      i.mark = false ; i.parent = nil
    end
    searchloop(node, nil)
    return @path
  end
  def searchloop(node, parent)
    # not putting anything here!
  end
end

class DepthFirstSearch < AbstractSearch
  def searchloop(node, parent)
    return if node.mark == true
    node.mark = true
    node.parent = parent
    @path.push(node)
    node.startOrder = @startOrder ; @startOrder += 1
    if node.connections != nil
      node.connections.each {|x| searchloop(@graph.nodes[x], node) }
    end
    node.endOrder = @endOrder ; @endOrder += 1
  end
end

class BreadthFirstSearch < AbstractSearch
  def searchloop(node)
    # code here!
  end
end

class TopologicalSearch
  def search(graph, node)
    s = DepthFirstSearch.new
    result = s.search(graph, node)
    # format results
    return result
  end
end

      

