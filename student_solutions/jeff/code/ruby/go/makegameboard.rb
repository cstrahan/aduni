require "algorithm/search"
require "algorithm/graph"
require "algorithm/common"
class Game
  def makeboard(width , height)
    puts "creating a game board #{width} x #{height}"
    i = 0
    file = "# game board adjacency array, by JMR\n" +
      "# size of board is ( #{width} x #{height} )\n" +
      "# node name | color | connections\n"
    0.upto(height - 1) do |y|
      0.upto(width - 1) do |x|
	array = []
	array.push(i, 'e') 
	array.push(i - height) if y != 0
	array.push(i - 1) if x != 0
	array.push(i + 1) if x != (width - 1)
	array.push(i + height) if y != (width - 1)
	file += array.join(' ') + "\n"
	i += 1
      end
    end
    return file
  end
  
  def setstones(graph, filename)
    f = File.open(filename, "r")
    a = []
    f.each do |line|
      next if line.slice(0) == "#"	# comment
      line.gsub!(/B/, 'black')
      line.gsub!(/W/, 'white')
      line.gsub!(/E/, 'empty')
      temp = line.split(' ')
      a += temp
    end
    # now we've got the board, so set the colors
    a.each_index { |x| graph.nodes[x].color = a[x] }
  end

  def loadGraph(graph, name)
    f = File.open(name, "r")
    a = []
    f.each do |line|
      next if line.slice(0).chr == '#'
      i = line.split(" ")
      temp = []
      2.upto(i.length - 1) { |j| temp.push(i[j].to_i) }
      node = Node.new(i[0].to_i , temp)
      c = i[1]
      node.color = 'black' if c == 'b' 
      node.color = 'white'if c == 'w'
      node.color = 'empty' if c == 'e' 
      a.push(node)
    end
    graph.add(a)
  end
end

class GameGraph < Graph
  def isSurrounded
    @nodes.each { |x| x.mark = false }
    @nodes.each do |x|
      path = []
      result = dfs(x, path) if x.mark == false
      p = path.collect { |x| x.color }
      path.each { |x| x.color = result }
    end
  end

  def dfs(node, path)
    return node.color if node.mark == true
    node.mark = true
    r = node.color
    if node.color == 'empty'
      path.push(node)
      colors = []
      node.connections.each do |x| 
	n = nodeAt(x)
	colors.push((dfs(n, path)))
      end
      c = colors.uniq 
      c.delete('empty') ; c.delete('grey')
      if c == ['black'] or c == ['white']
	r = c[0]
      else
	r = 'grey'
      end
    end
    return r  
  end
  
  def printboard
    @nodes.each_with_index do |x, i| 
      print x.color.slice(0).chr.swapcase , " " # !!
      print "\n" if (i + 1) % 5 == 0
    end
  end
  
  def score
    b = w = 0
    isSurrounded
    printboard
    @nodes.each do |x|
      color = x.color.slice(0).chr
      b += 1 if color == 'b'
      w += 1 if color == 'w'
    end
    puts
    puts "#{b} black stones, #{w} white stones"
    if b > w
      message = 'black is the victor!'
    elsif w > b
      message =  'white is the victor!'
    else
      message = 'it was a tie...'
    end
    puts message
  end
end

#---------------------------------------------------------------------

def main
  g = Game.new
  file = g.makeboard(5, 5)
  f = File.new("game.txt", "w")
  f.write(file)
  f.close

  graph = GameGraph.new
  g.loadGraph(graph, "game.txt")
  g.setstones(graph, "color.txt")
  graph.printboard 
  puts
  graph.score
end

main				# run the puppy!
