# go.rb

require "algorithm/search"
require "algorithm/graph"
require "algorithm/common"

class GoNode < Node
  def report
    puts "Yo! my name is node #{@key}"
  end
end

class GoBoard
  attr_reader :loc

  NOT_SEEN = 0
  BEGUN = 1
  FINISHED = 2
  
  def initialize
    # create the data structure
    @board = Graph.new
    @loc = []
  end
  
  # this reads in formation according to problem set 3, number 1
  def load(filename)
    stones = []
    File.open(filename, "r").each do |line|
      next if line =~ /^#/
      unless line =~ /^$/
	if line =~ /^size/
	  @size = line.split('=')[1].chomp.to_i
	  print "size is #{@size}\n"
	elsif line =~ /^move/
	  @move = line.split('=')[1].chomp
	  puts "moving player is #{@move}"
	else
	  stones += line.chomp.split(" ")
	end
      end
    end
    # create a graph of the right size...
    loadGraph(makeBoard(stones))
  end

  def checkLegalMoves(color)
    puts "checking legal moves for #{color}"
    @move = color
    @move == 'B' ? @opponent = 'W' : @opponent = 'B'
    recheck = [] ; clearMarks
    @board.nodes.each do |x|
      path = []
      search(x, path, @move, @opponent) if x.mark == NOT_SEEN
      p = path.collect { |x| x.color }
      if p.length == 1		
	recheck.push(x) ; result = '?'
      else
	result = '0'
      end
      path.each { |x| x.color = result if x.color != @move }
    end
    
    # recheck those popped onto the recheck list
    # puts "There are #{recheck.length} possible suicides."
    recheck.each do |node|
    clearMarks
      flag = false  ; node.color = @move
      node.connections.each do |child|
	temp = child.color ; child.color = '0'
	clearMarks
	path = []
	search(child, path, @opponent, @move)
	p = path.collect { |x| x.color }
	if p.length == 1
	  flag = true
	end
	child.color = temp
	break if flag == true
      end
      flag == true ? node.color = '0' : node.color = 'x'
    end
  end

  def display
    @board.nodes.each_with_index do |x, i| 
      print x.color, " "
      print "\n" if (i + 1) % @size == 0
    end
  end

  :private
  def search(node, path, move, opponent)
    return node.color if 
      node.mark != NOT_SEEN or
      node.color == opponent
    path.push(node) unless
      node.mark == BEGUN or
      node.color == move
    node.mark = BEGUN
    node.connections.each do |x|
      search(x, path, move, opponent)
    end
    node.mark = FINISHED
  end

  def clearMarks
    @board.nodes.each { |x| x.mark = NOT_SEEN }
  end

  def makeBoard(stones)
    i = 0
    file = "# game board adjacency array, by JMR\n" +
      "# size of board is ( #{@size} x #{@size} )\n" +
      "# node name | color | connections\n"
    for y in 0...@size
      for x in 0...@size
	array = []
	array << i << stones[i]
	array << (i - @size) if y != 0
	array << (i - 1) if x != 0
	array << (i + 1) if x != (@size - 1)
	array << (i + @size) if y != (@size - 1)
	file += array.join(' ') + "\n"
	i += 1
      end
    end
    return file
  end

  def loadGraph(string)
    a = []
    string.each do |line|
      next if line =~ /^#/
      i = line.split(" ")
      temp = []
      2.upto(i.length - 1) { |j| temp.push(i[j].to_i) }
      node = GoNode.new(i[0].to_i , temp)
      node.color = i[1]
      a.push(node)
    end
    @board.add(a)

    # convert node numbers to direct pointers
    @board.nodes.each_with_index do |i, index|
      @loc.push([])
      i.connections.each_index do |j|
	i.connections[j] = @board.nodeAt(i.connections[j])
      end
    end

    counter = 0
    for y in 0...@size
      @loc.push([])
      for x in 0...@size
	@loc[x][y] = @board.nodes[counter] ; counter += 1
      end
    end
  end
end

#---------------------------------------------------------------------

def main
  puts "initializing board..."
  go = GoBoard.new
  go.load("input.txt")
  puts "starting search..."
  puts "search finished. Possible moves are:"
  go.display
end

# main				# run the puppy!
