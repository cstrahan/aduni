# a boring tree
class Tree
  attr_reader :size, :root
  attr_writer :root

  def initialize
    @size = 0
  end

  def walk
    inorderwalk(@root)
  end

  def inorderwalk(x)
    if x != nil
      inorderwalk(x.left)
      print x.key, " " if x.key != nil
      inorderwalk(x.right)
    end
  end

  def walk2
    otherwalk(@root)
  end

  def otherwalk(x)  
    if x != nil
      print "#{x.key}[#{x.color}] "
      otherwalk(x.left)
     otherwalk(x.right)
    end
  end

  def max
    x = @root ; x = x.right while x.right != nil
    return x
  end

  def min
    x = @root ; x = x.left while x.left != nil
    return x
  end
  
  def add(z)
    x = @root ; y = nil
    while x != nil
      y = x
      if z.key < x.key
	x = x.left
      else 
	x = x.right
      end
    end
    z.parent = y
    if y == nil 
      @root = z
    elsif z.key < y.key
      y.left = z
    else
      y.right = z
    end
    @size += 1
  end

  def delete(z)
    # code here
  end

  def successor(z)
    # code here
  end
end

class RBTree < Tree
  def left_rotate(x)
    y = x.right
    x.right = y.left
    if y.left != nil
      y.left.parent = x
    end
    y.parent = x.parent
    if x.parent == nil
      @root = y
    elsif x == x.parent.left
      x.parent.left = y
    else
      x.parent.right = y
    end
    y.left = x
    x.parent = y
  end

  def right_rotate(x)		# think this works
    y = x.parent
    y.left = x.right
    x.right = y
    temp = y.parent
    y.parent = x.parent 
    x.parent = temp
    if y.left != nil
      y.left.parent = y
    end
    @root = x if x.parent == nil
  end
  
  def insert(x)
    # ugh! come back to this later...
  end
end



tree = RBTree.new 
array = [7, 4, 11, 3, 2, 6, 9, 18, 14, 12, 19, 17, 22, 20]
array.each {|i| 
  tree.insert(Node.new(i))
} 


#puts tree.walk2
#tree.right_rotate(tree.root.left)
#tree.left_rotate(tree.root.right.right)
puts tree.walk2
