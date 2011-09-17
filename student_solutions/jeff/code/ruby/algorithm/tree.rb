# tree.rb - tree stuff

module Tree
  require 'algorithm/common' # common algorithm stuff

  #---------------------------------------------------------
  # WALKS
  #---------------------------------------------------------
  def inorderWalk(tree)
    inWalk(tree, tree.root)
  end
  def inWalk(tree, node)
    if node != nil
      inWalk(tree, node.left)
      print "#{node.key} "    if node.key != nil
      print "[#{node.color}]" if node.color != nil
      inWalk(tree, node.right)
    end
  end

  def preorderWalk(tree)
    preWalk(tree, tree.root)
  end
  def preWalk(tree, node)
    if node != nil
      print "#{node.key} "    if node.key != nil
      print "[#{node.color}]" if node.color != nil
      preWalk(tree, node.left)
      preWalk(tree, node.right)
    end
  end

  def postorderWalk(tree)
    postWalk(tree.root)
  end
  def postWalk(tree, node)
    if node != nil
      preWalk(tree, node.left)
      preWalk(tree, node.right)
      print "#{node.key} "    if node.key != nil
      print "[#{node.color}]" if node.color != nil
    end
  end

  #---------------------------------------------------------
  # SEARCH, MAX, MIN
  #---------------------------------------------------------
  def search(node, key)
    while node != nil and key != node.key
      key < node.key ? node = node.left : node = node.right 
    end
    return node
  end

  def max
    return maximum(@root)
  end

  def maximum(node)
    node = node.right while node.right != nil
    return node
  end
  def min
    return minimum(@root)
  end

  def minimum(node)
    node = node.left while node.left != nil
    return node
  end

  def successor(x)
    return minimum(x.right) if x.right != nil
    y = x.parent
    while y != nil and x == y.right
      x = y ; y = y.parent
    end
    return y
  end
end # end of module Tree

#------------------------------------------------
#           TREE DATATYPE CLASSES FOLLOW
#------------------------------------------------
# a simple, basic tree
class GenericTree
  attr_reader :size, :root
  attr_writer :root
  
  include Tree

  def initialize
    @size = 0
  end
  
  def insert(z)
    x = @root ; y = nil
    while x != nil
      y = x
      if z.key < x.key
	x = x.left
      else x = x.right
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
    nil
  end

  def delete(z)
    if z.left == nil or z.right == nil
      y = z
    else
      y = successor(z)
    end
    if y.left != nil
      x = y.left
    else
      x = y.right
    end
    if x != nil
      x.parent = y.parent
    end
    if y.parent == nil
      @root = x
    else
      if y == y.parent.left
	y.parent.left = x
      else
	y.parent.right = x
      end
    end
    if y != z
      z.key = y.key
    end
    @size -= 1
    return y
  end
end

# a red-black tree. Much fancier, maintains a balance as it goes
class RBTree < GenericTree
  
  def insert(node)
    # code me
  end

  def delete(node)
    # code me
  end

  :private
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

  def right_rotate(x)
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
end

# An AVL-Tree (eventually)
class AVLTree < GenericTree
  def insert(node)
    # code me
  end

  def delete(node)
    # code me
  end
end

#----------------------------------------------------------
# TEST CLASS 
# ---------------------------------------------------------
def runTest
  tree = GenericTree.new 
  array = [40, 20,  60, 10, 30, 50, 80, 70, 90, 100]
  array.each {|i| tree.insert(TreeNode.new(i)) }

  [60, 70, 90, 20, 40, 10, 30, 50, 80, 100].each do |i|
    tree.delete(tree.search(tree.root, i))
    puts tree.preorderWalk(tree)
  end
end

runTest

