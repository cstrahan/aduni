#!/usr/bin/env ruby

require "ArrayMixin-0.3"
require 'algorithm/tree'

# ArrayUsingTree (and accoutrements)
# AUTHOR: Jeffrey Radcliffe <radcliffe@linuxfreemail.com>
# LICENSING: Same as that of Ruby

# This is a test attempt to interact with Mathieu Bouchard's
# ArrayMixin using a very basic tree structure. This has 
# few (if any) speed benefits.
class ArrayUsingTree
  include ArrayMixin

  def ArrayUsingTree.[](*v)
    # what to do here?
  end

  def initialize(*args)
    @tree = Tree.new		# tree data structure
    @array = []		        # an array of pointers
    # to tree nodes
    super(*args)
  end

  def length
    @array.length
  end

  def get(i)
    @array[i].key		# returns just the value
  end
  
  def get_many(i, n, *v)
    temp = []
    i.upto(n) { |j| temp.push(@array[j].key) }
    return temp
  end

  # put will create a tree node containing key v,
  # make a pointer in @array linking to the node,
  # and insert the node into the tree
  def put(i, v)
    node = TreeNode.new(v)
    @array[i] = node
    @tree.add(node)
    nil
  end

  def put_many(i, n, *v)
    puts "put_many called"
    counter = 0
    i.upto(n) { |j| put(j, v[counter]) ; counter += 1 }
  end

  def dup
    # code here ... 
  end
end 

def test
#  array = []
  array = ArrayUsingTree.new

  array[0] = 5
  array[1] = 7
  array[2] = 11
#  array[3,5] =   3, 2, 1	# weirdness still
#  array[3..5] = [3, 2, 1]	# ditto
  puts array.join(" --> ")
end

test	
