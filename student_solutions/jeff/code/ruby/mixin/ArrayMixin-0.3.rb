#!/usr/bin/env ruby

=begin
ArrayMixin 0.3

AUTHOR(S)

	Mathieu Bouchard <matju@cam.org>
	with some help from Dave Thomas

LICENSING

	Copyright (c) 2001 by Mathieu Bouchard
	Licensed under the same license as Ruby.

PURPOSE

1. To describe a minimal but reasonably efficient interface for
interacting with Array-like objects. 

2. To implement a set of default replacements for methods provided
in Array but not in the Array interface.

3. In short this can be seen as equivalent to Perl's tie.

4. This is also can be seen as a small part of an upcoming Ruby-in-Ruby. I
am not aware of anyone who is seriously involved in that sort of thing,
but who knows.

STRUCTURE

the ArrayMixin file contains those components:

1. module ArrayInterface
2. module ArrayMixin
3. class ArrayUsingArray (sample for test purposes)
4. class ArrayUsingArrayOfArray (sample; soon)

WARNING

	This software does not fully conform to its test cases.

TO DO

1. Fix all the things marked with #!@#$

2. Try it and write a useful program with it

3. Make sure TestArrayMixin.rb gets published

4. Ensure conformance with that test suite.
	Some Test Results (Most Recent First):
	* 0.3 Run: 66/66(268 asserts) Failures: 16 Errors: 17
	*     Run: 66/66(271 asserts) Failures: 13 Errors: 23
	*     Run: 66/66(220 asserts) Failures: 13 Errors: 31
	*     Run: 66/66(155 asserts) Failures: 11 Errors: 41
	*     Run: 66/66(163 asserts) Failures: 13 Errors: 39
	* 0.2 (none)

5. Categorize ArrayMixin methods.

6. Fix the possibly many bugs with the return values in ArrayMixin.

7. Fix the return types of methods that return a new Array. (decide
between the basic Array and self.type and other...)

8. Tune performance

9. Observer pattern

BUGS & QUIRKS

1. [dave] #map returns Array, #map! returns ArrayMixin

2. [matju] #replace does not accept ArrayMixin

3. [dave] #reject returns an ArrayMixin and #reject! too; this is not like
	#map, #map!

=end

# Minimal interface for using Arrays. Some new methods have been added
# because the equivalent in the Array class are too complex to be elegantly
# and simply defined by any Array provider. ' 

module ArrayInterface

  # length() returns the number of elements in the array.
  def length
    raise NotImplementedError
  end

  # get(i) returns contents of cell i.
  # i is the number of a cell. Such a number is always in the
  # range 0...length.
  # should raise IndexError when i is invalid.
  def get(i)
    raise NotImplementedError
  end

  # i is like in get(i). put(i,v) sets cell i to v s value.
  # the return value should not be used; but it should be nil.
  # note: never should change the array length.
  def put(i,v)
    raise NotImplementedError
  end

  # returns plain Array of elements i...(i+n)
  # i is like in get(i).
  # n.type <= Integer and n >= 0
  # i+n should be in 0..length
  def get_many(i,n)
    raise NotImplementedError
  end

  # deletes a range and inserts elements at a same point.
  # i is the insertion point.
  # i is in range 0..length
  # i+n is in range 0..length
  # deletes elements in i...(i+n)
  # inserts *v elements at i
  # the return value should not be used; but it should be nil.
  def put_many(i,n,*v)
    raise NotImplementedError
  end

  # returns an array of the same class as self.
  # if this cannot be done, raise NotImplementedError or
  # maybe TypeError or something else.
  def dup
    raise NotImplementedError
  end

  # Exceptions:
  # NotImplementedError: abstract method not overloaded
  # IndexError: invalid index
  #	(element index or insertion point or range length)
  # ArgumentError: other argument error, if applicable
  # May throw other exceptions when it makes sense.
end

module ArrayMixin
  include ArrayInterface
  include Enumerable

  # could have:
  #   include Comparable
  # well, maybe

  # category: base

  #!@#$ this won't really work (append_features ?)
  def ArrayMixin.[](*v)
    foo = new
    foo.replace v
  end

  def initialize(length=0,value=nil)
    # presuming the array is created by the implementor,
    # and is empty.

    if (length < 0)
      raise ArgumentError "negative array size"
    end

    (0...length).each {|i| put i,value }
  end

  def freeze
    #!@#$ write me
  end

  def frozen?
    #!@#$ write me
  end

  # category: assertions

  def arg_is_array(x)
    x.type <= Array || x.type <= ArrayMixin
  end

  # category: [] and []=

  # this whole category of methods is possibly a mess.

  def range_to_i_n(r)
    i,j = r.begin,r.end
    i += length if i < 0
    return nil if i < 0
    j += length if j < 0
    n = j-i + (r.exclude_end? ? 0 : 1)
    return (if n < 0 then nil else [i,n] end)
  end
  # private :range_to_i_n

  # ruby 1.6
  def at(i)
    i += length if i < 0
    return get(i) if (0...length) === i
    return nil
  end

  def at_range(i)
    foo = range_to_i_n(i)
    return (if foo.nil? then nil
	    else get_many foo[0],foo[1] end)
  end
  # private :at_range

  def at_with_length(i,n)
    return nil unless (0...length) === i && n >= 0
    if i+n > length then n = length - i end
    return [] if n <= 0
    return get_many(i,n)
  end
  # private :at_with_length

  def [](i,*args)
    if Range === i then
      if args.length > 0 then
	raise ArgumentError,
	  "wrong # of arguments (%d for 1)" % [
	  args.length + 1]
      end
      return at_range(i)
    end
    if args.length == 0 then return at(i) end
    if args.length > 1 then
      raise ArgumentError,
	"wrong # of arguments (%d for 2)" % args.length
    end
    return at_with_length(i,l)
  end

  def at_put(i,v)
    if i < 0 then i += length end
    if i < 0 then
      return nil
    else
      put i,v
      return get i
    end
  end
  # private :at_put

  def at_range_put(i,v)
    foo = range_to_i_n(i)
    if foo.nil? then
      return nil
    else
      v = v.to_a
      put_many foo[0],foo[1],*v
      return v
    end
  end
  # private :at_range_put

  def at_with_length_put(i,n,v)
#    v = args[0].to_a		# where is args coming from? [JMR]
#    put_many i,n,*v
    v = *v.to_a
    put_many (i, n, v)
    v
  end
  # private :at_with_length_put

  def []=(i,b,*args)
    puts "i = #{i} b = #{b} args = #{args}"
    if Range === i then
      if args.length > 0 then
	raise ArgumentError,
	  "wrong # of arguments (%d for 2)" % [
	  args.length + 2]
      end
      at_range_put(i,b)
    end
    if args.length == 0 then
      at_put(i, b)		# orig: at_put(i,v)
    end
    if args.length > 1 then
      raise ArgumentError,
	"wrong # of arguments (%d for 3)" % [
	args.length+2]
    end
    #    at_with_length_put(i,b,args[0]) # why is this called here?
  end

  def first; self[0]; end
  def last; self[-1]; end

  # category: miscellaneous operations

  def *(n)
    foo = []
    n.times { foo.put_many length,0,*self }
  end

  def <=>(other)
    #		STDERR.print 'self: ', self.inspect
    #		STDERR.print 'other: ', other.inspect
    c = 0
    n = [length,other.length].min
    length.times {|i|
      a = get(i)
      b = other[i]
      c = a <=> b
      break if c != 0
    }
    #		STDERR.print "RETURNING ", c, "\n"
    return (if c==0 then length <=> other.length else c end)
  end

  def eql?(other)
    # can't use (self <=> other) == 0
    length.times {|i|
      return false if not get(i).eql?(other.get(i))
    }
    true
  end

  #!@#$ should it handle loops other than selfloops?
  def ==(other)
    return false unless arg_is_array(other)

    #=begin
    # can't use (self <=> other) == 0
    length.times {|i|
      a = get(i)
      b = other[i]
      return false unless a.id==b.id || a==b
    }
    true
    #=end
  end
  alias === ==

    def eql?(other)
      (self <=> other) == 0
    end

    def clear
      put_many 0,length
      self
    end

    def concat(other)
      put_many length,0,*other
      self
    end		

    def delete(v,&proc)
      a = length
      delete_if {|x| x == v}
      if a == length then
	proc && proc.call
      else
	v
      end
    end

    def delete_at(i)
      foo = get(i)
      put_many(i,1)
      foo
    end

    def delete_if(&proc)
      foo = []
      each {|x|
	foo << x unless proc.call(x)
      }
      replace foo
    end
    def reject!(&proc)
      foo = length
      delete_if(&proc)
      foo == length ? nil : self
    end

    def each(&proc)
      length.times {|i|
	proc.call get(i)
      }
      self
    end

    def each_index(&proc)
      (0...length).each(&proc)
    end

    def empty?
      length == 0
    end

    def fill(v,start=0,length=nil)
      #!@#$ write me / fix me
    end

    def filter(&proc)
      (0...length).each {|i| put i, proc.call(get i) }
    end

    def flatten!
      i=nil
      modified = false
      flattening = []
      each_with_index {|x,i|
	# x = get i
	if x.type == Array then
	  if self == x then
	    x = nil
	  else
	    id = ary2.id
	    if flattening.include?(id) then
	      raise "tried to flatten recursive array"
	    end
	    flattening << id
	  end
	  put_many i, 1, *x
	  modified = true
	  redo
	end
      }
      return (if modified then nil else self end)
    end
    def flatten; dup.flatten!; end

    def include?(v)
      [] != find_all {|x| x == v }
    end

    def index(v)
      each_with_index {|x,i|
	return i if x==v
      }
      nil
    end

    # this may be a good candidate for adding to ArrayInterface
    def indices(*i)
      i.collect {|x| get x }
    end
    alias indexes indices

    #!@#$ this is O(n**2). Make it O(n log n)
    def join(sep=$,)
      return "" if length==0
#      foo = get 0
      foo = get(0).to_s
      (1...length).each {|i|
	foo += sep		# isn't working with ArrayAsTree [JMR]
	foo += get(i).to_s      # orig: foo += get(i) [JMR]
      }
      foo
    end

    # do not use alias here.
    def size
      length
    end

    def pack(template)
      #!@#$ write me (?)
      get_many(0,length).pack(template)
    end

    def replace(other)
      put_many 0,length,*other
      self
    end

    def reverse!
      foo = []
      reverse_each {|x| foo << x }
      replace foo
    end

    def reverse_each(&proc)
      length.times {|i|
	proc.call get(length-1-i)
      }
      self
    end

    def rindex(v)
      #!@#$ write me
    end

    # ruby 1.6
    alias slice []
    def slice!(*args)
      replace slice(*args)
    end

    def to_a
      # can't say just "self"
      get_many 0,length
      # self
    end

    def to_ary
      # can't say just "self"
      get_many 0,length
      # self
    end

    def to_s
      join
    end

    def hash
      h = 0
      each {|v|
	h = (h<<1) | (h<0 ? 1 : 0)
	h &= 2**32-1
	n = v.hash
	n &= 2**32-1
	h ^ n
      }
      h
    end

    def inspect
      #!@#$ should prepend class name ?
      "[%s]" % [map {|x| x.inspect }.join ', ']
    end

    #!@#$ what do i do about #clone ?

    # category: stack/queue

    def push(*v)
      # Array#push does this check (may be superfluous)
      raise ArgumentError "wrong # of arguments(at least 1)" if v.length == 0
      put_many length,0,*v; self
    end
    def pop; foo = get(length-1); put_many length-1,1; foo; end
    def shift; foo = get(0); put_many 0,1; foo; end
    def unshift(*v); put_many 0,0,*v; self; end

    alias << push

    # category: dup/replace wrappers for some methods

    def compact; dup.compact!; end
    def reverse; dup.reverse!; end
    def sort(&proc); dup.sort!(&proc); end
    def reject(&proc); dup.reject!(&proc); end
    def map!(&proc); replace map(&proc); end
    def collect!(&proc); replace collect(&proc); end

    # category: shortcuts involving nil

    def compact!; reject! {|x| x.nil? }; end
    def nitems; find_all {|x| x != nil }.length; end

    # category: array of [key,value] arrays

    def assoc(key)
      #!@#$ write me
    end

    def rassoc(v)
      #!@#$ write me
    end

    # category: sorting

    # &proc must be present
    # range.exclude_end? == false
    def sort_partition_range!(range,&proc)
      pivot = get(range.end)
      foo = get_many(range.start,range.end+1-range.start)
      bar = find_all {|x| x < pivot }
      pivot_pos = bar.length
      bar << find_all {|x| x == pivot }
      bar << find_all {|x| x  > pivot }
      put_many range.start,range.end+1-range.start,*bar
      return pivot_pos
    end

    # &proc must be present
    # range.exclude_end? == false
    def sort_range!(range,&proc)
      if range.end - range.start < 0 then return nil end
      pivot = sort_partition_range!(range,&proc)
      a = sort_range!(range.start .. pivot-1)
      b = sort_range!(pivot+1 .. range_end)
      a || b
    end

    # private :sort_partition_range!
    # private :sort_range!

    #!@#$ this _HAS_ to work
    def sort!(&proc)
      proc ||= proc{|a,b|a<=>b}
      sort_range!(0..length-1,&proc)
    end

    # category: set operations

    def +(other)
      foo = dup
      foo.put_many length,0,*other
      foo
    end

    def -(other)
      t = {}
      each {|x| t[x]=1 }
      other.each {|x| t.delete x }
      r
    end

    # as seen in [ruby-talk:6756]
    def &(other)
      t = {}
      r = []
      other.each {|x| t[x]=1 }
      uniq.each  {|x| r<<x if t.has_key? x }
      r
    end

    # as seen in [ruby-talk:6756]
    def |(other)
      t = {}
      r = []
      (self+other).each {|x| r<<x unless t.has_key?(x); t[x]=1 }
      r
    end

    # as seen in [ruby-talk:6756]
    def uniq; self|[] end
    def uniq!; replace self|[]; end

    # category: temporary hacks

    def collect(&proc)
      foo = []
      each {|x| foo << proc.call(x) }
      foo
    end
    alias map collect
  end

class ArrayUsingArray
  include ArrayMixin

  #!@#$ temporary hack
  def ArrayUsingArray.[](*v)
    foo = new
    foo.replace v
  end

  #	alias initialize_super initialize
  def initialize(*args)
    # this makes array smaller than just "=[]".
    @array = [nil]
    @array.pop
    #		# do what ArrayMixin wants us to do.
    #		initialize_super(*args)
    super(*args)
  end
  #	private initialize_super
  def length
    @array.length
  end
  def get(i)
    @array[i]
  end
  def put(i,v)
    @array[i]=v
    nil
  end
  def get_many(i,n)
    @array[i,n]
  end
  def put_many(i,n,*v)
    @array[i,n]=v
    nil
  end
  def dup
    foo = type.new
    foo.replace self
  end
end

$c = ArrayUsingArray
$a = $c[]
$r = []
