# point.rb
# Jeffrey M. Radcliffe
# Tue Feb 27, 2001  2:32 PM


#-------------------------------------------------------#
#                        HEY! TAs!                      #
# This is the code for problem set 4.                   #
# Ghostbuster problem is answered at the bottom.        #
#               -- Jeffrey                              #
#-------------------------------------------------------#

require "algorithm/sort"


# ye olde point class
class Point
  attr_reader :x, :y
  attr_writer :x, :y

  def initialize(x, y)
    @x = x ; @y = y
  end

  def -(other)
    Point.new(self.x - other.x, self.y - other.y)
  end

  def +(other)
    Point.new(self.x + other.x, self.y + other.y)
  end

  def cross(other)			# cross product
    (@x * other.y) - (other.x * @y)
  end

  def <=>(other)
    return -1 if self.y < other.y
    return  1 if self.y > other.y
    if self.y == other.y
      return -1 if self.x < other.x
      return  1
    end
    return  0
  end

  def to_s
    "(#{@x}, #{y})"
  end
end
#======================================================================
# line segment class
class LineSegment
  def initialize(point1, point2)
    @point1 = point1 ; @point2 = point2
  end

  def getBoundingBox
    p1 = Point.new([@point1.x, @point2.x].min,
		   [@point1.y, @point2.y].min)
    p2 = Point.new([@point1.x, @point2.x].max,
		   [@point1.y, @point2.y].max)
    return p1, p2
  end
  def intersects? (other)
    p1, p2 = self.getBoundingBox
    p3, p4 = other.getBoundingBox
    if ((p2.x >= p3.x) and (p4.x >= p1.x) and (p2.y >= p3.y) and (p4.y >= p1.y))
      a, b, c = (p3 - p1) , (p4 - p1), (p2 - p1)
      return true if (a.cross(c) == 0 or b.cross(c) == 0)
      return false
    end
  end

  def to_s
    "[Line Segment: #{@point1} #{@point2}]"
  end
end
#======================================================================
class ConvexHull
  # graham scan!!
  def graham(a)
    array = a.sort		# finds bottom y
    ref = array.delete_at(0)	
    sort(array,ref)		# sorts by polar coords
    array.unshift(ref)
    hull = [].push(array[0], array[1], array[2])
    for i in 3...array.length
      hull.pop while helper(hull[-2], hull[-1], array[i]) > 1
      hull.push(array[i])
    end
    return hull
  end

  def helper(a, b, c)
    p1, p2 = a - b, c - b
    (p1.x * p2.y) - (p2.x * p1.y)
  end

  # jarvis march!
  def jarvis(a)
    array = a.sort
    hull = [].push(array[0])
    chain = 'R'
    loop do
      ref = hull.last
      @minAngle = @minPoint = nil
      array.each do |point| 
	foo = point - ref
	angle = Math.atan2(foo.y, foo.x)
	angle += (2 * Math::PI) if (angle < 0) and (chain == 'R')
	if (point != ref) and ((@minPoint == nil) or (angle < @minAngle))
	  @minPoint, @minAngle = point, angle
	end
      end
      if chain == 'R'
	chain = 'L' if (@minAngle > Math::PI)
      else
	break if (@minPoint == hull.first)
      end
      hull.push(@minPoint)
    end
    return hull
  end

  # this is a slow sort! A quicksort could easily be substituted
  def sort(array, ref)
    for j in 0...array.length
      key = array[j]
      i = j - 1 
      while i >= 0 and helper(array[i], key, ref) > 0
	array[i+1] = array[i] ; i -= 1
      end
      array[i+1] = key
    end
  end
end
#======================================================================
# test of convex hull
class TestConvexHull
  def initialize
    # set up test array
    a = [-6, 0, -5, -2, -2.5, -5, 1, -2.5, -2, -1, 0, 0, 
      2.5, .5, -2.2, 2.2, -3.5, 1, -1.5, 1.5 , -4.5, 1.5]
    scan = ConvexHull.new
    points = makePoints(a)

    puts "Convex Hull test program\n------------------------"
    # run jarvis
    puts "Jarvis finds convex hull: #{scan.jarvis(points).join(' ')}"
    # run graham
    puts "Graham finds convex hull: #{scan.graham(points).join(' ')}"
  end

  def makePoints(a)
    temp = []
    i = 0
    while true
      temp << Point.new(a[i], a[i+1])
      i += 2 ; break if i >= a.length
    end
    temp
  end
end

# test of intersect
class TestIntersect
  def initialize
    segmentA = LineSegment.new(Point.new(0,0), Point.new(0,3))
    segmentB = LineSegment.new(Point.new(3,0), Point.new(0,3))
    segmentC = LineSegment.new(Point.new(4,4), Point.new(7,5))
    intersectTest(segmentA, segmentB)
    intersectTest(segmentC, segmentA)
  end
  
  def intersectTest(a, b)
    message = "intersects"
    message = "does not intersect" if (not a.intersects? b)
    puts "#{a} #{message} #{b}"
  end
end

# run the tests
TestConvexHull.new	
TestIntersect.new


#  Problem 2 (Ghostbusters)
#  a. An easy way to prove that there can be an even number of ghosts and
#  ghostbusters on one side of a line is as follows:

#  Set up the ghosts and ghostbusters, sorted as a convex hull Graham
#  Scan were about to start. O(n log n) 

#  As a ghostbuster is passed, increment the number
#  of ghostbusters. As a ghost is passed, increment the number of
#  ghosts. As soon as an even number (n) of ghosts and ghostbusters is
#  reached, draw a line using the point of ghost n and the point of
#  ghostbuster n.  O(n)

#  Due to the way of sorting, this will guarantee that there are an even
#  number of ghosts and ghostbusters on each side of the line.

#  b. Using this method, one can pair off ghostbusters. If on one side of
#  this line there is one ghost and one ghostbuster, connect the two with
#  a stream. On the other side of the line, repeat the process
#  recursively until all are paired off.



