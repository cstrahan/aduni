###Bryon Gill
###Problem set 1 - algorithms
###Output from running the code is located at end of file

import time

#

## 1.
## this array:
##    12 10 3 37 57 2 23 99
## a (bubble sort):
##     10 3 12 37 2 23 57 99
##     3 10 12 2 23 37 57 99
##     3 10 2 12 23 37 57 99

## b (insertion sort):
##     10 12 3 37 57 2 23 99
##     3 10 12 37 57 2 23 99
##     3 10 12 37 57 2 23 99

## c (partition quicksort):
##     partitioned on 12:
##     10 3 2
##     37 57 23 99

## d (first iteration outer radix sort):
##     10 12 02 03 23 37 57 99

## e (mergesort):
##     12 10 3 37 --- 57 2 23 99
##     12 10 --- 3 37 ------ 57 2 --- 23 99
##     12 --- 10 ------ 3 --- 37 ------------ 57 --- 2 ------ 23 --- 99
##     10 12 --- 3 37 ------ 2 57 --- 23 99
##     3 10 12 37 --- 2 23 57 99
##     2 3 10 12 23 37 57 99

## 2.
##take array a, return it sorted
def maxsort(a):
    for last_index in  range((len(a) -1), 0, -1):
        finished = 1
        last_a = 0 ## I'm introducting the assumption that the numbers are all positive.
                   ## There are more complicated ways of doing this, but I'm just interested in making it work.
        max = last_index
        for i in range(0, last_index):####cycle up through the list
            if a[i] > last_a:
                 max = i
            last_a = a[i]

        if a[max] > a[last_index] :
            tmp = a[max]
            a[max] = a[last_index]
            a[last_index] = tmp
    print 'all done!  Here is the max sort result:'
    print a
    return a

test_lst = [12, 10, 3, 37, 57, 2, 23, 9] ### an array to test the function on
maxsort(test_lst)                        ### a call to our function

#This is a double loop, requiring one pass through to find the max and another to test and replace if necessary, making it O(n^2)

#3a.  The binary search.
# first perform a binary search to find the end of the positive integers by testing the indices at successively larger powers of 2 to
# see if they are equal to 0.  If it is, back up half the distance from the last jump and keep doing that until you find the index of
# the last non-zero integer, this is the length of the array.
#
# then perform an ordinary binary search on the full length of the array for x.  Since each of these operations are binary searches on
# which are theta(log n), the full operation is a constant multiple of that procedure.
#3b. Insertion sort
#
#You could treat the portion of the list sorted so far as an independent array to be binary searched, but the insertion time will be
# O(n), so the gain in time on the search is swamped by the required time for insertion afterwards.



#4. A better quicksort
# n^2 time for quicksort happens when the initial case is either the max or min of the list.  To guarantee a worst case nlogn,
#we need to guarantee somehow that the numbers are separated into two relatively even piles.  The way to do this is to
#divide the list up into (n/5) groups of 5 (plus possibly one with less than 5) elements.   Find the median of each of these with insertion sort,
#then find the median of these medians.  This is close enough to the median of the whole list to guarantee a sufficiently well divided
#quicksort to get at worst nlogn time.

#5.
#
#a.  This is theta nk because insertion sort works in n^2 time, and since the length of each is
#    k  that's k^2 time, and we do that n/k times which comes out to nk time.

#b. The sublists can be merged in theta nlg(n/k) worst case time since
#   since the ordinary time complexity of merge sort is nlg(n).  The second
#   n represents the number of sublists to be merged, but this procedure
#   allows the mergesort to "bottom out" earlier than normal, that is,
#   when the merge gets to subarrays of length k rather than length 1.

#c. lg(n)

#d. This requires some trial and error twiddling the constant factors and testing of the results, but is bounded between 1 and lg(n).

#6 Building Heaps
#a. Which is which?
# The recursive method which pushes up corresponds to the iterative push down
# method, since they both build the tree in the same way once the recursion
# bottoms out.
#
#b. ---############
#T(n) = T(n-1) + lg(n)
#This one is theta(n^2)
#
#T(n) = 2T(n/2) + lg(n)
#This one is theta(nlg(n))
#c. ---*****************************
##################################################################################################
###heap utility functions
###find the parent of a node.  Relies on rounding in integer division.
def parent(i):
    if i <= 0:
        return 0
    else:
        return ((i-1)/2)

###find the children of a node
def left(i):
    return ((2 * i) + 1)

def right(i):
    return ((2 * i) + 2)

####################################################################################################
#heap_down: creates a heap starting from the leaves, and building then merging small heaps together.
def heap_down(a):                        ### the array in will be called a
    for i in range((len(a) - 1), 0, -1): #start at end of array
        #print "i is now"
        #print i
        if a[i] > a[parent(i)]:
            push_down(a, parent(i))          # push smaller parents down recursively
    return a

###push_down: does all the work for heap down.
def push_down(a, i):
    #print 'pushing down on ' + str(i) + ' now... \r'
    try:                   ###Note: this try block is necessary to avoid out of range errors, I fear it will mess up my time analysis
        l = a[left(i)]
    except: l = ''

    try:
        r = a[right(i)]
    except:
        r = ''

    if l != '':
        if l > a[i]:
            if r == '':
                a[left(i)] = a[i] # if r is nil definitely swap l with its parent, no need to push any further since r was 'nil'
                a[i] = l
            else:
                if l > r:         # if r is not nil but l is larger than r, then still swap l with parent.
                    a[left(i)] = a[i]
                    a[i] = l
                    push_down(a, left(i)) #we swapped, so keep pushing
                else:
                    a[right(i)] = a[i] # if r is bigger than l and l is bigger than parent, swap r and parent.
                    a[i] = r
                    push_down(a, right(i)) # we swapped, keep pushing
        elif r != '':  ###in case it's not clear, this else is on the l>a[i] test.
            if r > a[i]:
                a[right(i)] = a[i]
                a[i] = r
                push_down(a, right(i))
    return a

#####################################################################################################
#heap_up - build a heap by inserting new values in the last position of the heap and bubbling them up
def heap_up(a):
    for i in range(0, len(a), 1):  # not sure why the length isn't giving me out of bounds here- # 15 should break it!
        #print "pushing up:"
        #print a[i]
        #print '\r'
        push_up(a, i)
        #print_tree(a)
        #print '\r'
        #print a
        #print '\r'
    return a
###
#push_up: does the work for heap up
def push_up(a, i):
    if a[i] > a[parent(i)]:
        #string_to_print = "switching " + str(a[i]) + " in position " + str(i) + " with " + str(a[parent(i)]) + " in position " + str(parent(i))
        #print string_to_print
        #print '\r'
        temp = a[i]
        a[i] = a[parent(i)]
        a[parent(i)] = temp
        #print_tree(a)
        push_up(a, parent(i))
    else:
        return a


#####################################################################################################################################
#print_tree: a function that displays a tree.  It looks like the tree is slumped over to the left, but hey- this isn't horticulture.
#maybe someday I'll center the parents over their children.
def print_tree(tree_in):
    end_of_row = 0 # we'll use this momentarily
    row_exp = 1
    string_to_print = ""
    for i in range(len(tree_in)):
        string_to_print = string_to_print + str(tree_in[i]) + ' '
        if i == end_of_row:
            print string_to_print
            string_to_print = ""
            row_exp = row_exp * 2
            end_of_row = end_of_row + row_exp
    print string_to_print
    return

################################################################################################################
#timing function: this takes a function, runs it an arbitrary number of times, and reports how long it took
def time_function(num_times, func):
    total = 0.0
    for x in range(num_times):
        starttime = time.time()
        apply(func)
        stoptime = time.time()
        elapsed = stoptime - starttime
        total = total + elapsed
    print "running %s %d times took %.3f seconds" % (func.__name__, num_times, total)

###the functions that go into time_function can't take arguments, so here's the setup for the tests:
def heap_up_test():
    heap_up([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15])
def heap_down_test():
    heap_down([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15])

time_function(200, heap_up_test)
time_function(200, heap_down_test)

### it seems that heap_down actually took longer, but I suspect that this is because
### it has so many extra tests to avoid out of range exceptions, despite our expectation that the
### algorithm that starts with the leaves would be faster.
### such is engineering.

test_lst2 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15] ### an array to test the function on
print '\r'
print "--- here's the unsorted tree: \r"
print_tree(test_lst2)
print '\r'
print "here's the heap_down heap: \r"
print_tree(heap_down(test_lst2))
test_lst2 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15] ### reset it for the next function call
print '\r'
print "here's the heap_up heap: \r"
print_tree(heap_up(test_lst2))
print '\r'


#7.
#a) this process is nlgn.

#b) cut off the heap at the row where the kth largest lives and copy the contents into a new heap.
#
# (5,4,3,2,1)
#finding the 3 largest would require us to create a new heap
#
#(5,4,3)
#
# then test the heap propertyonly on those items in the heap
#
#here's a problem though: what if it's in the middle of a row?  For instance,
#(7, 5, 6, 1, 2, 3, 4)
# if we want the fifth largest we would take
# (7,5,6,1) and we'd end up getting 1, which would be incorrect.  So we either have to have a
# method for breaking rows, or else we need to just accept the inefficiency of letting k be the next
# row ending for purposes of cutting off the bottom of the tree.
#
# the problem is that any procedure that analyzes the cutoff row and returns the top values from it sorted may be more time intensive in the worst case
# than simply finding the next row cutoff and possibly being forced to do a few extra heapifications.

#c)
###################################################################################################
###heap_klargest: this code rebuilds the whole heap every time.
def heap_klargest(a, k):
    heap_to_cut = heap_up(a)
    for i in range(k):
        heap_to_cut = heap_up(heap_to_cut[1:])
    return heap_to_cut[0]
###fast_heap_klargest: this code cuts off at the nearest row end
def fast_heap_klargest(a, k):
    new_heap = heap_up(a)
    row_end = 0
    row_length = 1
    while k > row_end:
        row_length = row_length * 2
        row_end = row_end + row_length
    try:
        new_heap = new_heap[:rowend]
    except:
        new_heap = new_heap         ###if the index of the rowend is out of range, then we just use the whole list.
    return heap_klargest(new_heap, k)

print heap_klargest(test_lst2, 5)
print fast_heap_klargest(test_lst2, 5)
def test_fast_klarg():
    fast_heap_klargest([1,2,3,4,5,6,7,8,9,10], 4)
def test_klarg():
    heap_klargest([1,2,3,4,5,6,7,8,9,10], 4)
def test_long_fast_klarg():
    fast_heap_klargest([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,41,42,113,43,45,46,47,48,49, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000], 4)
def test_long_klarg():
    heap_klargest([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,41,42,113,43,45,46,47,48,49, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000], 4)


time_function(200, test_fast_klarg)
time_function(200, test_klarg)
time_function(200, test_long_fast_klarg)
time_function(200, test_long_klarg)

#8.
#?????
#

#9. Strategies:
# Either all three numbers must be 0, or at least one must be negative and at least one positive.
# The selected number with the largest absolute value must have the opposite sign from the other two.
#
#First test for a zero in all three arrays, failing that
#
# a = next available value in the first array
# b = next available value in next array such that 0 <= abs(a) - abs(b)
#
#   check if the the (a+b) * -1 is in the last array.
#
#   repeat for the rest of the values in the second array.
#
# repeat for the each eligible value in the second array.
#
# repeat for the other two arrays.
#
# This is getting to the idea that if you can figure out what you need to see to get the answer you need, you only
# need to look at some of the values.  If you can sort the individual arrays (which will take nlogn time each)
# then you can do a binary search to find the set of eligible values rather than doing comparisons on each one.

## Here's the output when this file is run as a  python module:

## [bgill@localhost Desktop]$ python ps1.py
## all done!  Here is the max sort result:
## [2, 3, 9, 10, 12, 37, 57, 23]
## running heap_up_test 200 times took 0.129 seconds
## running heap_down_test 200 times took 0.165 seconds

## --- here's the unsorted tree: 
## 1 
## 2 3 
## 4 5 6 7 
## 8 9 10 11 12 13 14 15 


## here's the heap_down heap: 
## 15 
## 11 14 
## 9 10 13 7 
## 8 4 2 5 12 6 3 1 


## here's the heap_up heap: 
## 15 
## 10 14 
## 7 9 11 13 
## 1 4 3 8 2 6 5 12 


## 10
## 10
## running test_fast_klarg 200 times took 0.190 seconds
## running test_klarg 200 times took 0.135 seconds
## running test_long_fast_klarg 200 times took 1.683 seconds
## running test_long_klarg 200 times took 1.573 seconds
## [bgill@localhost Desktop]$ 


















