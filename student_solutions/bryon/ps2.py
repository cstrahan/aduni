##############################################################################80

###Algorithms ps2 code
###

import sys

##############################################################################80
###setup: DFS code...
class vertex: 
    def set_parent(self, new_parent):
        self.parent = new_parent
    def __init__(self):
        self.adjacent = []
        self.color = 'white'
        self.parent = 'nil'
        self.gamestate = 'e'
    def add_adjacent(self, new_adjacent): #list of adjacent vertices
        self.adjacent.append(new_adjacent)
    def set_color(self, new_color):
        self.color = new_color
    def set_discover_time(self, new_discover_time):
        self.discover_time = new_discover_time
    def set_finish_time(self, new_finish_time):
        self.finish_time = new_finish_time
    def set_name(self, new_name):
        self.name = new_name
    def set_gamestate(self, state):
        self.gamestate = state

##############################################################################80
####problem 4

def in_degree(graph, vertex):  ####cycle through the lists
    degree = 0
    for from_vertex in graph:
        if vertex in from_vertex.adjacent:
            degree = degree + 1
    return degree 

def finder(g): ###returns the index of the first item with indegree 0
    for i in range(0, len(g)):
        my_in_degree = in_degree(g, g[i])
        print 'in_degree of ' + g[i].name + ' is ' + str(my_in_degree)
        if my_in_degree == 0:
            return i
    print 'something has gone horribly wrong, this should never print'
    
def topo2(g):
    answer = []
    g_minus = g[:] #make a local copy of g so we don't screw g up 2 badly.
    while g_minus != []:
        next_vertex = finder(g_minus)
        answer.append(g_minus[next_vertex].name)
        del g_minus[next_vertex]
    return answer
##############################################################################80
###Problem 5
###Functions to help us treat lists like stacks:
def pop(stack):
    try:
        last_index = len(stack) - 1
        del stack[last_index]
    except:
        print 'the stack seems to be empty'
        
def push(stack, new_item):
    stack.append(new_item)
    #print 'the stack is now '
    #for i in stack:
    #    print i.name

##############################################################################80
###The actual cycle-finding code

def dfs_cycle_killer(g): #cycle killer, qu'est-ce que c'est?
    path = []            #initialize the stack
    for vertex in g:     #initialize everything else
        vertex.set_color('white')
        vertex.set_parent('nil')
    time = 0
    for vertex in g:
        if vertex.color == 'white':
            special_dfs_visit(vertex, path, time)
    print '\n If there were any cycles, they have appeared above. \n'

def special_dfs_visit(v, path, time):  
    v.set_color('gray')          #this vertex has been discovered
    push(path, v)                # push v onto the path
    v.set_discover_time(time)
    time = time + 1
    for adjacent_vertex in v.adjacent:
        #print adjacent_vertex
        #print adjacent_vertex.__dict__
        path_check(adjacent_vertex, path)
        if adjacent_vertex.color == 'white':
            #print 'setting parent'
            adjacent_vertex.set_parent(v)
            special_dfs_visit(adjacent_vertex, path, time)
    #print 'coloring black'
    v.color = 'black' # looped through the adjacencies, we're done here.
    v.set_finish_time(time)
    #print 'about to pop something from the following stack: '
    #for i in path:
    #    print i.name
    #print '\n'
    pop(path)
    time = time + 1
    #print 'exiting dfs_visit'
    #print time

def path_check(vertex, path): # is vertex on trail we've blazed?
    for i in range(0, (len(path) - 1)):  
        if path[i] == vertex:
            print "ack! a cycle!"
            cycle_path = path[i:]
            cycle_path.append(vertex)
            for each_vertex in cycle_path:
                print each_vertex.name
    #print 'finished checking for ' + vertex.name

##############################################################################80
###Problem 6

def score_finder(g):
    b_tally = 0
    w_tally = 0
    for vertex in g:
        print 'finding score for vertex %s %s' %(vertex.xpos, 
vertex.ypos)
        if vertex.gamestate == 'b':
            b_tally = b_tally + 1
        if vertex.gamestate == 'w':
            w_tally = w_tally + 1
        if vertex.gamestate == 'e':
            e_state = surround_search(vertex, g)
            if e_state == 'w':
                w_tally = w_tally + 1
            if e_state == 'b':
                b_tally = b_tally + 1
    print "black's score is %s, white's score is %s" %(b_tally, 
w_tally)

def surround_search(search_vertex, g):
    for each_vertex in g:     #initialize everything
        each_vertex.set_color('white')
    global seen_black
    seen_black = 0
    global seen_white
    seen_white = 0
    print 'about to search from %s %s' %(search_vertex.xpos, 
search_vertex.ypos)
    game_dfs_visit(search_vertex)
    print str(seen_black) + str(seen_white)
    if seen_black and seen_white:
        print 'saw both'
        return 0
    if seen_black:
        print 'saw black'
        return 'b'
    if seen_white:
        print 'saw white'
        return 'w'
    print "you really shouldn't get here" 
    return 0 # we'll only get here on a totally empty board


def game_dfs_visit(v):  
    v.set_color('gray')          #this vertex has been discovered
    for adjacent_vertex in v.adjacent:
        if adjacent_vertex.gamestate == 'e' \
           and adjacent_vertex.color == 'white':
            game_dfs_visit(adjacent_vertex)
        if adjacent_vertex.gamestate == 'b':
            global seen_black
            seen_black = 1
        if adjacent_vertex.gamestate == 'w':
            global seen_white
            seen_white = 1
    #print 'coloring black'
    v.color = 'black' # looped through the adjacencies, done here.

##############################################################################80
###some graph operations
def matrix_to_adj(matrix): # takes array of lists, converts to adjacency list
    converted = []  #initialize the adjacency list
    # make each cell into a vertex, set the game state
    for row_index in range(0, len(matrix)):
        #print 'row ' + str(row_index)
        for cell_index in range(0, len(matrix[row_index])):
            #print 'column ' + str(cell_index)
            this_cell = vertex()
            this_cell.gamestate = matrix[row_index][cell_index]
            this_cell.xpos = row_index
            this_cell.ypos = cell_index
            converted.append(this_cell)
    #go back and install the adjacencies
    for one_vertex in converted:
        for another_vertex in converted:
            if (  (((one_vertex.xpos - another_vertex.xpos ) == 1)\
                   and (one_vertex.ypos == another_vertex.ypos))\
               or (((one_vertex.ypos - another_vertex.ypos ) == 1)\
                   and (one_vertex.xpos == another_vertex.xpos))\
               or (((another_vertex.xpos - one_vertex.xpos ) == 1)\
                   and (one_vertex.ypos == another_vertex.ypos))\
               or (((another_vertex.ypos - one_vertex.ypos ) == 1)\
                   and (one_vertex.xpos == another_vertex.xpos))):
                one_vertex.add_adjacent(another_vertex)
##     for each_vertex in converted:
##         print '\n' + str(each_vertex.xpos) + ' ' + str(each_vertex.ypos)  \
                ##+ ' ' +  each_vertex.gamestate + '\n adjacencies: '
##         for x in each_vertex.adjacent:
##             print str(x.xpos) + ' ' + str(x.ypos)  + ' ' +  x.gamestate
    return converted
##############################################################################80
###print the game board in a reasonably readable format
def print_board(matrix):
    for row in matrix:
        for cell in row:
            if cell == 'e':
                sys.stdout.write('.')
            else:
                sys.stdout.write(cell)
        sys.stdout.write('\n')
    sys.stdout.write('\n')



##############################################################################80
###Problem 7
## This problem can be solved by comparing every possible ordering of
## performers, deciding if the ordering is legal, then figuring out how
## many people are in the biggest one.  Obviously this is not an efficient
## solution.  It would entail running nested for loops that create an array
## of all the possible combinations then comparing them, and then taking
## the length of the winning list of people.

## A more elegant solution would take advantage of the fact that many combinations can
## be dismissed out of hand.  Basically, first create a graph representing "can stand
## on the shoulders of" as an adjacency.  Then, follow the paths from each node and
## keep a tally of the size of the largest one so far using a stack like the one
## used in problem 5.  When all the paths have been tried, the largest one so far
## is the largest one.










##############################################################################80
###here's some sample data to chew on:

# a directed acyclic graph:
v1 = vertex()
v1.set_name('v1')
v2 = vertex()
v2.set_name('v2')
v3 = vertex()
v3.set_name('v3')
v4 = vertex()
v4.set_name('v4')
v5 = vertex()
v5.set_name('v5')
v6 = vertex()
v6.set_name('v6')
v7 = vertex()
v7.set_name('v7')

v1.add_adjacent(v2)
v2.add_adjacent(v3)
v3.add_adjacent(v5)
v4.add_adjacent(v7)
v5.add_adjacent(v6)
v5.add_adjacent(v7)
v6.add_adjacent(v4)
v6.add_adjacent(v7)
g1 = [v1, v2, v3, v4, v5, v6, v7] 

##here's a directed graph with a cycle 3,5,6,3 
vc1 = vertex()
vc1.set_name('vc1')
vc2 = vertex()
vc2.set_name('vc2')
vc3 = vertex()
vc3.set_name('vc3')
vc4 = vertex()
vc4.set_name('vc4')
vc5 = vertex()
vc5.set_name('vc5')
vc6 = vertex()
vc6.set_name('vc6')
vc7 = vertex()
vc7.set_name('vc7')

vc1.add_adjacent(vc2)
vc2.add_adjacent(vc3)
vc3.add_adjacent(vc5)
vc4.add_adjacent(vc7)
vc5.add_adjacent(vc6)
vc5.add_adjacent(vc7)
vc6.add_adjacent(vc3)
vc6.add_adjacent(vc7)
gc1 = [vc1, vc2, vc3, vc4, vc5, vc6, vc7] 

##here's a 7x7 matrix of ewb's
gameboard = [['e', 'e', 'e', 'e', 'e', 'e', 'e'], \
             ['e', 'w', 'w', 'w', 'e', 'e', 'e'], \
             ['e', 'w', 'e', 'w', 'e', 'e', 'e'], \
             ['e', 'w', 'w', 'w', 'e', 'e', 'e'], \
             ['e', 'e', 'e', 'e', 'e', 'e', 'e'], \
             ['e', 'e', 'e', 'e', 'e', 'e', 'e'], \
             ['e', 'e', 'e', 'e', 'e', 'b', 'b']]


##############################################################################80
###Here's the calls to our code.

print "\n problem 4: \n"
print topo2(g1)

print "\n problem 5: \n"
dfs_cycle_killer(gc1)

print "\n problem 6: "
score_finder(matrix_to_adj(gameboard))

print '\n'

print_board(gameboard)


print "\n\n\n"

