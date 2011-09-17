##############################################################################80

###Algorithms ps3 code
###
###This builds substantially on the ps2 code.

import sys

##############################################################################80
###setup: DFS code...
class vertex: # 5 methods to handle parent, unweighted adjacencies, color, discover time, finish time.
    def set_parent(self, new_parent):
        self.parent = new_parent
    def __init__(self):
        self.adjacent = []
        self.color = 'white'
        self.parent = 'nil'
        self.gamestate = 'e'
        self.legality = 'I' 
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
    def set_legality(self, legal_or_not):
        self.legality = legal_or_not


##############################################################################80
###some graph operations
def matrix_to_adj(matrix): # takes array ewb lists, converts into adjacency list
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

def adj_to_matrix(adj_list):
    print adj_list
    converted = []  #initialize the adjacency list
    for each_vertex in adj_list:
        while len(converted) < (each_vertex.xpos + 1):
            print 'looping x, length is %s, xpos is %s' % (len(converted), each_vertex.xpos)
            converted.append([])
        while len(converted[each_vertex.xpos]) < (each_vertex.ypos + 1):
            converted[each_vertex.xpos].append([])
        converted[each_vertex.xpos][each_vertex.ypos] = each_vertex.legality
        print converted
    return converted
    
##############################################################################80
###print the game board in a reasonably readable format
def print_board(matrix):
    for row in matrix:
        for cell in row:
            if cell == '0':
                sys.stdout.write('.')
            else:
                sys.stdout.write(cell)
                sys.stdout.write(' ')
        sys.stdout.write('\n')
    sys.stdout.write('\n')

###############################################################################
####Problem 1:

###plan: a point is legal if
    #it is adjacent to an empty square OR
    #it is adjacent to a square of the same color that is adjacent to 
    #an empty square or lies on a path of like-colored squares to such a 
    #square or
    #placing the square would cause those off-color squares surrounding 
    #it and its like-colored neighbors to be surrounded themselves.
    # if surrounded, check along the search path for a white square-empty square cycle.
    #
    #look for cycles- use the ps2 special dfs thing to do it?

def legality_check(g, mover):
    global move_owner
    move_owner = mover
    for vertex in g:
        print 'checking legality for vertex %s %s' %(vertex.xpos, vertex.ypos)
        if vertex.gamestate == 'b' or vertex.gamestate == 'w':
            vertex.legality = 'I'
        if vertex.gamestate == '0':
            vertex.legality = surround_search(vertex, g)
    return g


def surround_search(search_vertex, g):
    for each_vertex in g:     #initialize everything
        each_vertex.set_color('white')
    global seen_black
    seen_black = 0
    global seen_white
    seen_white = 0
    global seen_empty
    seen_empty = 0
    print 'about to search from %s %s' %(search_vertex.xpos, search_vertex.ypos)
    game_dfs_visit(search_vertex)
    print str(seen_black) + str(seen_white)
    if seen_black and seen_white:
        print 'saw both'
        return 'L'
    # need to have a check for double surround
    if seen_empty:
        if seen_black:
            print 'saw black'
            if move_owner == 'b':
                return 'L'
            else:
                return 'I'
        if seen_white:
            print 'saw white'
            if move_owner == 'w':
                return 'L'
            else:
                return 'I'
    print "you really shouldn't get here" 
    return 'L' # we'll only get here on a totally empty board


def game_dfs_visit(v):  
    v.set_color('gray')          #this vertex has been discovered
    for adjacent_vertex in v.adjacent:
        if adjacent_vertex.gamestate == '0' \
           and adjacent_vertex.color == 'white':
            game_dfs_visit(adjacent_vertex)
            global seen_empty
            seen_empty = 1
        if adjacent_vertex.gamestate == 'b':
            global seen_black
            seen_black = 1
        if adjacent_vertex.gamestate == 'w':
            global seen_white
            seen_white = 1
        ####Need to test for surrounding properly, searching deeper on 
           #seen white/seen black unless seen both?
    #print 'coloring black'
    v.color = 'black' # we've looped through the adjacencies, we're done here.

###here's some sample data to chew on:

##here's a 7x7 matrix of 0wb's
gameboard = [['0', '0', '0', '0', '0', '0', '0'], \
             ['0', 'w', 'w', 'w', '0', '0', '0'], \
             ['0', 'w', '0', 'w', '0', '0', '0'], \
             ['0', 'w', 'w', 'w', '0', 'b', '0'], \
             ['0', '0', '0', '0', '0', 'b', 'b'], \
             ['0', '0', '0', '0', '0', 'b', '0'], \
             ['0', '0', '0', '0', '0', 'b', 'b']]


##############################################################################80
###Here's the calls to our code.

print "\n problem 6: "
legality_check(matrix_to_adj(gameboard), 'b')


print '\n'

print_board(gameboard)
print_board(adj_to_matrix(legality_check(matrix_to_adj(gameboard), 'b')))

print "\n\n\n"








