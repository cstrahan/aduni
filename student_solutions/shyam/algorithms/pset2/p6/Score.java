/*
 * Score.java. Depth first serch to find the score of a given Go board
 * Algorithms PSet-2 Problem 6
 * Shyam Visweswarn
 */
import java.util.*;

public class Score
{
  static int UNVISITED = 0; // vertex not yet visited on DFS
  static int VISITING = 1; // vertex being scanned during DFS
  static int VISITED = 2; // vertex completely scanned at DFS
  static Vertex[][] g; // 2-D array of vertices that represent the graph
  static Vertex[] neighbors = new Vertex[4]; // make an association list for a vertex
  static int neighborCount; // number of neighbors that a vertex has
  static Stack s;
  
  static char currentColor; // black, white or empty
  static boolean toAdd; // should the empty vertices found be added to someone's core
  static int count; // string of connected empty vertices
  static int blackScore; // black's score
  static int whiteScore; // white's score

  static char[][] board = new char[][] // use this board if a random board is not generated
  {
    {'W', 'W', 'E', 'E', 'W', 'W', 'B'},
    {'E', 'E', 'W', 'E', 'E', 'E', 'W'},
    {'E', 'W', 'W', 'W', 'W', 'E', 'E'},
    {'W', 'W', 'W', 'B', 'E', 'B', 'B'},
    {'B', 'B', 'B', 'E', 'E', 'B', 'W'},
    {'E', 'E', 'B', 'W', 'B', 'E', 'B'},
    {'B', 'B', 'W', 'W', 'W', 'E', 'E'},
  };
  
  public static void main(String[] args)
  { 
    if (args.length == 1)
      makeRandomBoard(Integer.parseInt(args[0])); // create a random n*n board according to first argument
    makeGraph(); // make a 2-D graph of association lists
    displayBoard(); // display the board as a 2*2 matrix
    computeScore(); // compute scores
    System.out.println("Scores: Black = " + blackScore + ", White = " + whiteScore);
  }

  // create a n*n array and populate randomly
  public static void makeRandomBoard(int n)
  {
    board = new char[n][n];
    for (int i = 0; i < n; i++)
    {
      for (int j = 0; j < n; j++)
      {
        int random = (int)((Math.random() * 10) % 3);
        if (random == 0) board[i][j] = 'W';
        if (random == 1) board[i][j] = 'B';
        if (random == 2) board[i][j] = 'E';
      }
    }
  }
    
  // display the board as a 2*2 matrix
  public static void displayBoard()
  {
    System.out.println("Displaying board...");
    for (int i = 0; i < board.length; i++)
    {
      for (int j = 0; j < board[i].length; j++)
      { // display e for E
        if (board[i][j] == 'E') System.out.print(" " + "e");
        else System.out.print(" " + board[i][j]);
      }
      System.out.println();
    }
    System.out.println();
  }
  
  // make vertex objects and store them in graph array
  public static void makeGraph()
  {
    g = new Vertex[board.length][board[0].length];
    for (int i = 0; i < board.length; i++)
      for (int j = 0; j < board[i].length; j++)
        g[i][j] = new Vertex(i, j, board[i][j]);
  }
  
  public static void computeScore()
  {
    blackScore = 0;
    whiteScore = 0;
    for (int i = 0; i < g.length; i++)
    {
      for (int j = 0; j < g[i].length; j++)
      {
        // do a DFS search on an emapty vertex that has not been visited
        if ((g[i][j].color() == 'E') && (g[i][j].status() == UNVISITED))
        {
          count = 0;
          toAdd = false;          
          currentColor = 'N';
          dfsScore(g[i][j]);
          
          if (toAdd && (currentColor == 'W'))
            whiteScore = whiteScore + count;
          if (toAdd && (currentColor == 'B'))
            blackScore = blackScore + count;
        }
        else if (g[i][j].color() == 'W') whiteScore++;
        else if (g[i][j].color() == 'B') blackScore++;
      }
    }
  }

  public static void dfsScore(Vertex vertex)
  {
    vertex.setStatus(VISITING);
    getNeighbors(vertex);
    for (int i = 0; i < neighborCount; i++)
    {
      Vertex newVertex = neighbors[i];
      if (newVertex.status() == UNVISITED)
      {
        if (newVertex.color() == 'E')
        { // recursively call DFS if empty
          dfsScore(newVertex);
        }
        else if (currentColor == 'N')
        { // get color of first non-empty vertex
          currentColor = newVertex.color();
          toAdd = true;
        }   
        else if (newVertex.color() != currentColor)
        { // if color is different from the currently stored color do not add empty vertices to scores
          toAdd = false;
        }
        else if (newVertex.color() == currentColor)
        { // if color matches do nothing
        }
      }
    }
    vertex.setStatus(VISITED);
    count++;
  }

  // given a vertex get its neighbors and their count
  public static void getNeighbors(Vertex vertex)
  {
    int x = vertex.x();
    int y = vertex.y();
    int i = 0;
    if (x > 0)
    {
      neighbors[i] = g[x - 1][y];
      i++;
    }
    if (x < (g.length - 1))
    {
      neighbors[i] = g[x + 1][y];
      i++;
    }
    if (y > 0)
    {
      neighbors[i] = g[x][y - 1];
      i++;
    }
    if ( y < (g.length - 1))
    {
      neighbors[i] = g[x][y + 1];
      i++;
    }
    neighborCount = i;
  }
}       

class Vertex{
  String id; // coordinates as a string - for debugging
  int x; // x coordinate
  int y; // y coordinate
  char color; // color
  int status; // visited, visiting or unvisited
    
  public Vertex(int x, int y, char color)
  {
    id = (x + "-" + y);
    this.x = x;
    this.y = y;
    this.color = color;
    status = Score.UNVISITED;
  }
  
  public String id()
  {
    return id;
  }
  
  public int x()
  {
    return x;
  }
  
  public int y()
  {
    return y;
  }
  
  public char color()
  {
    return color;
  }
    
  public void setColor(char color)
  {
    this.color = color;
  }
  
  public int status()
  {
    return status;
  }
  
  public void setStatus(int status)
  {
    this.status = status;
  }
}

/*
Tests on Score.java

> java Score (uses default board)

Displaying board...
 W W e e W W B
 e e W e e e W
 e W W W W e e
 W W W B e B B
 B B B e e B W
 e e B W B e B
 B B W W W e e

Scores: Black = 15, White = 21


> java Score 10 (constructs a 10*10 board)

Displaying board...
 B B B B W e W B W B
 B e W W B W B W e B
 B W e e W B e B B B
 W W B W B e B W e W
 B W W e B e B W e W
 W W B e W B B W W W
 W W e W B W B W W e
 W B B W e W W B W e
 e W B B W e e W W W
 e W W e e W W e B W

Scores: Black = 35, White = 52
*/
