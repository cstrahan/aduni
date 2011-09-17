/*
  Go.java - computing legal moves given a legal Go board.
  First computes liberties for each non-empty spot. Then scans the
  liberites of neighbors for every empty spot to compute if the spot 
  is a legal place to move for a given color.
  Algorithms: PSet 3 - Problem 1
  Shyam Visweswaran
*/

import java.util.*;

public class Go
{
  static int UNVISITED = 0; // unvisited vertex
  static int VISITING = 1;
  static int VISITED = 2; // vertex completely scanned

  // variables representing moves
  static char UNKNOWN = '?'; 
  static char LEGAL = '*';
  static char ILLEGAL = 'x';

  static int score; // for computing liberties for a given vertex
  
  static Vertex[][] g; // 2-d array of vertices represents a graph
  static int neighborCount; // number of neighbors for a given vertex
  static Vertex[] neighbors = new Vertex[4]; // array for neighbors - adjacency list
  
  static char[][] board = new char[][] // input board
  {
    {'E', 'B', 'B', 'E', 'E'},
    {'B', 'W', 'W', 'B', 'E'},
    {'B', 'W', 'E', 'W', 'B'},
    {'B', 'W', 'W', 'W', 'B'},
    {'E', 'B', 'B', 'B', 'E'},
  };
  
  public static void main(String[] args)
  {
    if (args.length > 0)
    { // use input args to construct board,otherwise use default board
      board = new char[args.length][];
      for (int i = 0; i < args.length; i++)
        board[i] = args[i].toCharArray();
    }
    
    makeGraph(); // compute 2-D array g
    computeLiberties(); // compute liberties for each non-empty vertex
    computeMoves('B', 'W'); // compute legal moves for Black
    displayBoard('B'); // display board and legal moves for Black
    computeMoves('W', 'B'); // compute legal moves for White
    displayBoard('W'); // display board and legal moves for White
  }

  // display input board and legal moves for a particular player
  public static void displayBoard(char player)
  {
    System.out.println(" Displaying board and the legal moves for " + player);
    System.out.println(" Legend: * = LEGAL move, x = ILLEGAL move");
    System.out.println();   
    for (int i = 0; i < board.length; i++)
    {
      for (int j = 0; j < board[i].length; j++)
      {
        if (board[i][j] == 'E') System.out.print(" e");
        else System.out.print(" " + board[i][j]);
      }
      System.out.print("      "); 
      
      for (int j = 0; j < board[i].length; j++)
      {
        if (board[i][j] == 'E') System.out.print(" " + g[i][j].move());
        else System.out.print(" " + board[i][j]);
      }
      System.out.println();
    }
    System.out.println();
  }
  
  // make a 2-D array of vertices
  public static void makeGraph()
  {
    g = new Vertex[board.length][board[0].length];
    for (int i = 0; i < board.length; i++)
      for (int j = 0; j < board[i].length; j++)
        g[i][j] = new Vertex(i, j, board[i][j]);
  }
  
  // compute liberties for each non-empty vertex
  public static void computeLiberties()
  {
    for (int i = 0; i < g.length; i++)
    {
      for (int j = 0; j < g[i].length; j++)
      {
        Vertex v = g[i][j];
        if ((v.color() != 'E') && (v.scanStatus() == UNVISITED))
        {
          score = 0;
          dfsScan(v); // recursively compute number of surrounding empty vertices
          dfsUpdate(v); // update the connected vertices with the computed liberties
        }
      }
    }
  }
  
  // recursively compute the number of surrounding empty vertices
  // which is the number of liberties
  public static void dfsScan(Vertex v)
  {
    v.setScanStatus(VISITING);
    getNeighbors(v);
    for (int i = 0; i < neighborCount; i++)
    {
      Vertex u = neighbors[i];
      if ((u.color() == v.color()) && (u.scanStatus() == UNVISITED))
        dfsScan(u);
      else if ((u.color() == 'E') && (u.scanStatus() == UNVISITED))
      {
        u.setScanStatus(VISITED);
        u.setUpdateStatus(UNVISITED);
        score++;
      }
    }
    v.setScanStatus(VISITED);
  }
  
  // recursively update the vertices with the liberties(score) computed
  // in dfsScan
  public static void dfsUpdate(Vertex v)
  {
    v.setUpdateStatus(VISITING);
    v.setLiberties(score);
    getNeighbors(v);
    for (int i = 0; i < neighborCount; i++)
    {
      Vertex u = neighbors[i];
      if ((u.color() == v.color()) && (u.updateStatus() == UNVISITED))
        dfsUpdate(u);
      else if ((u.color() == 'E') && (u.updateStatus() == UNVISITED))
      {
        u.setUpdateStatus(VISITED);
        u.setScanStatus(UNVISITED);
      }
    }
    v.setUpdateStatus(VISITED);
  }

  // for a given color compute all legal moves by scanning the liberities
  // of the 4 adjacent vertices. Only empty vertices are scanned.
  public static void computeMoves(char player, char opponent)
  {
    for (int i = 0; i < g.length; i++)
    {
      for (int j = 0; j <g[i].length; j++)
      {
        Vertex v = g[i][j];
        if (v.color() == 'E')
        {
          getNeighbors(v);
          int oppScore = -1;
          int playerScore = -1;
          for (int k = 0; k < neighborCount; k++)
          {
            Vertex u = neighbors[k];
            if ((u.color() == opponent) && (u.liberties() == 1)) oppScore = 1;
            if ((u.color() == player) && (u.liberties() == 1)) playerScore = 1;
          }
          if (oppScore == 1) v.setMove(LEGAL); // if opponent has a stone with 1 liberty then move is legal
          else if (playerScore == 1) v.setMove(ILLEGAL); // else if player has a stone with 1 liberty this spot is 'suicidal'
          else v.setMove(LEGAL); // if the spot is not 'suicidal' then it is legal
        }
      }
    }
  }
  
  // given a vertex compute its neighbors
  public static void getNeighbors(Vertex v){
    int x = v.x();
    int y = v.y();
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

class Vertex
{
  String id;
  int x; // x coordinate
  int y; // y coordinate
  char color;
  int scanStatus; // flag used during dfsScan
  int updateStatus; // flag used during dfsUpdate
  int liberties; // number of liberties
  char move; // legal or not
  
  public Vertex(int x, int y, char color)
  {
    id = (x + "-" + y);
    this.x = x;
    this.y = y;
    this.color = color;
    scanStatus = Go.UNVISITED;
    updateStatus = Go.UNVISITED;
    liberties = 9;
    move = Go.UNKNOWN;
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
  
  public int scanStatus()
  {
    return scanStatus;
  }
  
  public void setScanStatus(int status)
  {
    scanStatus = status;
  }
  
  public int updateStatus()
  {
    return updateStatus;
  }
  
  public void setUpdateStatus(int status)
  {
    updateStatus = status;
  }
  
  public int liberties()
  {
    return liberties;
  }
  
  public void setLiberties(int score)
  {
    liberties = score;
  }
  
  public char move()
  {
    return move;
  }
  
  public void setMove(char move)
  {
    this.move = move;
  }
}

