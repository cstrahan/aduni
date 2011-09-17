/**
   PS-2 Problem 2
   Game.java implements the data model for Same Game
   3 2-D arrays are used: one for tracking colors, another for previous
   configuration of the color array (for undo) and a third for tracking
   neighbors (for highlighting and deletion).
   @author Shyam Visweswaran

   See towards the end for the methods implemented for the Algorithms problem.
*/
import java.util.*;

public class Game
{
  public static final int EMPTY = 0; // empty cell is 0
  public int height; // height of board
  public int width; // width of board
  private int numberOfColors; // 3 colors for now
  private int neighborCount; // includes current cell
  public int emptyCells; // count of empty cells
  private int prevEmptyCells;
  private int totalScore; // running total
  private int prevTotalScore;
  private boolean isGameOver;
  private boolean canUndo; // since can undo only 1 step

  // cell colors are stored as integers
  private int[][] colorArray;
  private int[][] prevColorArray;
  //same-color neighbors including current cell are marked true
  private boolean[][] neighborArray;
  private boolean[][] visitArray;
  
  public static final int MAX_DEPTH = 3;
  private Board[] path;
  private boolean[] tracker;
  private int xCoordinate;
  private int yCoordinate;
  
  // default constructor
  public Game() 
  {
    width = 15;
    height = 10;
    numberOfColors = 3;
    neighborCount = 0;
    emptyCells = 0;
    prevEmptyCells = 0;
    totalScore = 0;
    prevTotalScore = 0;
    isGameOver = false;
    canUndo = false;
    colorArray = new int[width][height];
    prevColorArray = new int[width][height];
    neighborArray = new boolean[width][height];
    initializeArrays();
    xCoordinate = 0;
    yCoordinate = 0;
  }

  // Keep original width, height and numberOfColors when restarting game
  public void restartGame()
  {
    neighborCount = 0;
    emptyCells = 0;
    prevEmptyCells = 0;
    totalScore = 0;
    prevTotalScore = 0;    
    isGameOver = false;
    canUndo = false;
    colorArray = new int[width][height];
    prevColorArray = new int[width][height];
    neighborArray = new boolean[width][height];
    initializeArrays();
    xCoordinate = 0;
    yCoordinate = 0;
  }
  
  // undo method - keep same width, height, numberOfColors
  public void undoGame()
  {
    neighborCount = 0;
    emptyCells = prevEmptyCells;
    totalScore = prevTotalScore;
    isGameOver = false;
    canUndo = false;
    // copy prev array to current array
    for (int i = 0; i < width; i++)
      for (int j = 0; j < height; j++)
        colorArray[i][j] = prevColorArray[i][j];
    initBooleanArray(neighborArray, false);
  }
  
  // return width of board
  public int getWidth()
  {
    return width;
  }

  // return height of board
  public int getHeight()
   {
    return height;
  }

  // points for a connected group = number of cells of
  // same color - 2 and square
  public int getNeighborScore()
  {
    if (neighborCount <= 2) return 0;
    else return ((neighborCount - 2) * (neighborCount - 2));
  }
  
  // get the total score
  public int getTotalScore()
  {
    return totalScore;
  }

  // return the color array
  public int[][] getColorArray()
  {
    return colorArray;
  }

  // return the neighbor array
  public boolean[][] getNeighborArray()
  {
    return neighborArray;
  }

  // if mouse is outside frame boundaries set initialize neighbor
  // array to false
  public void mouseOutside()
  {
    initBooleanArray(neighborArray, false);
  }
  
  // is game over?
  public boolean gameEnd()
  {
    return isGameOver;
  }
  
  // is undo possible?
  public boolean isUndoPossible()
  {
    return canUndo;
  }
  
  /**
     Fill the color array randomly with numbers 1 to numberOfColors (3 here)
  */
  private void initializeArrays()
  {
    for (int i = 0; i < width; i++)
      for (int j = 0; j < height; j++)
        colorArray[i][j] = (((int)(Math.random() * 100)) % numberOfColors) + 1;
    // initialize neighbor array to false
    initBooleanArray(neighborArray, false);
  }

  /**
     Compact all columns.
  */
  private void compactColumns()
  { // uses a temp column, could also be implemented where the compacting is
    // done in place
    int[] tempColumn = new int[height];
    for (int i = 0; i < width; i++)
    {
      int t = 0; // temp index
      for (int j = 0; j < height; j++)
      { // copy only non-empty cells to temp
        if (colorArray[i][j] != EMPTY) tempColumn[t++] = colorArray[i][j];
      }
      // fill the top of temp column with empty cells
      while (t < height) tempColumn[t++] = EMPTY;
      // copy compacted temp column back to the array
      for (int j = 0; j < height; j++)
      {
        colorArray[i][j] = tempColumn[j];
      }
    }
  }
  
  /**
     Slide columns to the left; this method should be called after compacting
     columns.
  */
  private void slideColumns()
  { // note: increment only upto penultimate column
    for (int i = 0; i < (width - 1); i++)
    { // check if a bottom cell is empty
      if (colorArray[i][0] == EMPTY)
      {
        int t = i + 1; // t is the next column
        // increment t to next non-empty column
        while ((t < width) && (colorArray[t][0] == EMPTY)) t++;
        // if t points beyond the last possible column we dont have to slide any columns
        // otherwise copy column t to column i
        if (t != width)
        {
          for (int j = 0; j < height; j++)
          {
            colorArray[i][j] = colorArray[t][j]; // copy column t to column i
            colorArray[t][j] = EMPTY; // and set column t to empty
          }
        }
      }
    }
  }
  
  /**
     Mark the neighboring 'connected' cells
  */
  public void updateNeighbors(int x, int y)
  { // initialize neighbor array to false
    initBooleanArray(neighborArray, false);
    // and initialize neighbor count to zero
    this.neighborCount = 0;
    // test for out of array bounds
    if ((x < 0) || (x >= width) || (y < 0) || (y >= height)) return;
    // get color of the current cell
    int color = colorArray[x][y];
    // if it is an empty cell, return
    if (color == EMPTY) return;
    // call mathod to identify connected cells of same color
    markNeighbors(x, y, color);
    // if no neighboring cells are found set count to zero and initialize the array    
    if (neighborCount <= 1)
    {
      this.neighborCount = 0;
      initBooleanArray(neighborArray, false);
    }
  }
  
  /**
     Identify neighbors of same color and count them using a breadth first strategy.
     This method should be called only from the updateNeighbors routine.
  */
  private void markNeighbors(int x, int y, int color)
  {
    this.neighborArray[x][y] = true; // mark the current cell true
    this.neighborCount++; // count cells in a group
    // recursive calls for north, east, south, west cells
    if ((x > 0) && !neighborArray[x - 1][y] && (color == colorArray[x - 1][y]))
      markNeighbors(x - 1, y, color);
    if ((x < (width - 1)) && !neighborArray[x + 1][y] && (color == colorArray[x + 1][y]))
      markNeighbors(x + 1, y, color);
    if ((y > 0) && !neighborArray[x][y - 1] && (color == colorArray[x][y - 1]))
      markNeighbors(x, y - 1, color);   
    if ((y < (height - 1)) && !neighborArray[x][y + 1] && (color == colorArray[x][y + 1]))
      markNeighbors(x, y + 1, color); 
  }
  
  /**
     Delete group of cells of same color and do housekeeping
  */
  public void delNeighbors(int x, int y)
  {
    xCoordinate = x;
    yCoordinate = y;
    
    // first, backup configuration for undo
    for (int i = 0; i < width; i++)
      for (int j = 0; j < height; j++)
        prevColorArray[i][j] = colorArray[i][j];
  
    canUndo = true;
    prevTotalScore = totalScore;
    prevEmptyCells = emptyCells;
    
    // second, get neighboring cells of same color
    this.updateNeighbors(x, y);
    for (int i = 0; i < width; i++)
      for (int j = 0; j < height; j++)
        // delete these cells
        if (neighborArray[i][j]) colorArray[i][j] = EMPTY;

    // third, compact columns and slide columns and update score
    // and count of empty cells
    this.compactColumns();
    this.slideColumns();
    totalScore += this.getNeighborScore();
    emptyCells += this.neighborCount;

    // fourth, test for end of game if all cells are empty
    if (emptyCells == width * height) 
    { // bonus 1000 points if all cells are empty
      totalScore += 1000;
      isGameOver = true;
    }

    // fourth contd., test for end of game if some non-empty cells
  test_for_end_of_game:
    for (int i = 0; i < width; i++)
      for (int j = 0; j < height; j++)
      {
        this.updateNeighbors(i, j);
        if (this.neighborCount > 0)
        { // break out of loop at the first evidence
          // that there is a group of connected balls
          isGameOver = false;
          break test_for_end_of_game;
        }
        else
        {
          isGameOver = true;
        }
      }
    
    // fifth, again update neighbors for better display
    this.updateNeighbors(x, y);
  }

  /**
     Utility to initialize a boolean array
  */
  private void initBooleanArray(boolean[][] anArray, boolean value)
  { 
    for (int i = 0; i < width; i++)
      for (int j = 0; j < height; j++)
        anArray[i][j] = value;
  }

  /*===============================================================
   Methods for the algorithm pset. hint uses 2 one dimensional arrays
   for storing boards in the current best path of the search tree
   and a boolean array used to update upstream boards in the path
   if the leaf board changes

  public void hint()
  {
    int[][] emptyArray = new int[width][height];
    for (int i = 0; i < width; i++)
      for (int j = 0; j < height; j++)
        emptyArray[i][j] = -100;
    
    path = new Board[MAX_DEPTH + 1];
    // initialize path to dummy boards
    for (int i = 0; i < path.length; i++)
      path[i] = new Board(emptyArray, -100, -100, -100, -100);
    
    tracker = new boolean[MAX_DEPTH + 1];
    // initialize tracker to false
    for (int i = 0; i < tracker.length; i++)
      tracker[i] = false;
    
    // make a board object of the current board and call dfs
    Board board = new Board(colorArray, totalScore, 0, 0, 0);
    dfs(board);
    // coordinates of board #1 is the best possible move
    delNeighbors(path[1].xCoordinate(), path[1].yCoordinate());
  }
 
  // depth first search upto MAX_DEPTH
  public void dfs(Board board)
  {
    if (board.depth() == MAX_DEPTH)
    { // if current board has a higher score put it in the path
      if (board.score() > path[board.depth()].score())
        tracker[board.depth()] = true;
    }
    else
    { // get all possible groups to reduce the number of children
      boolean[][] groupArray = board.computeGroups();
      boolean move = false;
      for (int i = 0; i < width; i++)
      {
        for (int j = 0; j < height; j++)
        {
          if (groupArray[i][j])
          { // create a child board for a given set of coordinates
            Board b = board.computeChild(i, j);
            move = true;
            dfs(b);
          }
        }
      }
      if (!move) tracker[board.depth()] = true;
    }
    
    if (tracker[board.depth()])
    { // if leaf board was changed then update its parents appropiately
      path[board.depth()] = board;
      tracker[board.depth()] = false;
      if (board.depth() > 0) tracker[board.depth() - 1] = true;
    }
  }
}




