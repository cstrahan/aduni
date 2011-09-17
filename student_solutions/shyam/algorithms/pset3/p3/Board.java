/*
  Algorithms Pset 3 - Problem 3
  Board.java
*/
public class Board
{
  public static final int EMPTY = 0;
  
  private int[][] colorArray; // color configuration of board
  private int width; // width of board
  private int height; // height of board
  private int depth; // depth of the board in the current depth-first-search tree
  private int score; // score associated with current configuration of board
  private int xCoordinate; // coordinates that led to current configuration
  private int yCoordinate;
  
  // array used in computing the group of same color given particulr coordinates
  private boolean[][] neighborArray;
  private int neighborCount; // number of balls in the group
  

  public Board(int[][] cArray, int score, int depth, int x, int y)
  {
    width = cArray.length;
    height = cArray[0].length;
    colorArray = new int[width][height];
    // copy the array
    for (int i = 0; i < width; i++)
      for (int j = 0; j < height; j++)
        colorArray[i][j] = cArray[i][j];

    this.depth = depth;
    this.score = score;
    xCoordinate = x;
    yCoordinate = y;
  }   
  
  public int[][] colorArray()
  {
    return colorArray;
  }
  
  public int width()
  {
    return width;
  }
  
  public int height()
  {
    return height;
  }
  
  public int depth()
  {
    return depth;
  }
  
  public int score()
  {
    return score;
  }
  
  public int xCoordinate()
  {
    return xCoordinate;
  }
  
  public int yCoordinate()
  {
    return yCoordinate;
  }
  
  // get all groups of once color with an associated single set of coordinates
  public boolean[][] computeGroups()
  {
    int width = this.width();
    int height = this.height();
    int[][] colorArray = this.colorArray();
    boolean[][] trackArray = new boolean[width][height]; // keeping track of cells already visited
    boolean[][] groupArray = new boolean[width][height]; // will contain the final coordinates
    neighborArray = new boolean[width][height];
    initBooleanArray(trackArray, true);
    initBooleanArray(groupArray, false);
    
    for (int i = 0; i < width; i++)
    {
      for (int j = 0; j <height; j++)
      {
        if (trackArray[i][j])
        {
          if (colorArray[i][j] == EMPTY)
          {
            trackArray[i][j] = false;
            groupArray[i][j] = false;
          }
          else
          {
            neighborCount = 0;
            initBooleanArray(neighborArray, false);
            getNeighbors(i, j, colorArray[i][j], colorArray);

            if (neighborCount > 1)
            {
              for (int a = 0; a < width; a++)
              {
                for (int b = 0; b < height; b++)
                {
                  if (neighborArray[a][b])
                  {
                    trackArray[a][b] = false;
                    groupArray[a][b] = false;
                  }
                }
              }
              groupArray[i][j] = true;
            }
          }
        }
      }
    }
    return groupArray;
  }
  
  // compute new configuration of board given a set of x-y coordinates
  public Board computeChild(int x, int y)
  {
    int width = this.width();
    int height = this.height();
    int depth = this.depth();
    int score = this.score();
    int[][] colorArray = this.colorArray(); // parent configuation
    int[][] childColorArray = new int[width][height]; // child configuration
    neighborArray = new boolean[width][height];
    int childScore;
    
    for (int i = 0; i < width; i++)
      for (int j = 0; j < height; j++)
        childColorArray[i][j] = colorArray[i][j];
    
    neighborCount = 0;
    initBooleanArray(neighborArray, false);
    getNeighbors(x, y, childColorArray[x][y], childColorArray);
    
    if (neighborCount > 1)
    {
      for (int i = 0; i < width; i++)
        for (int j = 0; j < height; j++)
          if (neighborArray[i][j]) childColorArray[i][j] = EMPTY;
      
      childColorArray = compact(childColorArray);
      childScore = score + ((neighborCount - 2) * (neighborCount - 2));
    }
    else
    {
      childScore = score;
    }
    return new Board(childColorArray, childScore, depth + 1, x, y);
  }
  
  // given a configuration compact board first top-down and then right-left
  private int[][] compact(int[][] colorArray)
  {
    int width = colorArray.length;
    int height = colorArray[0].length;
    int[] tempColumn = new int[height];
    // top down compaction
    for (int i = 0; i < width; i++)
    {
      int t = 0;
      for (int j = 0; j < height; j++)
        if (colorArray[i][j] !=EMPTY) tempColumn[t++] = colorArray[i][j];
      while (t < height) tempColumn[t++] = EMPTY;
      for (int j = 0; j < height; j++)
        colorArray[i][j] = tempColumn[j];
    }
    // right-left compaction
    for (int i = 0; i < (width - 1); i++)
    {
      if (colorArray[i][0] == EMPTY)
      {
        int t = i + 1;
        while ((t < width) && (colorArray[t][0] == EMPTY)) t++;
        if (t != width)
        {
          for (int j = 0; j < height; j++)
          {
            colorArray[i][j] = colorArray[t][j];
            colorArray[t][j] = EMPTY;
          }
        }
      }
    }
    return colorArray;
  }
  
  // get group of same color given a set of coordinates via breadth first search
  private void getNeighbors(int x, int y, int color, int[][] colorArray)
  {
    neighborArray[x][y] = true;
    neighborCount++;
    
    if ((x > 0) && !neighborArray[x-1][y] && (color == colorArray[x-1][y]))
      getNeighbors(x-1, y, color, colorArray);
    if ((x < (width-1)) && !neighborArray[x+1][y] && (color == colorArray[x+1][y]))
      getNeighbors(x+1, y, color, colorArray);
    if ((y > 0) && !neighborArray[x][y-1] && (color == colorArray[x][y-1]))
      getNeighbors(x, y-1, color, colorArray);
    if ((y < (height - 1)) && !neighborArray[x][y+1] && (color == colorArray[x][y+1]))
      getNeighbors(x, y+1, color, colorArray);
  }
  
  // utility to initialize boolean array
  private void initBooleanArray(boolean[][] anArray, boolean value)
  {
    for (int i = 0; i < anArray.length; i++)
      for (int j = 0; j < anArray[i].length; j++)
        anArray[i][j] = value;
  }
}

                
