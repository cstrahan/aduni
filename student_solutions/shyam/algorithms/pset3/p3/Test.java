/*
 * Test.java
 */

public class Test 
{
  public static final int EMPTY = 0;
  public static final int MAX_DEPTH = 4;
  int width = 1;
  int height = 1;
  int totalScore = 1;
  int[][] colorArray = new int[1][1];
  private Board[] path;
  private boolean[] tracker;
  
  public void hint()
  {
    int[][] emptyArray = new int[width][height];
    for (int i = 0; i < width; i++)
      for (int j = 0; j < height; j++)
        emptyArray[i][j] = -100;
    
    path = new Board[MAX_DEPTH + 1];
    for (int i = 0; i < path.length; i++)
      path[i] = new Board(emptyArray, -100, -100, -100, -100);
    
    tracker = new boolean[MAX_DEPTH + 1];
    for (int i = 0; i < tracker.length; i++)
      tracker[i] = false;
    
    Board board = new Board(colorArray, totalScore, 0, 0, 0);
    dfs(board);
    // delNeighbors(path[0].originX, path[0].originY);
  }
  
  public void dfs(Board board)
  {
    if (board.depth() == MAX_DEPTH)
    {
      if (board.score() > path[board.depth()].score())
        tracker[board.depth() - 1] = true;
    }
    else
    {
      boolean[][] groupArray = board.computeGroups();
      for (int i = 0; i < width; i++)
      {
        for (int j = 0; j < height; j++)
        {
          if (groupArray[i][j])
            dfs(board.computeChild(i, j));
        }
      }
    }
    
    if (tracker[board.depth()])
    {
      path[board.depth()] = board;
      if (board.depth() > 0) tracker[board.depth() - 1] = true;
      tracker[board.depth()] = false;
    }
  }
}
