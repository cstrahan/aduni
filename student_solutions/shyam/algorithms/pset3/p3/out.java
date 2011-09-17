/**
   Algorithms: PSet 3 - Problem 3
   The SameGame game
   Shyam Visweswaran
   Files: SameGame.java, Game.java, Board.java. The first two were written for the Java course pset
   and I decided to minimally modify them and implement a new Board.java for this exercise. As a result
   there is some duplication of methods between Game.java and Board.java. SameGame is the GUI portion
   and was minimally modified to include a Hint menu. Game.java has a couple of methods including
   hint and dfs (see later) that were added to implement the best possible move. Board.java is totally new and implements
   the board object that is used byte the depth-first-search (named dfs) method in Game.java.

   The Game.java
*/
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

/**
   Contains the main routine that starts the game.
*/
public class SameGame
{
  public static void main(String[] args)
  {
    Game game = new Game();
    ImageFrame frame = new ImageFrame(game);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.show();
  }
}

/**
   ImageFrame hardcodes the frame size. To the height add 
   space for menubar (25 pixels) and 40 pixels for space to
   display scores.
*/
class ImageFrame extends JFrame
{
  public ImageFrame(Game game)
  {
    setTitle("Same Game");
    setSize(game.getWidth() * 40, (game.getHeight() * 40) + 25 + 40);
    ImagePanel panel = new ImagePanel(game, this); // pass game and frame to panel
    panel.setBackground(Color.white);
    Container contentPane = getContentPane();
    contentPane.add(panel);
  }
}

/**
   ImagePanel does all the GUI stuff in a single panel
*/

class ImagePanel extends JPanel
{
  private Image[] images = new Image[6];
  private Game game;
  private int imageWidth = 40;
  private int imageHeight = 40;
  private int arrayWidth;
  private int arrayHeight;
  private JMenuItem newGame;
  private JMenuItem undoGame;
  private JMenuItem hintMove;
  private JMenuItem exitGame;

  public ImagePanel(Game game, ImageFrame frame)
  {  
    this.game = game;
    this.arrayWidth = game.getWidth(); // get array width
    this.arrayHeight = game.getHeight(); // get array height
        
    // Menu stuff
    JMenuBar mb = new JMenuBar();
    frame.setJMenuBar(mb);
    JMenu gameMenu = new JMenu("Game"); // Game is a top level menu
    mb.add(gameMenu);
    JMenu optionsMenu = new JMenu("Options");
    mb.add(optionsMenu);
    
    newGame = gameMenu.add("New"); // New is an item in Game
    undoGame = optionsMenu.add("Undo"); // Undo is an item in Game
    hintMove = optionsMenu.add("Hint");
    exitGame = gameMenu.add("Exit"); // Exit is an item in Game

    // Menu and Mouse listeners
    MenuHandler m = new MenuHandler();
    newGame.addActionListener(m);
    undoGame.addActionListener(m);
    hintMove.addActionListener(m);
    exitGame.addActionListener(m);
    addMouseListener(new MouseHandler());
    addMouseMotionListener(new MouseMotionHandler());
    gameMenu.addMenuListener(new FileMenuListener());
    optionsMenu.addMenuListener(new FileMenuListener());
    
    // load images that are used for display
    Toolkit toolkit = Toolkit.getDefaultToolkit();
    for (int i = 0; i < 6; i++)
      images[i] = toolkit.getImage("images/image" + i + ".gif");
    
    // this makes sure that all the images are loaded into the panel before painting
    MediaTracker tracker = new MediaTracker(this);
    for (int i = 0; i < 6; i++)
      tracker.addImage(images[i], i);

    // event loop
    try  { tracker.waitForAll(); }
    catch (InterruptedException exception) { }
  }

  /** 
      Inner class that handles the menu items
  */
  class MenuHandler implements ActionListener
  {
    public void actionPerformed(ActionEvent evt)
    { // for new game re-initialize the board
      if (evt.getSource() == newGame) 
      {
        game.restartGame();
        repaint();
      } // undo 1 move
      else if (evt.getSource() == undoGame)
      {
        game.undoGame();
        repaint();
      }
      else if (evt.getSource() == hintMove)
      {
        game.hint();
        repaint();
      } // exit game
      else if (evt.getSource() == exitGame)
      {
        System.exit(0);
      }
    }
  }
  
  /**
     Inner class that gets mouse presses and calls routine
     to delete cells after appropiately scaling the coordinates
  */
  class MouseHandler extends MouseAdapter
  {
    public void mousePressed(MouseEvent evt)
    {
      int x = evt.getX();
      int y = evt.getY();
      // delete cells of the same color
      game.delNeighbors(xDisplayToArray(x), yDisplayToArray(y));
      repaint();
    }
    public void mouseExited(MouseEvent evt)
    { // if outside frame boundaries remove any highlighting in the cells
      game.mouseOutside();
      repaint();
    }
  }

  /**
     Inner class that gets mouse movements and highlights neighboring cells
     of the same color.
  */
  class MouseMotionHandler implements MouseMotionListener
  {
    public void mouseDragged(MouseEvent evt) { } // mouse drags not implemented
    public void mouseMoved(MouseEvent evt)
    {
      int x = evt.getX();
      int y = evt.getY();
      // compute neighbors of same color and highlight them
      game.updateNeighbors(xDisplayToArray(x), yDisplayToArray(y));
      repaint();
    }
  }

  /**
     Inner class to enable/disable menu items.
  */
  class FileMenuListener implements MenuListener
  {
    public void menuSelected(MenuEvent evt)
    { // disable undo if done once already
      undoGame.setEnabled(game.isUndoPossible());
      hintMove.setEnabled(!game.gameEnd());
    }
    public void menuDeselected(MenuEvent evt) { }
    public void menuCanceled(MenuEvent evt) { }
  }
  

  /**
     Following two methods convert frame coordinates to array indices.
  */
  // X-axis is easy - just scale by width of the image
  private int xDisplayToArray(int x)
  {
    return x / imageWidth;
  }
  // Y-axis is painful - while the origin for the frame starts at the upper left hand
  // corner, for the array it starts at the lower left hand corner. This is is the result
  // of coding the data model first without knowing how java represents window coordinates
  private int yDisplayToArray(int y)
  { // subtract 1 to start from zero and add 1 to shift down by 1 unit to
    // account for space for scores
    return (((arrayHeight - 1) + 1) - (y / imageHeight));
  }

  /**
     Following 2 methods to adjust the array indices to account space
     for scores at the top of the frame
  */
  private int xCoordinateAdjust(int x)
  {
    return x;
  }
  private int yCoordinateAdjust(int y)
  { // Subtract 1 to start from zero and add 1 to shift down by 1 unit to
    // account for space for scores
    return ((arrayHeight - 1) - y + 1);
  }

  /**
     Do the actual painting on to the frame.
  */
  public void paintComponent(Graphics g)
  {
    super.paintComponent(g);
    // Total score is the running total;
    //Possible Points is the possible score for the current
    // highlighted group of cells
    g.drawString("Total Score " + game.getTotalScore(), 20, 20);
    if (game.gameEnd()) g.drawString("Game Over", 150, 20);
    else g.drawString("Possible Points " + game.getNeighborScore(), 150, 20);
    
    // display the images
    for (int i = 0; i < arrayWidth; i++)
      for (int j = 0; j < arrayHeight; j++)
      { // color array codes for cell colors with integers 1 -3 
        int color = game.getColorArray()[i][j];
        // if image(i) is the regular image then, image(i-1) is the corresponding highlighted image
        int regularImage = (color * 2) - 1; // non-highlighted image
        int highlightedImage = regularImage - 1; // highlighted image
        // neighbor array codes for 'connected' cells as true
        boolean neighbor = game.getNeighborArray()[i][j];
         // paint only cells that are non-empty
        if (color != game.EMPTY)
        { // paint the 'connected' neighbors with highlighted images; otherwise use regular images
          if (neighbor)
            g.drawImage(images[highlightedImage], xCoordinateAdjust(i) * imageWidth, yCoordinateAdjust(j) * imageHeight, null);
          else 
            g.drawImage(images[regularImage], xCoordinateAdjust(i) * imageWidth, yCoordinateAdjust(j) * imageHeight, null);
        }
      }  
  }
}

  








    
    
        
        
        

    
    
  
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

                
