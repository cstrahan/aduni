import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.event.*;
import java.util.*;
import java.io.*;
import java.awt.image.*;

public class GamePanel extends JPanel
{
  GamePanel(GameFrame parent)
  {
    this.parent = parent;
    setBackground(Color.white);
    loadImages(images);

    addMouseListener(new MouseHandler());
    addMouseMotionListener(new MouseMotionHandler());
    for(int i = 0; i < 15; i++)
      for(int j = 0; j < 10; j++) {
        board[i][j]  = initialize();
        highlight[i][j] = false;
        mark[i][j] = false;
      }
    repaint();
  }
  
  private int initialize() {
    double newColor = (double)Math.random();
    if(newColor < .33333333333) 
    {
      totalState1++;
      return STATE_1;
    }
    else if (newColor < .6666666666)
    {
      totalState2++;
      return STATE_2;
    }
    else
    {
      totalState3++;
      return STATE_3;
    }
  }

  public void paintComponent(Graphics g) {
    super.paintComponent(g);

    int imageNumber;
    for(int i = 0; i < 15; i++)
      for(int j = 0; j < 10; j++) 
      {
        int currentState = board[i][j];
        boolean isHighlighted = highlight[i][j];
        if(currentState != DEAD) {
          if(currentState == STATE_1) {
            if(isHighlighted)
              imageNumber = 3;
            else
              imageNumber = 0;
          }
          else if(currentState == STATE_2) {
            if(isHighlighted)
              imageNumber = 4;
            else
              imageNumber = 1;
          }
          else {
            if(isHighlighted)
              imageNumber = 5;
            else
              imageNumber = 2;
          }
          g.drawImage(image[imageNumber], i * 40, j * 40, null);
        }
      }
  }

    public void loadImages(String[] name) {
      MediaTracker tracker = new MediaTracker(this);
      for (int i = 0; i < 6; i++) 
      {
        image[i] = Toolkit.getDefaultToolkit().getImage(name[i]);
        tracker.addImage(image[i], 0);
        try { tracker.waitForID(0); }
        catch(InterruptedException e) {}
      }
    }

  public void resetGame()
  {
    totalState1 = 0;
    totalState2 = 0;
    totalState3 = 0;
    clearBoard();  
    for(int i = 0; i < 15; i++)
      for(int j = 0; j < 10; j++)
        board[i][j] = initialize();
    repaint();
  }
    
    public void clearBoard() {
    for(int i = 0; i < 15; i++)
      for(int j = 0; j < 10; j++) {
        mark[i][j] = false;
        highlight[i][j] = false;
      }
    repaint();
  }
    
  public void clearMarks() {
    for(int i = 0; i < 15; i++)
      for(int j = 0; j < 10; j++) {
        mark[i][j] = false;
      }
  }

  public boolean checkGameOver() {
    for(int j = 9 ; j > 0; j--) {
      for(int i = 0; i < 15; i++) {
        if(checkValidMove(i, j)) return false;
      }
    }
    if(board[0][9] == DEAD)
      parent.addBonus();
    return true;
  }

  private void kill() {
    clearBoard();               // Reset marks
    int score = findNeighbors(X, Y, board[X][Y], DIE); // Kill necessary blocks
    compress();                 // Compress the board

    if(checkValidMove(X,Y)) 
      highlight();              // Look for the new state under the mouse

    repaint();
    parent.addToScore(score);   // Add to the score
    if(checkGameOver())         // Check for end game
      parent.gameOver();
  }

  private void compress() {
    verticalCompress(0,9);
    while(needsHorizontalCompress)
      horizontalCompress(0,9);
  }

  private void verticalCompress(int x, int y) {
    int currentState = board[x][y];
    if(currentState != DEAD) {
      if(y != TOP) {
        verticalCompress(x, y - 1);
        if(y == BOTTOM && x != RIGHT)
          verticalCompress(x+1, y);
      }
      return;
    }

    // position is dead
    if(y == TOP) return;
    
    // ask for swap above if not top
    int tempState = swapVertical(x, y-1, currentState);
    if(tempState == DEAD) {
      if(y == BOTTOM)
      {
        if(x == RIGHT) return;
        needsHorizontalCompress = true;
      }
    }
    else {
      board[x][y] = tempState;
      verticalCompress(x, y-1);
    }
    if(y == BOTTOM && x != RIGHT)
      verticalCompress(x+1, y);
  }

  private int swapVertical(int x, int y, int c) {
    int currentState = board[x][y];
    if(currentState != DEAD)
    {
      board[x][y] = DEAD;
      return currentState;
    }

    if(y == TOP) 
    {
      return DEAD;
    }

    int tempState = swapVertical(x, y-1, c);
    return tempState;
  }

  private void horizontalCompress(int x, int y) {
    int currentState = board[x][y];  
    if(x == RIGHT) {
      needsHorizontalCompress = false;
      return;
    }
    if(currentState != DEAD) {
      horizontalCompress(x+1, y);
      return;
    }

    // Dead column, not right
    int column = locateNextColumn(x,y);
    if(column == -1) {
      needsHorizontalCompress = false;
      return;
    }
    for(int j = 0; j < 10; j++) {
      board[x][j] = board[column][j];
      board[column][j] = DEAD;
      repaint();
    }
  }

  private int locateNextColumn(int x, int y) {
    int currentState = board[x][y];  
    if(currentState != DEAD) return x;
    if(x == RIGHT) return -1;
    int tempColumn = locateNextColumn(x+1, y);
    return tempColumn;
  }
    
  private boolean checkValidMove(int x, int y)
  {
    clearBoard();
    int foo = findNeighbors(x, y, board[x][y], MARK);
    parent.reportSelectedBlocks(foo);
    repaint();
    if(foo > 1) return true;
    else return false;
  }
  
  private boolean checkValidMove() {
    return checkValidMove(X,Y);
  }

  private void highlight() {
    clearBoard();
    int foo =  findNeighbors(X, Y, board[X][Y], 1);
    repaint();
  }

  private int findNeighbors(int x, int y, int targetState, int message) {
   int currentState = board[x][y];
    if (mark[x][y] ||
        currentState == DEAD ||
        currentState != targetState)
      return 0;
    int temp = 0;
    if(currentState == targetState) {
      temp++;
      mark[x][y] = true;
      if(message == HIGHLIGHT) {
        highlight[x][y] = true;
      }
      if (message == DIE) {
        if(currentState == STATE_1)
          totalState1--;
        else if(currentState == STATE_2)
          totalState2--;
        else
          totalState3--;

        board[x][y] = DEAD;
      }
    }
    if(y != TOP)
      temp += findNeighbors(x, y-1, targetState, message);
    if(y != BOTTOM)
      temp += findNeighbors(x, y+1, targetState, message);
    if(x != LEFT)
      temp += findNeighbors(x-1, y, targetState, message);
    if(x != RIGHT)
      temp += findNeighbors(x+1, y, targetState, message);
    return temp;
  }

  private boolean setPosition(MouseEvent event) {
      // Calculate which cell is in focus
      int x = (int)Math.abs((event.getX() / 40));
      int y = (int)Math.abs((event.getY() / 40));
      
      // Check to see if we have moved tiles
      if(X == x && Y == y) return false;

      clearBoard();
      LAST_X = X;
      LAST_Y = Y;
      X = x;
      Y = y;
      return true;
  }


  public int getState(int choice)
  {
    if(choice == 1)
      return totalState1;
    else if(choice == 2)
      return totalState2;
    else if(choice == 3)
      return totalState3;
    else return 0;
  }

      

  // ************************************************************
  // FIELDS 
  // ************************************************************

  private static int[][] board = new int[15][10];
  private static String[] images = 
    { "images/tux.gif",
      "images/gnu.gif",
      "images/bsd2.gif",
      "images/tux_highlight.gif",
      "images/gnu_highlight.gif",
      "images/bsd2_highlight.gif"};

  private static boolean[][] highlight = new boolean[15][10];
  private static boolean[][] mark = new boolean[15][10];
  private static boolean needsHorizontalCompress = false;
  private Image[] image = new Image[6];

  private int X;
  private int Y;
  private int LAST_X;
  private int LAST_Y;

  // States
  public static final int NULL = -1;
  public static final int DEAD = 0;
  public static final int STATE_1 = 1;
  public static final int STATE_2 = 2;
  public static final int STATE_3 = 3;

  public static final int MARK = 0;
  public static final int HIGHLIGHT = 1;
  public static final int DIE = 2;

  public static int TOP = 0;
  public static int BOTTOM = 9;
  public static int LEFT = 0;
  public static int RIGHT = 14;
  public static int WIDTH = 600;
  public static int HEIGHT = 400;

  private static int totalState1, totalState2, totalState3 = 0;
  private GameFrame parent;
  private class MouseHandler extends MouseAdapter
  {
    public void mousePressed(MouseEvent event) {
      clearMarks();
      if(checkValidMove()) {
        clearMarks();
        kill();
      }
    }
    public void mouseEntered(MouseEvent event) {
      setPosition(event);
      clearBoard();
      if(checkValidMove())
        highlight();
    }

    public void mouseExited(MouseEvent event) {
      clearBoard();
      repaint();
    }
  }
  private class MouseMotionHandler implements MouseMotionListener
  {
    public void mouseMoved(MouseEvent event)
    {
      if(setPosition(event)) {
        if (X == LAST_X && Y == LAST_Y) return;

        if(checkValidMove()) {
             clearBoard();
          highlight();
          repaint();
        }
      }
    }
    public void mouseDragged(MouseEvent event) {}
  }
}
