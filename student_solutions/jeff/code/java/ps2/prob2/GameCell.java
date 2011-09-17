import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.event.*;
import java.util.*;
import java.awt.image.*;

/**
 GameCell.java
 All the cell requirements for the game of Same Game
 @author Jeffrey Radcliffe
 @version $Id: GameCell.java,v 1.9 2001/01/17 13:47:07 jeff Exp $
*/

class GameCell extends JPanel {
  GameCell(int i, int j, GameFrame frame)
  {
    // Set initial variables
    X = i; Y = j;
    this.frame = frame;
    this.state = state;

    // Determine if this cell is at the boundries of the board
    if(Y == frame.TOP) top = true;
    else if (Y == frame.BOTTOM) bottom = true;
    if(X == frame.LEFT) left = true;
    else if (X == frame.RIGHT) right = true;

    // Add a mouseListener to track actions
    addMouseListener(new MouseHandler()); 

    loadImages(frame.imagePackage.getImages());
    reset();
  }
 
 /**
     Loads a ImagePackage
     @param name A String with image file locations
  */
  public void loadImages(String[] name) {
    setBackground(frame.imagePackage.getColor());
    MediaTracker tracker = new MediaTracker(this);
    for (int i = 0; i < 6; i++) 
    {
      image[i] = Toolkit.getDefaultToolkit().getImage(name[i]);
      tracker.addImage(image[i], 0);
      try { tracker.waitForID(0); }
      catch(InterruptedException e) {}
    }
  }
 
  /**
     Repaints the cells according to state
  */
  public void paintComponent(Graphics g) {
    super.paintComponent(g);

    if(livingCell && !highlighted) {
      if(state == STATE_1)
        g.drawImage(image[0], 0, 0, null);
      if(state == STATE_2)
        g.drawImage(image[1], 0, 0, null);
      if(state == STATE_3)
        g.drawImage(image[2], 0, 0, null);
    }
    if(livingCell && highlighted) {
      if(state == STATE_1)
        g.drawImage(image[3], 0, 0, null);
      if(state == STATE_2)
        g.drawImage(image[4], 0, 0, null);
      if(state == STATE_3)
        g.drawImage(image[5], 0, 0, null);
    }
  }

  /**
     Mouse handler class controls mouse actions
  */
  private class MouseHandler extends MouseAdapter
  {
    public void mousePressed(MouseEvent event) {
      // If there is more than 1 block in the current grouping
      if(checkValidMove()) 
      {
        /*
          This is where the majority of the action takes
          place. Each click will carry through
          a complete cycle of activity.
        */
        frame.backup();
        counter = 0;               // Reset the counter
        sendMessage(DIE, state);   // Ask the cells to die
        frame.addToScore(counter); // Update the score
        cellActionCompress();      // Compress the board

        // after compressing, make sure the block is highlighted if it needs to be.
        frame.resetCellMarks();
        setHighlight(2,state);

        // check to see if the game is over
        if(frame.cell[frame.LEFT][frame.BOTTOM].checkEndGame())
          frame.gameOver();
      }
    }

    public void mouseEntered(MouseEvent event) {
      setHighlight(2, state);
    }
    public void mouseExited(MouseEvent event) {
      setUnhighlight(state);
    }
  }

  /**
     Asks a cell to propogate a message asking if it has neigbors.
     @return true if result is > 1, false if <= 1.
  */
  public boolean checkValidMove()
  {
      if (state == DEAD) return false;
      frame.resetCellMarks();
      counter = 0;
      sendMessage(MARK, state);
      if(counter > 1) return true;
      else 
        return false;
  }

  /**
     Check to see if there are any valid moves left
     @return true if game is over, false otherwise
  */
  private boolean checkEndGame() {
    if(frame.checkEndGame()) {
      if(frame.cell[frame.LEFT][frame.BOTTOM].state == DEAD)
        frame.addBonus(); 
      return true;
    }
    return false;
  }

  /**
     Gets the current total of cells in a given state
     @param choice The state queried
     @return The number of living cells in that state
  */
  public static int getState(int choice)
  {
    if(choice == 1)
      return totalState1;
    else if(choice == 2)
      return totalState2;
    else if(choice == 3)
      return totalState3;
    else return 0;
  }

  public static void setState(int choice, int value)
  {
    if(choice == 1)
      totalState1 = value;
    else if(choice == 2)
      totalState2 = value;
    else if(choice == 3)
      totalState3 = value;
  }


  public int getState()
  {
    return state;
  }

  /**
     Resets the score.
  */
  public void resetAllCells() {
    setUnhighlight(state);
    totalState1 = totalState2 = totalState3 = 0;
    sendMessage(RESET, 0);
    frame.repaintAllCells();
  }
  
  public void set(int c)
  {
    if(c == DEAD)
      kill();
    else
      reanimate(c);
  }

  private void reset() {
    double newColor = (double)Math.random();
    if(newColor < .33333333333) 
      state = STATE_1;
    else if (newColor < .6666666666) 
      state = STATE_2;
    else 
      state = STATE_3;

    reanimate(state);
  }

  /**
     Method responsible for highlighting the current block
     of cells
     @param count How many cells are needed for block
     @param testState Which state the adjacent cells need to be
  */
  private void setHighlight(int count, int testState) {
    counter = 0;
    frame.resetCellMarks();
    sendMessage(MARK, testState);
    frame.resetCellMarks();
    frame.reportSelectedBlocks(counter);
    if(counter >= count){
      sendMessage(HIGHLIGHT, testState);
    }
    frame.resetCellMarks();
  }

  /**
     Unhighlights cells
     @param revert The state to return to
  */
  private void setUnhighlight(int revert) {
    counter = 0;
    frame.reportSelectedBlocks(0);
    frame.resetCellMarks();
    sendMessage(UNHIGHLIGHT, revert);
    frame.resetCellMarks();
  }

  /**
     A message passing method. Upon receving a message,
     the cell will carry out the action and propogate
     the message to its neighbors, if needed.
  */
  private void sendMessage(int message, int stateInput)
  {
    int newMessage = message;
    int newState = state;
    boolean goNorth = true;
    if(top) goNorth = false;
    boolean goEast = true;
    if(right) goEast = false;
    boolean goSouth = true;
    if(bottom) goSouth = false;
    boolean goWest = true;
    if(left) goWest = false;

    // die message
    if(message == DIE) {
      if(state != stateInput) return;
      highlighted = false;
      kill();                   // die plz thx
      counter++;
    }

    // mark message
    if(message == MARK) {
      if(markedCell || !livingCell || state != stateInput) return;
      markedCell = true;
      counter++;
    }

    // highlight message
    if(message == HIGHLIGHT) {
      if(state != stateInput || markedCell) return;
      highlighted = true;
      markedCell = true;
      repaint();
    }

    // unhighlight message
    if(message == UNHIGHLIGHT) {
      if(state != stateInput || markedCell) return;
      highlighted = false;
      markedCell = true;
      repaint();
    }
    // Conditionals for reset message
    if(message == RESET) {
      if(markedCell) return;
      markedCell = true;
      reset();
    }
    
    // Propogate the message onwards, if possible!
    if(goNorth)
      nextCell(NORTH).sendMessage(newMessage, newState);
    if(goEast)
      nextCell(EAST).sendMessage(newMessage, newState);
    if(goSouth)
      nextCell(SOUTH).sendMessage(newMessage, newState);
    if(goWest)
      nextCell(WEST).sendMessage(newMessage, newState);
    return;
  } // end method sendMessage(int, int)

  /**
     Handy method for calling the next cell in a particular direction
     @param c The direction
     @return the cell in that direction
  */
  private GameCell nextCell(int c) {
    int x = X;
    int y = Y;
    if(c == NORTH) y--;
    else if(c == SOUTH) y++;
    else if(c == EAST) x++;
    else if(c == WEST) x--;
    return frame.cell[x][y];
  }
    
  // ------------------------------------------------------------------------
  //             BEGIN COMPRESSION BLOCK
  // ------------------------------------------------------------------------

  /**
     This is the control method for the compress routine. First it makes sure
     there are no gaps vertically in any of the columns. If compressVertical()
     finds any completely empty columns, it marks a flag, and compressHorizontal()
     takes care of shifting the cells to the left.
  */
  private void cellActionCompress() {
    needsShift = false;
    frame.cell[frame.LEFT][frame.BOTTOM].compressVertical(); // compress vert.
    while(needsShift) {
      frame.cell[frame.LEFT][frame.BOTTOM].compressHorizontal(); // and horiz.
    }
  }

  // Vertical compress control method
  private void compressVertical() {
    if(livingCell) {            // Cell is alive
      if(! top) 
      {
        nextCell(NORTH).compressVertical();    // propogate above
        if(bottom && ! right)
          nextCell(EAST).compressVertical();   // and right
      }
      return;
    }
      
    // cell is dead, is it the top?
    if(top) return;

    // ask for swap above if not.
    int tempState = nextCell(NORTH).swapVert();
    if(tempState == DEAD) {       
      if(bottom)
      {

        /* We went all the way up the column and back, down
         * which means this column is totally empty; need
         * compress horizonally
         */
        if(right) return;
        needsShift  = true;
      }
    }
    else {
      reanimate(tempState);                // Got a swap, reanimate ...
      nextCell(NORTH).compressVertical();  // And propogate above
    }
    if(bottom && ! right)
      nextCell(EAST).compressVertical(); // and right
  }
  
  // Horizontal compress control method
  private void compressHorizontal() {
    if(X == frame.RIGHT) {
      needsShift = false;       // We are at the right side of the screen,
      return;                   // Which means we are done
    }

    if(livingCell)                         // If this cell is alive...
      nextCell(EAST).compressHorizontal(); // Pass the message on
    
    // If we've gotten to here the column is dead and not the rightmost
    // Let's find the next column that has stuff in it
    int column = locateNextColumn(); 
    if(column == NULL) needsShift = false; // Ah! All clear from here

    // Ask the cells in that column to swap with this one...
    else frame.cell[X][frame.BOTTOM].migrate(column);
  }

  /**
     Asks a column of cells to switch states with the target column
     @param targetColumn The target column
  */
  private void migrate(int targetColumn) {
    int newState = swapRight(targetColumn);
    if (newState != DEAD)
      reanimate(newState);
    if(Y == frame.TOP) return;
    nextCell(NORTH).migrate(targetColumn);
  }

  /**
     Tries to swap its (living) state with the target column.
     @param target The target column
     @return the state of the cell in target column
  */
  private int swapRight(int target) {
    if(X != target && !right) {
      int tempState = nextCell(EAST).swapRight(target);
      return tempState;
    }
    return kill();
  }

  /**
   * Find the next column to the right, and pass back the
   * Column number
   */
  private int locateNextColumn() {
    if(livingCell) return X;
    if(right) return NULL;
    int tempColumn = nextCell(EAST).locateNextColumn();
    return tempColumn;
  }

  /**
     Attempts to have a cell swap its state with the next non-dead vertical cell.
     If if cannot do this (which means there is only deadness above), it returns
     DEAD.
     @return The state of the next non-dead vertical cell.
  */
  private int swapVert() {
    if(livingCell)
      return kill();              
    if(top) return DEAD;
    int tempState = nextCell(NORTH).swapVert();
    return tempState;
  }

  // ------------------------------------------------------------------------
  //                  END COMPRESSION BLOCK
  // ------------------------------------------------------------------------

  /**
     Resets the mark
  */
  public void resetMark()
  {
    markedCell = false;
  }
  
  public void resetHighlight()
  {
    highlighted = false;
  }

  /**
     Removes a cell from play
     @return the previous state of the cell
  */
  private int kill() {
    int oldState = state;
    if(state == STATE_1)
      totalState1--;
    else if(state == STATE_2)
      totalState2--;
    else if(state == STATE_3)
      totalState3--;
    
    state = DEAD;               // state of death
    livingCell = false;
    repaint();
    return oldState;
  }

  /**
     Brings a cell into play
  */
  private  void reanimate(int c) {
    state = c;
    if(state == STATE_1)
      totalState1++;
    else if(state == STATE_2)
      totalState2++;
    else if(state == STATE_3)
      totalState3++;
    livingCell = true;
    repaint();
  }

  public String toString() {
    String temp = "Cell (" + X + "," + Y + ")";
    if (! livingCell) temp += " is dead.";
    else temp += " is of state " + state + ".";
    return temp;
  }

  // --------------------------------------------------------------------------
  //                FIELDS
  // --------------------------------------------------------------------------

  // Immutable fields
  private int X;
  private int Y;
  private GameFrame frame;
  private Image[] image = new Image[6];
  private boolean top = false;
  private boolean bottom = false;
  private boolean left = false;
  private boolean right = false;

  
  // Mutable fields
  private int state;
  private boolean livingCell = false;
  private boolean markedCell = false;
  private boolean highlighted = false;
  
  // Static fields

  private static int counter;
  private static int totalState1 = 0;
  private static int totalState2 = 0;
  private static int totalState3 = 0;
  private static boolean needsShift = false;
  
  // States
  public static final int NULL = -1;
  public static final int DEAD = 0;
  public static final int STATE_1 = 1;
  public static final int STATE_2 = 2;
  public static final int STATE_3 = 3;
  
  // Static Messages
  public static final int DIE = 0;
  public static final int MARK = 1;
  public static final int HIGHLIGHT = 2;
  public static final int UNHIGHLIGHT = 3;
  public static final int RESET = 4;

  // Directions
  public static final int NORTH = 0;
  public static final int EAST = 1;
  public static final int SOUTH = 2;
  public static final int WEST = 3;
}
