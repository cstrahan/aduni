/**
   PS-2 Problem 2 - Implementation of Same Game in Java
   There are 2 main parts to the code: classes SameGame, ImageFrame and
   ImagePanel implement the GUI part while class Game implements the data model.
   Functionality: 'balls' are gif images, highlighting of 'connected' balls,
   displays running total and possible points if highlighted region is deleted,
   detects end of game and adds bonus points if all balls have been deleted,
   menu bar has ability to start a new game, undo 1 step and exit game.
   @author Shyam Visweswaran
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
  private JMenuItem exitGame;

  public ImagePanel(Game game, ImageFrame frame)
  {  
    this.game = game;
    this.arrayWidth = game.getWidth(); // get array width
    this.arrayHeight = game.getHeight(); // get array height
        
    // Menu stuff
    JMenuBar mb = new JMenuBar();
    frame.setJMenuBar(mb);
    JMenu menu = new JMenu("Game"); // Game is a top level menu
    mb.add(menu);
    newGame = menu.add("New"); // New is an item in Game
    undoGame = menu.add("Undo"); // Undo is an item in Game
    exitGame = menu.add("Exit"); // Exit is an item in Game

    // Menu and Mouse listeners
    MenuHandler m = new MenuHandler();
    newGame.addActionListener(m);
    undoGame.addActionListener(m);
    exitGame.addActionListener(m);
    addMouseListener(new MouseHandler());
    addMouseMotionListener(new MouseMotionHandler());
    menu.addMenuListener(new FileMenuListener());
    
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
    { // if outside frame boundaries remove any highlighting int the cells
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

  








    
    
        
        
        

    
    
  
