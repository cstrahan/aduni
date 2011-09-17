/**
 * SameJava -- a same-gnome/same-game clone
 * This file is GameFrame.java, which is the game board.
 *
 * @author Jeffrey M. Radcliffe
 * @version $Id: GameFrame.java,v 1.8 2001/01/17 18:50:59 jeff Exp $
 *
*/

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.event.*;
import java.util.*;

/**
   A frame which will contain our game board
*/
class GameFrame extends JFrame
{
  public GameFrame(int w, int h) {
    // sets width and height parameters passed in from the main
    this.WIDTH = w;
    this.HEIGHT = h;
    this.BOTTOM = HEIGHT - 1;
    this.RIGHT = WIDTH - 1;
    this.LEFT = 0;
    this.TOP = 0;

    // Set a title, and an icon
    setTitle("Same Java");
    Toolkit kit = Toolkit.getDefaultToolkit();
    Image img = kit.getImage("images/icon.gif");
    setIconImage(img);
    setSize(WIDTH * CELL_SIZE, (HEIGHT * CELL_SIZE + 50));
    setResizable(false);

    // **********************************************************
    // BEGIN HIDEOUS MENU BAR CREATION BLOCK
    // **********************************************************
    
    // FILE MENU
    JMenu fileMenu = new JMenu("File");
    fileMenu.setMnemonic('F');

    AbstractAction newAction = new
      AbstractAction("New Game") {
        public void actionPerformed(ActionEvent event) {
          resetCells();
        }
      };

    newAction.putValue(Action.MNEMONIC_KEY, new Integer('N'));
    fileMenu.add(newAction);
    
    AbstractAction exitAction = new
      AbstractAction("Quit") {
        public void actionPerformed(ActionEvent event) {
          System.exit(0);
        }
      };

    exitAction.putValue(Action.MNEMONIC_KEY, new Integer('Q'));
    fileMenu.add(exitAction);

    // OPTIONS MENU
    JMenu optionsMenu = new JMenu("Options");
    optionsMenu.setMnemonic('O');
    optionsMenu.addMenuListener(new OptionsMenuListener());

    undoItem = optionsMenu.add(new
      AbstractAction("Undo") {
        public void actionPerformed(ActionEvent event) {
          undo();
        }
      });
    optionsMenu.add(undoItem);

    itemizerItem = new JCheckBoxMenuItem("Itemizer", false);
    optionsMenu.add(itemizerItem);

    selectedItem = new JCheckBoxMenuItem("Selected", false);
    optionsMenu.add(selectedItem);
    
    optionsMenu.addSeparator();
    
    /*
     * The following sub-menu items change the game theme, using
     * the method newImagePackage(ImagePackage)
     */
    JMenu themesMenu = new JMenu("Themes");
    themesMenu.add(new
      AbstractAction("Gems") {
        public void actionPerformed(ActionEvent event) {
          newImagePackage(new GemImagePackage());
        }
      });
    themesMenu.add(new
      AbstractAction("Flowers") {
        public void actionPerformed(ActionEvent event) {
          newImagePackage(new FlowerImagePackage());
        }
      });
    themesMenu.add(new
      AbstractAction("Elegant") {
        public void actionPerformed(ActionEvent event) {
          newImagePackage(new ElegantImagePackage());
        }
      });
    themesMenu.add(new
      AbstractAction("OSes") {
        public void actionPerformed(ActionEvent event) {
          newImagePackage(new OSImagePackage());
        }
      });
    themesMenu.add(new
      AbstractAction("Matrix") {
        public void actionPerformed(ActionEvent event) {
          newImagePackage(new MatrixImagePackage());
        }
      });

    optionsMenu.add(themesMenu);

    // HELP MENU
    JMenu helpMenu = new JMenu("Help");
    helpMenu.setMnemonic('H');

    helpMenu.add(new
      AbstractAction("Help") {
        public void actionPerformed(ActionEvent event) {
        }
      });

    JMenuItem aboutItem = new JMenuItem("About");
    aboutItem.addActionListener(new
      ActionListener() {
        public void actionPerformed(ActionEvent event) {
          if(dialog == null) // first time
            dialog = new AboutDialog(GameFrame.this);
          dialog.show();
        }
      });
    helpMenu.add(aboutItem);

    // Finally, construct the menu bar and add the menus to it.
    JMenuBar menuBar = new JMenuBar();
    setJMenuBar(menuBar);
    menuBar.add(fileMenu);
    menuBar.add(optionsMenu);
    menuBar.add(helpMenu);

    // **********************************************************
    // END HIDEOUS MENU BAR CREATION BLOCK
    // **********************************************************

    // Create the content Frame
    Container contentPane = getContentPane();
    contentPane.setLayout(null); // Yes, really a null layout.

    // Create the default image set
    imagePackage = new ElegantImagePackage();
    
    // Add and initialize the score panel
    scorePanel = new ScorePanel(this);
    scorePanel.setBounds(0, 0, CELL_SIZE * WIDTH, 25);
    contentPane.add(scorePanel);

    /* Create and layout the cells. Each of the cells is an
       autonomous JPanel, which contains the vast majority
       of the gameplay rules. */

    // initialize grid
    cell = new GameCell[WIDTH][HEIGHT];
    undo = new int[WIDTH][HEIGHT];
    oldStates = new int[3];
    
    for(int i = 0; i < cell.length; i ++)
      for(int j = 0; j < cell[0].length; j++) {
        cell[i][j] = new GameCell(i , j, this);
        cell[i][j].setBounds(i * CELL_SIZE, ((j * CELL_SIZE) + 25),
                             CELL_SIZE, CELL_SIZE);
      }

    // set the states
    resetCells();

    // Add cells to content pane
    for(int i = 0; i < cell.length; i ++)
      for(int j = 0; j < cell[0].length; j++)
        contentPane.add(cell[i][j]);
  } // end beastly constructor for GameFrame()

  /**
     Resets cells
  */
  private void resetCells() {
    canUndo = false;
    scorePanel.reset();   // clears score
    resetCellMarks();     // clears marks
    // Asks cells to reanimate randonmly...
    cell[LEFT][BOTTOM].resetAllCells(); 
  }
  
  /**
     Repaints the entire board (needed when resetting game)
  */
  public void repaintAllCells() 
  {
    for(int i = 0; i < cell.length; i ++)
      for(int j = 0; j < cell[0].length; j++) 
      {
        cell[i][j].resetHighlight();
        cell[i][j].resetMark();
        cell[i][j].repaint();
      }
  }

  /**
     Implements a new theme. Called from Options menu bar
  */
  private void newImagePackage(ImagePackage newPackage) {
    imagePackage = newPackage;
    for(int i = 0; i < cell.length; i ++)
      for(int j = 0; j < cell[0].length; j++)
        cell[i][j].loadImages(imagePackage.getImages());
    scorePanel.initialize();
    resetCells();
  }
    
  /**
     Is the game over?
     @return <code>true</code> to GameCells if game is over,
     <code>false</code> otherwise
  */
  public boolean checkEndGame() {
    for(int i = 0; i < cell.length; i ++) {
      for(int j = 0; j < cell[0].length; j++) { 
        if (cell[i][j].checkValidMove())
          return false;
      }
    }
    return true;
  }

  /**  Resets the cell markers. This is important.  */
  public void resetCellMarks() {
    for(int i = 0; i < cell.length; i++)
      for(int j = 0; j < cell[0].length; j++)
        cell[i][j].resetMark();
  }

  /* **********************************************************
   * The next few methods deal with passing information from
   * the GameCells to the ScorePanel, and vice versa
   ************************************************************/

  /**
     Passes information on to the ScorePanel
     @param blocks The number of blocks currently highlighted
  */
  public void reportSelectedBlocks(int blocks) {
    scorePanel.activeBlocks(blocks);
  }

  /**
     Passes the current number of each state from the cells to the
     ScorePanel
     @param choice the State( 1 , 2 or 3)
  */
  public int getState(int choice) {
    if(choice == 1)
      return GameCell.getState(1);
    else if(choice == 2)
      return GameCell.getState(2);
    else if(choice == 3)
      return GameCell.getState(3);
    else return 0;
  }
  
  /** Sends a game over message to the ScorePanel */
  public void gameOver() {
    canUndo = false;
    scorePanel.setGameOver();
  }

  /** Sends an add score message to the ScorePanel */
  public void addToScore(int points) {
    if(points > 2) {
      scorePanel.addPoints((points - 2) * (points - 2));
    }
  }

  /** Sends a bonus message to the ScorePanel */
  public void addBonus() {
    scorePanel.addBonus();
  }

  public void backup() 
  {
    for(int i = 0; i < cell.length; i++) 
      for(int j = 0; j < cell[0].length; j++) 
        undo[i][j] = cell[i][j].getState();

    for(int i = 0; i < 3; i++)
      oldStates[i] = GameCell.getState(i + 1);
    oldScore = scorePanel.getScore();
    canUndo = true;
  }

  public void undo() 
  {
    for(int i = 0; i < cell.length; i ++) 
      for(int j = 0; j < cell[0].length; j++) 
        cell[i][j].set(undo[i][j]);
    for(int i = 0; i < 3; i++)
      GameCell.setState(i + 1, oldStates[i]);
    scorePanel.setScore(oldScore);
    canUndo = false;
  }

  // -----------------------------------------------------------------
  // FIELDS
  // -----------------------------------------------------------------
  public static int WIDTH;
  public static int HEIGHT;
  public static int CELL_SIZE = 40;
  public static int TOP;
  public static int BOTTOM;
  public static int LEFT;
  public static int RIGHT;
  public static GameCell cell[][]; // 2d array to hold game cells

  public static int undo[][];
  public static int oldStates[];
  public static int oldScore;
  
  private ScorePanel scorePanel;
  public ImagePackage imagePackage; // the ImagePackage being used
  public static Color backgroundColor;
  private AboutDialog dialog;
  public  JCheckBoxMenuItem itemizerItem;
  public JCheckBoxMenuItem selectedItem;
  public JMenuItem undoItem;
  private boolean canUndo = false;
  /** 
      Watches the options menu and sets gameplay variables 
      accordingly
  */
  private class OptionsMenuListener implements MenuListener
  {
    public void menuSelected(MenuEvent event) {
      undoItem.setEnabled(canUndo);
    }
    public void menuDeselected(MenuEvent event) {
      // this is just so that @#(&^$! JAVA WILL REPAINT THE SCOREPANEL!!! UGH!
      scorePanel.repaint(); 
    }
    public void menuCanceled(MenuEvent event) {}
  }
}
