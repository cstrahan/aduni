/**
 * SameJava -- a same-gnome/same-game clone
 * This file is ScorePanel.java, which maintains the game score.
 *
 * @author Jeffrey M. Radcliffe
 * @version $Id: ScorePanel.java,v 1.7 2001/01/17 15:26:34 jeff Exp $
 *
*/
import javax.swing.*;
import java.awt.*;

class ScorePanel extends JPanel {

  /**
     Constructor
     @param parent Parent frame
  */
  ScorePanel(GameFrame parent) {
    score = 0;
    this.parent = parent;
    initialize();
  }

  public void initialize() 
  {
    // Grab label information from the current imagePackage
    this.label = parent.imagePackage.getShortDescription();
    // Ditto with background color info.
    setBackground(parent.imagePackage.getScorePanelColor());
  }

  /**
     Ye Olde paintComponent does all the dirtywork
  */
  public void paintComponent(Graphics g) {
    super.paintComponent(g);
    String message;

    // Always score the score
    message = "Score: " + score;
    g.drawString(message, 520, 15);

    // Draw itemizer information if requested
    if(parent.itemizerItem.isSelected()) {
      message = label[0] + ": " +
        parent.getState(1) + 
        " " + label[1] + ": " +
        parent.getState(2) +
        " " + label[2] + ": " +
        parent.getState(3);
      g.drawString(message, 15,15);
    }

    // Draw game over message if the game is over
    if(isGameOver) {
      g.drawString("Game Over.", 250, 15);
      return;
    }

    // Draw active blocks if requested
    if(parent.selectedItem.isSelected() && blocks > 1)
    {
      message = "Currently " + blocks + " selected";
      g.drawString(message,250, 15);
    }
  }

  //  The rest of the methods are minor utility functions

  /**
     Adds points to the board
     @param addedPoints How many points to be added
  */
  public void addPoints(int addedPoints) {
    score += addedPoints;
    repaint();
  }

  /**
     Sets the number of active blocks
     @param blocks Active blocks
  */
  public void activeBlocks(int blocks) {
    this.blocks = blocks;
    repaint(); 
  }

  /**
     Adds the endgame bonus
  */
  public void addBonus() {
    score += BONUS;
    repaint();
  }

  /**
     Gets the current score (used mostly for undo)
     @return the score
  */
  public int getScore() {
    return score;
  }

  /**
     Sets the score to an arbitrary value (mostly
     for undo).
     @param score the new score
  */
  public void setScore(int score) {
    this.score = score;
    repaint();
  }
  
  /**
     Sets the game as over
  */
    public void setGameOver() {
    isGameOver = true;
    repaint();
  }

  /**
     Resets the panel
  */
  public void reset() {
    isGameOver = false;
    score = 0;
    repaint();
  }

  // Fields...
  public static final int BONUS = 1000; 
  private String[] label;
  private int score;
  private int blocks;
  private GameFrame parent;
  private boolean isGameOver = false;
}
