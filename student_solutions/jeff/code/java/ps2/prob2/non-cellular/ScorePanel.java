import javax.swing.*;
import java.awt.*;

class ScorePanel extends JPanel {

  ScorePanel(GameFrame parent) {
    score = 0;
    setBackground(Color.white);
    this.parent = parent;
  }

  public void paintComponent(Graphics g) {
    super.paintComponent(g);
    String message;
    message = "Score: " + parent.getScore();
    g.drawString(message, 520, 15);

    if(parent.itemizerItem.isSelected()) 
    {
        message = "Tux: " + parent.getState(1) +
          "   Gnu: " + parent.getState(2) +
          "   Daemon: " + parent.getState(3);
        g.drawString(message, 15,15);
    }

    if(isGameOver) 
    {
      g.drawString("Game Over.", 250, 15);
      return;
    }

    if(parent.selectedItem.isSelected() && blocks > 1)
    {
      message = "Currently " + blocks + " selected";
      g.drawString(message,250, 15);
    }
  }

  public void addPoints(int addedPoints)
  {
    score += addedPoints;
    repaint();
  }

  public void activeBlocks(int blocks)
  {
    this.blocks = blocks;
    repaint();
  }

  public void addBonus()
  {
    score += BONUS;
    repaint();
  }

  public int getScore() 
  {
    return score;
  }
  
  public void setGameOver() 
  {
    isGameOver = true;
    repaint();
  }

  public void reset() 
  {
    isGameOver = false;
    score = 0;
    repaint();
  }

  public static final int BONUS = 1000;
  private int score;
  private int blocks;
  private GameFrame parent;
  private boolean isGameOver = false;
}
