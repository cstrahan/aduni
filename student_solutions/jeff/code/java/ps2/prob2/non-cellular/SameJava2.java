/**
 *   SameJava -- a same-gnome/same-game clone
 * @author Jeffrey M. Radcliffe
 * @version $Id: SameJava2.java,v 1.1 2001/01/14 23:09:20 jeff Exp $
 *
 * TO DO:
 * New Game
 * Status Bar
 * About Panel
 * Game Over
*/

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.event.*;
import java.util.*;
import java.io.*;
import java.awt.image.*;

public class SameJava2
{
  public static void main(String[] args) {
    GameFrame frame = new GameFrame();
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.show();
  }
}

/**
   A frame will contain our game board
*/
class GameFrame extends JFrame
{
  public GameFrame() {
    setTitle("SameJava v.2");

    setSize(WIDTH, HEIGHT);
    
    // Create the menu bars
    JMenu fileMenu = new JMenu("File");
    fileMenu.setMnemonic('F');

    AbstractAction newAction = new
      AbstractAction("New Game")
      {
        public void actionPerformed(ActionEvent event)
        {
          gamePanel.resetGame();
          scorePanel.reset();
        }
      };
    newAction.putValue(Action.MNEMONIC_KEY, new Integer('N'));
    fileMenu.add(newAction);
    
    AbstractAction exitAction = new
      AbstractAction("Quit")
      {
        public void actionPerformed(ActionEvent event)
        {
          System.exit(0);
        }
      };

    exitAction.putValue(Action.MNEMONIC_KEY, new Integer('Q'));
    
    fileMenu.add(exitAction);

    // Options menu
    JMenu optionsMenu = new JMenu("Options");
    optionsMenu.setMnemonic('O');
    optionsMenu.addMenuListener(new OptionsMenuListener());
    itemizerItem = new JCheckBoxMenuItem("Itemizer", false);
    optionsMenu.add(itemizerItem);

    selectedItem = new JCheckBoxMenuItem("Selected", false);
    optionsMenu.add(selectedItem);


    // Help menu
    JMenu helpMenu = new JMenu("Help");
    helpMenu.setMnemonic('H');
    helpMenu.add(new
      AbstractAction("Help")
      {
        public void actionPerformed(ActionEvent event)
        {
        }
      });
    JMenuItem aboutItem = new JMenuItem("About");
    aboutItem.addActionListener(new
      ActionListener()
      {
        public void actionPerformed(ActionEvent event)
        {
          if(dialog == null) // first time
            dialog = new AboutDialog(GameFrame.this);
          dialog.show();
        }
      });
    helpMenu.add(aboutItem);
                                         
    JMenuBar menuBar = new JMenuBar();
    setJMenuBar(menuBar);
    menuBar.add(fileMenu);
    menuBar.add(optionsMenu);
    menuBar.add(helpMenu);

    // Create the content Frame
    Container contentPane = getContentPane();
    contentPane.setLayout(null);

    // Add a score panel
    scorePanel = new ScorePanel(this);
    scorePanel.setBounds(0, 0, 600, 25);
    contentPane.add(scorePanel);
    
    // Add the game panel
    gamePanel = new GamePanel(this);
    gamePanel.setBounds(0, 25, 600, 400);
    contentPane.add(gamePanel);
  }

  
  public int getState(int choice)
  {
    if(choice == 1)
      return gamePanel.getState(1);
    else if(choice == 2)
      return gamePanel.getState(2);
    else if(choice == 3)
      return gamePanel.getState(3);
    else return 0;
  }
  
  public int getScore() {
    return scorePanel.getScore();
  }

  public void gameOver() {
    scorePanel.setGameOver();
  }
    
  public void addToScore(int points) {
    if(points > 2) {
      scorePanel.addPoints((points - 2) * (points - 2));
    }
    else scorePanel.repaint();
  }
  public void addBonus() {
    scorePanel.addBonus();
  }

  public void reportSelectedBlocks(int blocks)
  {
    scorePanel.activeBlocks(blocks);
  }

  public static final int WIDTH = 600;
  public static final int HEIGHT = 450;

  private GamePanel gamePanel;
  private ScorePanel scorePanel;
  private static int score = 0;

  private AboutDialog dialog;
  public  JCheckBoxMenuItem itemizerItem;
  public JCheckBoxMenuItem selectedItem;

  // Watches the options menu and sets gameplay variables accordingly
  private class OptionsMenuListener implements MenuListener
  {
    public void menuSelected(MenuEvent event) {}
    public void menuDeselected(MenuEvent event)
    {
      scorePanel.repaint();
    }
    public void menuCanceled(MenuEvent event) {
      scorePanel.repaint();
    }
  }
}


