/**
   GoodbyeWorld.java

   @author Jeffrey M. Radcliffe
   
   created: Mon Jan  8, 2001  4:12 PM
*/

import javax.swing.*;
import java.awt.*;

public class GoodbyeWorld
{
  public static void main(String[] args)
  {
    GoodbyeFrame frame = new GoodbyeFrame();
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.show();
  }
}

/**
   A frame that contains a message panel
*/
class GoodbyeFrame extends JFrame
{
  public GoodbyeFrame()
  {
    setTitle("Goodbye, Cruel World!");
    setSize(WIDTH, HEIGHT);

    // add panel to frame
    GoodbyePanel panel = new GoodbyePanel();
    Container contentPane = getContentPane();
    contentPane.add(panel);
  }

  public static final int WIDTH = 300;
  public static final int HEIGHT = 300;
}

/**
   A panel that displays a message.
*/
class GoodbyePanel extends JPanel
{
  public void paintComponent(Graphics g)
  {
    super.paintComponent(g);

    g.drawString("Goodbye, world!", MESSAGE_X, MESSAGE_Y);
  }

  public static final int MESSAGE_X = 100;
  public static final int MESSAGE_Y = 100;
}

    
    
  
      
    
