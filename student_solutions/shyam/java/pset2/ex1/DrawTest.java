/**
   PS-2 Exercise 1 extension - Goodbye World in a circle
   @author Shyam Visweswaran
*/

import javax.swing.*;
import java.awt.*;
import java.awt.geom.*;

public class DrawTest
{
  public static void main(String[] args)
  {
    GoodbyeWorldFrame frame = new GoodbyeWorldFrame();
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.show();
  }
}

/**
   A frame with a message panel
*/
class GoodbyeWorldFrame extends JFrame
{
  public static final int WIDTH = 400;
  public static final int HEIGHT = 400;
  
  public GoodbyeWorldFrame()
  {
    setTitle("GoodbyeWorld in Rectangle");
    setSize(WIDTH, HEIGHT);
      
    // add panel to frame
    GoodbyeWorldPanel panel = new GoodbyeWorldPanel();
    panel.setBackground(Color.green);    
    panel.setForeground(Color.red);    
    Container contentPane = getContentPane();
    contentPane.add(panel);
  }
}

/**
   Panel that displays a message
*/
class GoodbyeWorldPanel extends JPanel
{
  public void paintComponent(Graphics g)
  {
    super.paintComponent (g);
    Graphics2D g2 = (Graphics2D)g;
    
    double leftX = 100;
    double topY = 100;
    double width = 200;
    double height = 150;
    
    Rectangle2D rect = new Rectangle2D.Double(leftX, topY, width, height);
    g2.draw(rect);
    
    int MESSAGE_X = (int)(leftX + width/4);
    int MESSAGE_Y = (int)(topY + height/2);

    g.drawString("Goodbye, World.", MESSAGE_X, MESSAGE_Y);
   }
}

   
