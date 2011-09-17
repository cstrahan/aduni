/**
   Footella.java
   @author JMR
   @version $Id: Footella.java,v 1.7 2001/01/28 15:58:49 jeff Exp $

   This is main routine for footella. It initializes the backend, the GUI,
   and then its job is done.
*/
import javax.swing.*;
import java.awt.*;

public class Footella
{
  private static int mySocket;

  /**
     This is basically a control loop. Neat, eh?
  */
  public static void main(String[] args) {
    int port;
    if(args.length == 0)
      port = 6346;
    else
      port = Integer.parseInt(args[0]); // could use error check

    // Set up the main frame
    FootellaFrame frame = new FootellaFrame(port);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.show();
  }
}


