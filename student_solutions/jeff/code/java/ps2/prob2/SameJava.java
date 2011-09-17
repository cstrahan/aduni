/**
 * SameJava -- a same-gnome/same-game clone
 * @author Jeffrey M. Radcliffe
 * @version $Id: SameJava.java,v 1.14 2001/01/16 12:57:23 jeff Exp $
 *
*/

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.event.*;
import java.util.*;

public class SameJava
{
  public static void main(String[] args) {
    int w, h;
    try {
      // Some basic command line parsing, to make sure that w, h parameters
      // are good.
      if (args.length == 2) {
        w = Integer.parseInt(args[0]);
        h = Integer.parseInt(args[1]);
      } else {
        w = 15; h = 10;
      }
      // Construct a game frame...
      GameFrame frame = new GameFrame(w, h);
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      // and show it.
      frame.show();
    }
    catch(Exception e) {
      System.out.println("Error parsing command line arguments.\n" +
                         "Correct usage: samejava <WIDTH> <HEIGHT>\n\n");
      System.exit(0);
    }
  }
}
