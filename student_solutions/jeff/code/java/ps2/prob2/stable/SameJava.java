/**
 * SameJava -- a same-gnome/same-game clone
 * @author Jeffrey M. Radcliffe
 * @version $Id: SameJava.java,v 1.4 2001/01/17 15:34:12 jeff Exp $
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
    // Construct a game frame...
    GameFrame frame = new GameFrame();
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    // and show it.
    frame.show();
  }
}
