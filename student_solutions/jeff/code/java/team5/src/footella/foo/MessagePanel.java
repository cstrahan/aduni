/**
   HostPanel.java
   part of footella
   @author JMR
   @version $Id: MessagePanel.java,v 1.1 2001/01/28 15:59:13 jeff Exp $

   This panel watches messages coming in from Control
   (eventually, at least).
*/
import java.io.*;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.table.*;

class MessagePanel extends JPanel implements Pokable
{

  protected JTextArea textArea;

  MessagePanel() {
    // ---------------------------------------------------
    // MESSSAGE AREA
    // ---------------------------------------------------    
    textArea = new JTextArea(14, 43);
    JScrollPane textScrollPane = new JScrollPane(textArea);
    textScrollPane.setBorder
      (BorderFactory.createLoweredBevelBorder());
    add(textScrollPane);
  }

  public void poke() {}
}
