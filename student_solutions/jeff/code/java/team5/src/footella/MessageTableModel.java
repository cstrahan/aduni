/**
   MessageTableModel.java
   @author JMR
   @version $Id: MessageTableModel.java,v 1.1 2001/01/29 22:48:23 jeff Exp $

   This is the model for the host table, used by class HostPanel
*/
import java.util.Vector;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.table.*;

public class MessageTableModel extends AbstractTableModel {
  public MessageTableModel(int c, Vector v) {
    contents = v;
    columns = c;
  }

  public int getRowCount() {
    return contents.size();
  }

  public int getSocketAt(int i) {
    ConnectionObject o = (ConnectionObject)contents.get(i);
    return o.getSocketId();
  }

  public int getColumnCount() {
    return columns;
  }

  public String getColumnName(int c) {
    if(c == 0)
      return "Messages";
    else
      return "Unknown";
  }

  public Object getValueAt(int r, int c) {
    String content = (String)contents.get(r);
    String foo = content.trim();
    return foo;
  }

  // fields
  private Vector contents;
  private int length;
  private int columns;
}

