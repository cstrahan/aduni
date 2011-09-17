
/**
   MonitorTableModel.java
   @author JMR
   @version $Id: MonitorTableModel.java,v 1.2 2001/01/30 13:06:37 jeff Exp $

   This is the model for the host table, used by class HostPanel
*/
import java.util.Vector;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.table.*;

public class MonitorTableModel extends AbstractTableModel {
  public MonitorTableModel(int c, Vector v) {
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
      return "Query";
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

