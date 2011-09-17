/**
   DownloadTableModel.java
   @author JMR
   @version $Id$

   This is the model for the host table, used by class HostPanel
*/
import java.util.Vector;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.table.*;

public class DownloadTableModel extends AbstractTableModel
{
  public DownloadTableModel(int c, Vector v) {
    contents = v;
    columns = c;
  }

  public int getRowCount()
  {
    return contents.size();
  }

  public int getSocketAt(int i)
  {
    ConnectionObject o = (ConnectionObject)contents.get(i);
    return o.getSocketId();
  }

  public int getColumnCount()
  {
    return columns;
  }

  public String getColumnName(int c)
  {
    if(c == 0)
      return "File name";
    else if(c == 1)
      return "Size";
    else if(c == 2)
      return "% Complete";
    else
      return "Unknown";
  }

  public Object getValueAt(int r, int c)
  {
      return new Integer(0);
  }

  // fields
  private Vector contents;
  private int length;
  private int columns;
}

