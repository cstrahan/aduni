/**
   HostTableModel.java
   @author JMR
   @version $Id: HostTableModel.java,v 1.2 2001/01/28 19:32:41 jeff Exp $

   This is the model for the host table, used by class HostPanel
*/
import java.util.Vector;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.table.*;

public class HostTableModel extends AbstractTableModel
{
  public HostTableModel(int c, Vector v) {
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
      return "Remote Host";
    else if(c == 1)
      return "Type";
    else if(c == 2)
      return "Sent";
    else if(c == 3)
      return "Received";
    else
      return "Status";
  }

  public Object getValueAt(int r, int c)
  {
    ConnectionObject con = (ConnectionObject)contents.get(r);
    Object[] information = con.getInfo();
      return information[c];
  }

  // fields
  private Vector contents;
  private int length;
  private int columns;
}

