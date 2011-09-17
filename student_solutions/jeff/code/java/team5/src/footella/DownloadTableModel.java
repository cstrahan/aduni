/**
   DownloadTableModel.java
   @author JMR
   @version $Id: DownloadTableModel.java,v 1.4 2001/01/31 18:32:22 jeff Exp $

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

  public int getRowCount() { return contents.size(); }

  public int getSocketAt(int i) {
    ConnectionObject o = (ConnectionObject)contents.get(i);
    return o.getSocketId();
  }

  public int getColumnCount() { return columns; }

  public String getColumnName(int c) {
    if(c == 0) return "File name";
    else if(c == 1) return "Remote Host";
    else if(c == 2) return "Status";
    else return "Unknown";
  }

  public Object getValueAt(int r, int c) {
    DownloadObject con = (DownloadObject)contents.get(r);
    Object[] information = con.getInfo();
      return information[c];
  }

  // fields
  private Vector contents;
  private int length;
  private int columns;
}

