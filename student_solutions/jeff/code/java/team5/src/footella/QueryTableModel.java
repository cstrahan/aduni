/**
   QueryTableModel.java
   part of footella

   @author JMR
   @version $Id: QueryTableModel.java,v 1.1 2001/01/28 15:59:13 jeff Exp $

   This is the model for the QueryTable
*/
import java.util.Vector;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.table.*;

public class QueryTableModel extends AbstractTableModel {
  public QueryTableModel(int c, Vector v) {
    contents = v;
    columns = c;
  }

  public int getRowCount() {
    return contents.size();
  }

  public int getColumnCount() {
    return columns;
  }

  public String getColumnName(int c) {
    if(c == 0)
      return "File Name";
    else if(c == 1)
      return "Size";
    else if(c == 2)
      return "Host";
    else
      return "Unknown";
  }

  public Object getValueAt(int r, int c) {
    ResultTableObject con = (ResultTableObject)contents.get(r);
    Object[] information = con.getInfo();
      return information[c];
  }

  private Vector contents;
  private int length;
  private int columns;
}

