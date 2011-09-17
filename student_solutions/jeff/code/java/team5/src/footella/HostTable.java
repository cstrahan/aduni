import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.table.*;

public class HostTable
{
  public static void main(String[] args)
  {
    JFrame frame = new HostTableFrame();
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.show();
  }
}

class HostTableFrame extends JFrame
{
  public HostTableFrame()
  {
    setTitle("HostTable");
    setSize(500,200);

    JTable table =  new JTable(cells, columnNames);

    Container contentPane = getContentPane();
    JScrollPane scrollPane = new JScrollPane(table);
    
    contentPane.add(scrollPane, "Center");
  }
  

  private Object[][] cells = new Object[10][10];

  private String[] columnNames = 
  {
    "Remote Host", "Sent", "Received", "Status"
  };
}

    


    

