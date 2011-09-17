/**
   HostPanel.java
   part of footella

   @author JMR
   @version $Id: HostPanel.java,v 1.1 2001/01/28 15:59:13 jeff Exp $

   This panel takes care of host connections and disconnections,
   and keeps tabs on messages sent and received. It incorporates
   the HostTableModel.
*/
import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.table.*;

class HostPanel extends JPanel implements Pokable
{
  HostPanel(ControlHandler c) {
    // initialize the important stuff
    control = c;
    control.setHostPanel(this);

    // Make some buttons Buttons
    JButton connectButton = new JButton("Connect");
    JButton disconnectButton = new JButton("Disconnect");
    hostField = new JTextField("10.11.0.65", 16); // arbitrary!
    portField = new JTextField("8845", 4);        // arbitrary! 

    // add actions to the buttons
    connectButton.addActionListener(new
      ActionListener() {
        public void actionPerformed(ActionEvent event) {
          connect(); }
      });
    disconnectButton.addActionListener(new
      ActionListener() 
      {
        public void actionPerformed(ActionEvent event) 
        { disconnect(); }
      });

    // add the fields and buttons into their own panel
    JPanel connectPanel = new JPanel();
    connectPanel.add(hostField);
    connectPanel.add(portField);
    connectPanel.add(connectButton);
    connectPanel.add(disconnectButton);

    // initialize a table, and display it
    hostModel = new HostTableModel(5, control.getHostVector());
    hostTable =  new JTable(hostModel);
    hostTable.addMouseListener(new TableListener());
    JScrollPane scrollPane = new JScrollPane(hostTable);
    scrollPane.setPreferredSize(new Dimension(525, 200));
    
    // create a horizontal box for the table
    Box hostBox = Box.createHorizontalBox();
    hostBox.add(scrollPane);

    // create a vertical box for the entire panel, 
    // and pack it
    Box fooBox = Box.createVerticalBox();
    fooBox.add(connectPanel);
    fooBox.add(hostBox);    

    // finally, add the box to the panel
    add(fooBox);
  } // end constructor

  /**
     Poke is called when some data has been changed,
     and the host table needs a refresh. Pokes are
     done usually from the ControlHandler
  */
  public void poke() { 
    hostModel.fireTableDataChanged();
  }


  /**
     Implements a user initiated connection, getting information
     from the contents of the host and port fields.
  */
  public void connect() {
    try {
      // create a SocketHandler, and get its number
      String host = hostField.getText().trim();
      int port = Integer.parseInt(portField.getText().trim());
      if(! control.openConnection(host, port)) 
        System.out.println("Error opening Connection.");
      poke();
    }
    catch(Exception e) { 
      System.out.println("Error opening Connection:  " + e);
    }
  }

  /**
     Initates a disconnect, getting socket information from
     what is currently highlighted in the table.
  */
  public void disconnect() {
    if(mySocket != -1) {
      control.removeConnection(mySocket);
      //this could be messy
      mySocket = -1;
    }
  }

  // fields
  private int mySocket = -1;
  private JTextField portField;
  private ControlHandler control;
  private JTable hostTable;
  private HostTableModel hostModel;
  private JTextField hostField;
  private JTextArea textArea;

  /* this small class deals with table events */
  private class TableListener extends MouseAdapter {
    public void mouseClicked(MouseEvent e) {
      int row = hostTable.getSelectedRow();
      mySocket = hostModel.getSocketAt(row);
    }
  }
}



