/**
   HostPanel.java
   part of footella

   @author JMR
   @version $Id: HostPanel.java,v 1.6 2001/01/31 16:10:48 jeff Exp $

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
    JLabel hostName = new JLabel("Host");
    hostField = new JTextField("10.11.0.140", 14); // arbitrary!
    JLabel hostPort = new JLabel("Port");
    portField = new JTextField("6346", 4);        // arbitrary! 

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
    connectPanel.add(hostName);
    connectPanel.add(hostField);
    connectPanel.add(hostPort);
    connectPanel.add(portField);
    connectPanel.add(connectButton);
    connectPanel.add(disconnectButton);

    // initialize a table, and display it
    hostModel = new HostTableModel(5, control.getHostVector());
    hostTable =  new JTable(hostModel);
    hostTable.addMouseListener(new TableListener());
    JScrollPane scrollPane = new JScrollPane(hostTable);
    scrollPane.setPreferredSize(new Dimension(625, 250));
    scrollPane.setBorder(BorderFactory.createLoweredBevelBorder());
    
    // create a horizontal box for the table
    JPanel hostBox = new JPanel();
    hostBox.setBorder(BorderFactory.createTitledBorder("Connections"));
    hostBox.add(scrollPane);


    // create another small panel
    JLabel maxIn = new JLabel("Maximum Incoming");
    inField = new JTextField("" + Utility.MAX_INCOMING, 2);
    JLabel maxOut = new JLabel("Maximum Outgoing");
    outField = new JTextField("" + Utility.MAX_OUTGOING, 2);
    // need document listeners here
    
    JPanel hostManagerPanel = new JPanel();
    connections = new JLabel(new String("      " + Utility.currentIncoming +
                                               " Incoming / " +
                                               Utility.currentOutgoing +
                                   " Outgoing"));
    hostManagerPanel.add(maxIn);
    hostManagerPanel.add(inField);
    hostManagerPanel.add(maxOut);
    hostManagerPanel.add(outField);
    hostManagerPanel.add(connections);

    // create a vertical box for the entire panel, 
    // and pack it
    Box fooBox = Box.createVerticalBox();
    fooBox.add(connectPanel);
    fooBox.add(hostBox);    
    fooBox.add(hostManagerPanel);

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
    connections.setText(new String("      " + Utility.currentIncoming +
                                               " Incoming / " +
                                               Utility.currentOutgoing +
                                   " Outgoing"));

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
  private JTextField inField;
  private JTextField outField;
  private JLabel connections;

  /* this small class deals with table events */
  private class TableListener extends MouseAdapter {
    public void mouseClicked(MouseEvent e) {
      int row = hostTable.getSelectedRow();
      System.out.println("Host row " + row + "selected");
      mySocket = hostModel.getSocketAt(row);
    }
  }
}



