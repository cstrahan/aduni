/**
   QueryPanel.java
   @author JMR
   @version $Id: QueryPanel.java,v 1.3 2001/01/28 21:55:26 jeff Exp $

   This panel is responsible for dealing with user queries and results.
   It utilizes the QueryTableModel.
*/
import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.table.*;

class QueryPanel extends JPanel implements Pokable
{
  QueryPanel(ControlHandler c) {
    // initialize the important stuff
    control = c;
    control.setQueryPanel(this);

    // make a few buttons and fields
    JButton pingButton = new JButton("Ping"); // <<< NOT IN FINAL!
    JButton queryButton = new JButton("Query");
    queryField = new JTextField("", 30); // query
    socketField = new JTextField("0", 3);
    JButton downloadButton = new JButton("Download Selected");

    // add actions
    pingButton.addActionListener(new
      ActionListener() {
        public void actionPerformed(ActionEvent event) {
            sendPing(); }
      });
    queryButton.addActionListener(new
      ActionListener() {
        public void actionPerformed(ActionEvent event) {
          sendQuery(); }
      });
    downloadButton.addActionListener(new
      ActionListener() {
        public void actionPerformed(ActionEvent event) {
          startDownload(); }
      });
                                     
    // initialize the query table
    queryModel = new QueryTableModel(3, control.getQueryVector());
    queryTable =  new JTable(queryModel);
    queryTable.addMouseListener(new TableListener());
    JScrollPane scrollPane = new JScrollPane(queryTable);
    scrollPane.setPreferredSize(new Dimension(525, 200));

    // Create a panel to hold the query search
    JPanel commandPanel = new JPanel();
    commandPanel.add(pingButton);
    commandPanel.add(queryButton);
    commandPanel.add(queryField);

    // Create a panel to hold the download button
    JPanel downloadPanel = new JPanel();
    downloadPanel.add(downloadButton);

    // add everything to the main panel
    add(commandPanel);
    add(scrollPane);
    add(downloadPanel);
  }

  /* sends out a ping */
  public void sendPing() {
      Ping ping = new Ping();
      control.deliver(ping);
  }
 
  /**
     Initializes a search. Information is taken from the query field
  */
  public void sendQuery() {
    control.clearResults();
    String message = queryField.getText().trim();
    QueryObject object = new QueryObject(-181, 10, (short)0, message);
    control.deliver(object);
    queryField.setText("");
  }

  /**
     Initiates a download
  */
  public void startDownload() {
    int row = queryTable.getSelectedRow();
    control.download(row);
 }

  /**
     Poke is called when some information has changed. Usually called
     from the ControlHandler.
  */
  public void poke() {
    queryModel.fireTableDataChanged();
  }

  // fields
  private static JTextField socketField;
  private static JTextField queryField;
  private ControlHandler control;
  private JTable queryTable;
  private QueryTableModel queryModel;

  /* this small class deals with mouse motion in the table */
  private class TableListener extends MouseAdapter {
    public void mouseClicked(MouseEvent e) {
      System.out.println("Column " + queryTable.getSelectedRow() +
                         " selected");
    }
  }
}

