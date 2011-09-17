/**
   QueryPanel.java
   @author JMR
   @version $Id: QueryPanel.java,v 1.12 2001/01/31 18:32:23 jeff Exp $

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
    JButton queryButton = new JButton("Query");
    queryField = new JTextField("", 30); // query
    socketField = new JTextField("0", 3);
    JButton downloadButton = new JButton("Download");

    // add actions
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
    //TableColumnModel columnModel = new TableColumn();
      //    columnModel.getColumn(0).setWidth(350);
    
    JScrollPane scrollPane = new JScrollPane(queryTable);
    scrollPane.setPreferredSize(new Dimension(625, 150));
    scrollPane.setBorder(BorderFactory.createLoweredBevelBorder());

    // Create a panel to hold the query search
    JPanel commandPanel = new JPanel();
    commandPanel.add(queryButton);
    commandPanel.add(queryField);
    commandPanel.add(downloadButton);
    add(commandPanel);

    // Create a panel to hold the download button
    JPanel query = new JPanel();
    query.setBorder(BorderFactory.createTitledBorder("Queries"));
    query.add(scrollPane);
    add(query);


    //---------------------------------------------------------------------

    // Download Table
    downloadModel = new DownloadTableModel(3, control.getDownloadVector());
    downloadTable = new JTable(downloadModel);
    downloadTable.addMouseListener(new TableListener());
    JScrollPane downloadPane = new JScrollPane(downloadTable);
    downloadPane.setPreferredSize(new Dimension(625, 80));
    downloadPane.setBorder(BorderFactory.createLoweredBevelBorder());

    // Buttons
    JButton cancelDownload = new JButton("Cancel Selected");
    JButton cancelAllDownload = new JButton("Cancel All");    
    JButton clearDownloadButton = new JButton("Clear completed");


    cancelDownload.addActionListener(new
      ActionListener() {
        public void actionPerformed(ActionEvent event) {
          control.removeDownloadAt(downloadRow); }
      });
    clearDownloadButton.addActionListener(new
      ActionListener() {
        public void actionPerformed(ActionEvent event) {
          control.removeCompletedDownloads(); }
      });

    // A panel for the buttons and things
    JPanel downloadCommand = new JPanel();
    downloadCommand.add(cancelDownload);
    downloadCommand.add(cancelAllDownload);
    downloadCommand.add(clearDownloadButton);
    JPanel down = new JPanel();
    down.setBorder(BorderFactory.createTitledBorder("Downloads"));
    down.add(downloadPane);
    add(down);
    add(downloadCommand);
  }
 
  /**
     Initializes a search. Information is taken from the query field
  */
  public void sendQuery() {
    control.clearResults();
    String message = queryField.getText().trim();
    QueryObject object = new QueryObject(-181, 10, (short)0, message);
    object.setSocketId(-1); //
    control.deliver(object);
  }

  /**
     Initiates a download
  */
  public void startDownload() {
    if(row != -1)
    control.download(row);
 }
  /**
     Poke is called when some information has changed. Usually called
     from the ControlHandler.
  */
  public void poke() {
    downloadModel.fireTableDataChanged();
  }

  public void poke(int i) {
    queryModel.fireTableDataChanged();
  }


  // fields
  private static JTextField socketField;
  private static JTextField queryField;
  private ControlHandler control;
  private JTable downloadTable;
  private JTable queryTable;
  private QueryTableModel queryModel;
  private DownloadTableModel downloadModel;
  private int row = -1;
  private int downloadRow;

  /* this small class deals with mouse motion in the table */
  private class TableListener extends MouseAdapter {
    public void mouseClicked(MouseEvent e) {
      row = queryTable.getSelectedRow();
      if(e.getClickCount() == 2) {
        startDownload();
      }
    }
  }

  /* this small class deals with table events */
  private class DownloadTableListener extends MouseAdapter {
    public void mouseClicked(MouseEvent e) {
      downloadRow = downloadTable.getSelectedRow();
    }
  }
}

