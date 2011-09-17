/**
   DownloadPanel.java
   part of footella
   @author JMR
   @version $Id: DownloadPanel.java,v 1.1 2001/01/28 15:59:12 jeff Exp $

   This panel displays download info coming in from Control
   (eventually, at least).
*/
import java.io.*;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.table.*;
import java.util.*;

class DownloadPanel extends JPanel implements Pokable
{
  DownloadPanel(ControlHandler control) {
    this.control = control;

    // Download Table
    downloadModel = new DownloadTableModel(3, control.getDownloadVector());
    JTable downloadTable = new JTable(downloadModel);
    JScrollPane scrollPane = new JScrollPane(downloadTable);
    scrollPane.setPreferredSize(new Dimension(575, 150));
    add(scrollPane);

    // Buttons
    JButton cancelDownload = new JButton("Cancel Selected Download");
    JButton cancelAllDownload = new JButton("Cancel All");    

    JPanel downloadCommand = new JPanel();
    downloadCommand.add(cancelDownload);
    downloadCommand.add(cancelAllDownload);
    add(downloadCommand);

    // Upload Table
    uploadModel = new DownloadTableModel(3, control.getDownloadVector());
    JTable uploadTable = new JTable(uploadModel);
    JScrollPane uploadPane = new JScrollPane(uploadTable);
    uploadPane.setPreferredSize(new Dimension(575, 75));
    add(uploadPane);

    // Buttons
    JButton cancelUpload = new JButton("Cancel Selected Up");
    JButton cancelAllUpload = new JButton("Cancel All");    

    JPanel uploadCommand = new JPanel();
    uploadCommand.add(cancelUpload);
    uploadCommand.add(cancelAllUpload);
    add(uploadCommand);
  }

  public void poke() {
    downloadModel.fireTableDataChanged();
    uploadModel.fireTableDataChanged();
  }
  
  private DownloadTableModel downloadModel;
  private DownloadTableModel uploadModel;  
  private ControlHandler control;
}
