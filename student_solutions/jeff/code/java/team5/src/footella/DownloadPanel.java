/**
   DownloadPanel.java
   part of footella
   @author JMR
   @version $Id: DownloadPanel.java,v 1.7 2001/01/30 16:36:50 jeff Exp $

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

class DownloadPanel extends JPanel implements Pokable, ChangeListener
{
  DownloadPanel(ControlHandler control) {
    this.control = control;
    control.setDownloadPanel(this);


    // Monitor Table
    monitorModel = new MonitorTableModel(1, control.getMonitorVector()); // give a real vector
    JTable monitorTable = new JTable(monitorModel);
    monitorTable.setShowHorizontalLines(false); // no lines
    JScrollPane monitorPane = new JScrollPane(monitorTable);
    monitorPane.setPreferredSize(new Dimension(625, 150));
    monitorPane.setBorder(BorderFactory.createLoweredBevelBorder());
    JPanel monitorPanel = new JPanel();
    monitorPanel.setBorder(BorderFactory.createTitledBorder("Incoming Query Monitor"));
    monitorPanel.add(monitorPane);
    add(monitorPanel);

    // a button which allows turning off query showing
    enableQueryShow = new JCheckBox("Show Queries", true);
    enableQueryShow.addChangeListener(this);
    add(enableQueryShow);

    
    // Upload Table
    // give this a real data model
    uploadModel = new DownloadTableModel(3, new Vector());
    JTable uploadTable = new JTable(uploadModel);
    JScrollPane uploadPane = new JScrollPane(uploadTable);
    uploadPane.setPreferredSize(new Dimension(625, 90));
    uploadPane.setBorder(BorderFactory.createLoweredBevelBorder());
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
    monitorModel.fireTableDataChanged();
    uploadModel.fireTableDataChanged();
  }

  public void stateChanged(ChangeEvent e) {
    control.setQueryShow((enableQueryShow.isSelected()));
  }
  
  private JCheckBox enableQueryShow;
  private MonitorTableModel monitorModel;
  private DownloadTableModel uploadModel;  
  private ControlHandler control;
}
