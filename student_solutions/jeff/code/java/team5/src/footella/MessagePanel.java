/**
   HostPanel.java
   part of footella
   @author JMR
   @version $Id: MessagePanel.java,v 1.3 2001/01/31 18:32:23 jeff Exp $

   This panel watches messages coming in from Control
   (eventually, at least).
*/
import java.io.*;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.table.*;

class MessagePanel extends JPanel implements Pokable, ChangeListener
{
  MessagePanel(ControlHandler control){
    this.control = control;
    control.setMessagePanel(this);

    // Message Table
    messageModel = new MessageTableModel(1, control.getMessageVector());
    JTable messageTable = new JTable(messageModel);
    messageTable.setShowHorizontalLines(false); // no lines
    messageTable.setBackground(new Color(255, 255, 255)); // and white!

    JScrollPane messagePane = new JScrollPane(messageTable);
    messagePane.setPreferredSize(new Dimension(625, 320));
    messagePane.setBorder(BorderFactory.createLoweredBevelBorder());
    JPanel messagePanel = new JPanel();
    messagePanel.setBorder(BorderFactory.createTitledBorder("System Messages"));
    messagePanel.add(messagePane);
    add(messagePanel);

    messageDisplay = new JCheckBox("Show messages", false);
    messageDisplay.addChangeListener(this);
    errorDisplay = new JCheckBox("Show errors", true);
    messageDisplay.addChangeListener(this);
    incomingDisplay = new JCheckBox("Show incoming", false);
    incomingDisplay.addChangeListener(this);

    Box optionBox = Box.createHorizontalBox();
    optionBox.add(messageDisplay);
    optionBox.add(incomingDisplay);
    optionBox.add(errorDisplay);
    add(optionBox);
  }

  public void poke() {
    messageModel.fireTableDataChanged();
  }

  public void stateChanged(ChangeEvent e) {
    control.setShowMessages((messageDisplay.isSelected()));
    control.setShowError((errorDisplay.isSelected()));
    control.setShowIncoming((incomingDisplay.isSelected()));
  }

  private JCheckBox messageDisplay;
  private JCheckBox errorDisplay;
  private JCheckBox incomingDisplay;
  private MessageTableModel messageModel;  
  private ControlHandler control;
}
