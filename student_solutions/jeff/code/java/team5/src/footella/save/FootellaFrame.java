/**
   FootellaFrame.java
   @author JMR
   @version $Id: FootellaFrame.java,v 1.4 2001/01/29 15:57:06 eklempne Exp $

   This is the main panel for Footella. It contains a menu bar,
   and a tabbed panel that contains everything else.
*/

import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;

class FootellaFrame extends JFrame implements ChangeListener
{
  private int mySocket;
  private JTabbedPane tabbedPane;
  protected MessageServer server;
  protected ControlHandler controlHandler;
  
  public FootellaFrame(int port) {
    // add file prefs;
    server = new MessageServer(port);
    controlHandler = server.getControlHandler();
    PingThread pingThread = new PingThread(controlHandler);
    RoutingTable routingTable = new RoutingTable();

    setTitle("Footella Beta");
    Toolkit kit = Toolkit.getDefaultToolkit();
    Image img = kit.getImage("images/icon.gif");
    setIconImage(img);
    setSize(WIDTH, HEIGHT);

    /* --------------------------------------------------------------
     *            BEGIN HIDEOUS MENU BAR CONSTRUCTION
     *  ----------------------------------------------------------- */
    // ************* FILE MENU ****************
    JMenu fileMenu = new JMenu("File");
    fileMenu.setMnemonic('F');
    
    AbstractAction exitAction = new
      AbstractAction("Quit")
      {
        public void actionPerformed(ActionEvent event)
        {
          System.exit(0);
        }
      };

    exitAction.putValue(Action.MNEMONIC_KEY, new Integer('Q'));
    fileMenu.add(exitAction);
    // ************* OPTIONS MENU ****************
    JMenu optionsMenu = new JMenu("Options");
    optionsMenu.setMnemonic('O');

    // ************* ABOUT MENU ****************
    JMenu aboutMenu = new JMenu("About");
    aboutMenu.setMnemonic('A');

    // create a menu bar, and add everything to it
    JMenuBar menuBar = new JMenuBar();
    menuBar.add(fileMenu);
    menuBar.add(optionsMenu);
    menuBar.add(aboutMenu);

    // add it to the panel
    setJMenuBar(menuBar);

    /* --------------------------------------------------------------
     *                END HIDEOUS MENU BAR CONSTRUCTION
     *  ------------------------------------------------------------ */

    // initialize the various panels
    HostPanel host = new HostPanel(controlHandler);
<<<<<<< FootellaFrame.java
    MessagePanel message = new MessagePanel();
    DownloadPanel download = new DownloadPanel(controlHandler);
=======
    MessagePanel message = new MessagePanel(controlHandler);
    DownloadPanel download = new DownloadPanel(controlHandler);
>>>>>>> 1.4
    QueryPanel query = new QueryPanel(controlHandler);
    
    // create a tabbed pane, and add the panels
    tabbedPane = new JTabbedPane();
    tabbedPane.addChangeListener(this);
    tabbedPane.addTab("Connections", host);
    tabbedPane.addTab("Search", query);
    tabbedPane.addTab("Monitor", download);
    tabbedPane.addTab("Preferences", null);
    tabbedPane.addTab("Messages", message);

    // add everything to the content pane
    Container contentPane = getContentPane();
    contentPane.add(tabbedPane);

    server.start();
    pingThread.start();
    routingTable.start();
  }
  
  public void stateChanged(ChangeEvent event) {
    JTabbedPane pane = (JTabbedPane)event.getSource();
  }

  // fields
  public static final int WIDTH = 675;
  public static final int HEIGHT = 460;
}

