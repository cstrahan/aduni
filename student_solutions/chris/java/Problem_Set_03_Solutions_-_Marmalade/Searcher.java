/**
   Big hairy GUI
*/
import java.io.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;
import javax.swing.border.*;


public class Searcher
{
    // private static int Speed has to come from the user.
    static LinkedList searches = new LinkedList();
    static SearchPanel mypanel;
    static ConnectionPanel myconnectionpanel;
    static MonitorPanel mymonitorpanel;
    
    //added constructor
    
    public Searcher()
    {
	TabbedPaneFrame frame = new TabbedPaneFrame();
	mypanel = frame.getSearchPanel();
	myconnectionpanel = frame.getConnectionPanel();
	mymonitorpanel = frame.getMonitorPanel();
	frame.show();
    }
    
    public static void addSearch(Query search) // Called when the user presses the "Search" button
    {
	searches.add(search);
    }
    
    public static void clear() // Called when the user presses the "Clear" button
    {
	searches = new LinkedList();
    }
    
    public static void addFileTransfer(IPAddress ip, String name)
    {
	mypanel.addFileTransfer(ip, name);
    }
    
    public static void updateConnectionStatus(IPAddress ip, String name, String status)
    {
	mypanel.updateConnectionStatus(ip, name, status);
    }
    
    public static void updateFileTransferStatus(IPAddress ip, String name, int percent)
    {
	mypanel.updateFileTransferStatus(ip, name, percent);
    }
    
    public static void inform(IPAddress ip, QueryHit qh)
    {
	Integer port = new Integer(qh.getPort());
	String myip = qh.getIP().toString();
	
	Iterator iter = searches.iterator();
	while (iter.hasNext())
	    {
		Query b = (Query) iter.next();
		if(qh.compare(b))
		    {
			ResultSet r = qh.getResults();
			while (r.more())
			    {
				Integer index = new Integer(r.getIndex());
				Integer size = new Integer(r.getFilesize());
				String name = r.getName();
				mypanel.addQHit(index, name, size, myip, port);
			    }
		    }
	    }
    }

    public static void inform(Query q)
    {
	mymonitorpanel.addQuery(q.getIP().toString(), q.getIP().getPort(), q.getSearchString());
    }
    
    public static void updateInfo(int hosts, int totalkb, int totalfiles) 
    {
	myconnectionpanel.updateStats(hosts, totalfiles, totalkb);
    }

  public static void updateAddedConnection(Connection c)
  {
    myconnectionpanel.addAConnection(c.getIPAddress().toString(), c.getIPAddress().getPort(), c.getTypeString(), "Connected");
  }

  public static void updateRemovedConnection(IPAddress ip)
  {
    myconnectionpanel.removeAConnection(ip.toString(), ip.getPort(), "Disconnected");
  }

  public static void updateHostCache(Host h, boolean flag)
  {
    if (flag) myconnectionpanel.addAHostInCache(h.getName(), h.getPort());
    else myconnectionpanel.removeAHostInCache(h.getName(), h.getPort());
  }
}


class TabbedPaneFrame extends JFrame implements ChangeListener
{  
  private JTabbedPane tabbedPane;
  private ConnectionPanel cPanel;
  private SearchPanel sPanel;
  private MonitorPanel mPanel;
  private JMenuItem newMarmalade;
  private JMenuItem exitMarmalade;
  
  public TabbedPaneFrame()
  {
    setTitle("Marmalade");
    
    // get screen dimensions
    Toolkit kit = Toolkit.getDefaultToolkit();
    Dimension screenSize = kit.getScreenSize();
    int inset = 100;
    setBounds(inset, inset, 800, 550);
    
    JMenuBar menuBar = new JMenuBar();
    setJMenuBar(menuBar);
    JMenu fileMenu = new JMenu ("File");
    menuBar.add(fileMenu);
    newMarmalade = fileMenu.add("New");
    exitMarmalade = fileMenu.add("Exit");
    
    MenuHandler m = new MenuHandler();
    newMarmalade.addActionListener(m);
    exitMarmalade.addActionListener(m);
    
    // JMenu optionsMenu = new JMenu ("Options");
    //menuBar.add(optionsMenu);

    cPanel = new ConnectionPanel();
    sPanel = new SearchPanel();
    mPanel = new MonitorPanel();

    //Border etched = BorderFactory.createEtchedBorder();
    //Border titled = BorderFactory.createTitledBorder(line, "Connections");
    Border line = BorderFactory.createLineBorder(Color.black);    
    cPanel.setBorder(line);
    sPanel.setBorder(line);
    mPanel.setBorder(line);

    tabbedPane = new JTabbedPane();
    tabbedPane.addChangeListener(this);
    tabbedPane.addTab("Connections", cPanel);
    tabbedPane.addTab("Search", sPanel);
    tabbedPane.addTab("Monitor", mPanel);

    addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent e)
        {
          Preferences.writeToFile();
          System.exit(0);
        }
      });
    
    getContentPane().add(tabbedPane, "Center");
  }
  
  /**
     Inner class - MenuHandler
  */
  class MenuHandler implements ActionListener
  {
    public void actionPerformed(ActionEvent evt)
    {
      if (evt.getSource() == newMarmalade)
      {

      }
      else if (evt.getSource() == exitMarmalade)
      {
        Preferences.writeToFile();
        System.exit(0);
      }
    }
  }

  
  public void stateChanged(ChangeEvent event)
  {  
    JTabbedPane pane = (JTabbedPane)event.getSource();
  }
    
    public ConnectionPanel getConnectionPanel()
    {
	return(cPanel);
    }

    public SearchPanel getSearchPanel()
    {
	return(sPanel);
    }

    public MonitorPanel getMonitorPanel()
    {
	return(mPanel);
    }
}


class ConnectionPanel extends JPanel
{
  private static DefaultTableModel liveModel;
  private static DefaultTableModel cacheModel;
  private static DefaultTableModel statsModel;
  private static JTable liveTable;
  private static JTable cacheTable;
  private static JTable statsTable;
  private static JTextField ipField;
  private JTable table;
  
  public ConnectionPanel()
  {
    setLayout(null);
    ipField = new JTextField(20);
    add(ipField);
    ipField.setBounds (50, 200, 250, 25);
    
    JButton connectButton = new JButton("Connect");
    add(connectButton);
    connectButton.setBounds (320, 200, 150, 25);
    connectButton.addActionListener(new ConnectAction());
    
    liveModel = new DefaultTableModel(0, 0);
    liveModel.addColumn((Object) "Remote host");
    liveModel.addColumn((Object) "Port");
    liveModel.addColumn((Object) "Type");
    liveModel.addColumn((Object) "Status");
    
    liveTable = new JTable(liveModel);
    
    JScrollPane scroll = new JScrollPane (liveTable);
    scroll.setBackground(Color.blue);
    add(scroll);
    scroll.setBounds(50, 50, 700, 120);
    
    JButton deleteConnection = new JButton("Delete Connection");
    add(deleteConnection);
    deleteConnection.setBounds (575, 200, 175, 25);
    deleteConnection.addActionListener(new DeleteConnectionAction());
    
    statsModel = new DefaultTableModel(0, 0);
    statsModel.addColumn((Object) "Hosts");
    statsModel.addColumn((Object) "Total Files");
    statsModel.addColumn((Object) "Total kB");
      
    statsTable = new JTable(statsModel);
    
    JScrollPane statsScroll = new JScrollPane (statsTable);
    statsScroll.setBackground(Color.blue);
    add(statsScroll);
    statsScroll.setBounds(50, 280, 300, 50);
    
    Object[] newStatsRow = new Object[3];
    statsModel.addRow(newStatsRow);


    
    cacheModel = new DefaultTableModel(0, 0);
    cacheModel.addColumn((Object) "Remote host");
    cacheModel.addColumn((Object) "Port");
    
    cacheTable = new JTable(cacheModel);
    //downloadtable.getTableHeader().setBackground(Color.black);
    //downloadtable.getTableHeader().setForeground(Color.black);

    JScrollPane cacheScroll = new JScrollPane (cacheTable);
    cacheScroll.setBackground(Color.blue);
    add(cacheScroll);
    cacheScroll.setBounds(450, 280, 300, 150);

    JButton delete = new JButton("Delete host");
    add(delete);
    delete.setBounds (525, 440, 150, 25);
    delete.addActionListener(new DeleteAction());
    
  }

  public static void addAConnection(String host, int port, String type, String status)
  {
    Object[] newRow = new Object[4];
    newRow[0] = host;
    newRow[1] = new Integer(port);
    newRow[2] = type;
    newRow[3] = status;
    liveModel.insertRow(0, newRow);
  }

  public static void removeAConnection(String host, int port, String status)
  {
    for (int i = 0; i < liveModel.getRowCount(); i++)
    {
      String ip = (String)(liveModel.getValueAt(i, 0));
      if (ip.equals(host)) liveModel.removeRow(i);
    }
  }
  
  public static void addAHostInCache(String host, int port)
  {
    Object[] newRow = new Object[2];
    newRow[0] = host;
    newRow[1] = new Integer(port);
    cacheModel.insertRow(0, newRow);
  }
  
  public static void removeAHostInCache(String host, int port)
  {
    for (int i = 0; i < cacheModel.getRowCount(); i++)
    {
      String h = (String)(cacheModel.getValueAt(i, 0));
      if (host.equals(h)) cacheModel.removeRow(i);
    }
  }

  public static void updateStats(int hosts, int files, int kb)
  {
    statsModel.setValueAt(new Integer(hosts), 0, 0);
    statsModel.setValueAt(new Integer(files), 0, 1);
    statsModel.setValueAt(new Integer(kb), 0, 2);
  }
	
  class ConnectAction implements ActionListener
  {
    public void actionPerformed(ActionEvent event)
    {
      String ip = ipField.getText();
      StringTokenizer st = new StringTokenizer(ip, ":");
      if (st.countTokens() == 2)
	    {
        ip = st.nextToken();
        int port = Integer.parseInt(st.nextToken());
        Connector connector = new Connector(ip, port);
        connector.start();
	    }
    }
  }
  
  class DeleteAction implements ActionListener
  {
    public void actionPerformed(ActionEvent evt)
    {
      int rowIndex = cacheTable.getSelectedRow();
      String ip = (String) cacheTable.getValueAt(rowIndex, 0);
      Integer port = (Integer) cacheTable.getValueAt(rowIndex, 1);
      Host h = new Host(ip, port.intValue());
      HostCache.removeHost(h);
    }
  }
  
  class DeleteConnectionAction implements ActionListener
  {
    public void actionPerformed(ActionEvent evt)
    {
      int rowIndex = liveTable.getSelectedRow();
      String ip = (String) liveTable.getValueAt(rowIndex, 0);
      Integer port = (Integer) liveTable.getValueAt(rowIndex, 1);
      StringTokenizer s = new StringTokenizer(ip, ".");
      int ip1 = Integer.parseInt(s.nextToken());
      int ip2 = Integer.parseInt(s.nextToken());
      int ip3 = Integer.parseInt(s.nextToken());
      int ip4 = Integer.parseInt(s.nextToken());
      IPAddress ipaddress = new IPAddress(ip1, ip2, ip3, ip4, port.intValue());
      NetworkManager.notify(ipaddress);
    }
  }
  
}

class SearchPanel extends JPanel
{
    private static DefaultTableModel searchModel;
    private static DefaultTableModel downloadModel;
    private static JTextField searchField;
    private static JTable table;
    private static JTable downloadtable;
    
    public SearchPanel()
    {
	setLayout(null);
	searchField = new JTextField(30);
	add(searchField);
	searchField.setBounds (50, 25, 300, 25); //(sets location and size of search field)
	
	JButton searchButton = new JButton("Search");
	add(searchButton);
	searchButton.setBounds (400, 25, 150, 25);
	searchButton.addActionListener(new SearchAction());
	
	JButton clearButton = new JButton("Clear");
	add(clearButton);
	clearButton.setBounds(600, 25, 150, 25);
	clearButton.addActionListener(new ClearAction());
	
	JButton download = new JButton ("Download");
	add(download);
	download.setBounds (300, 290, 200, 25);
	download.addActionListener(new DownloadAction());
	
	searchModel = new DefaultTableModel(0,0);
	searchModel.addColumn((Object) "File Index");
	searchModel.addColumn((Object) "File Name");
	searchModel.addColumn((Object) "File Size");
	searchModel.addColumn((Object) "IP Address");
	searchModel.addColumn((Object) "Port");

	downloadModel = new DefaultTableModel(0,0);
	downloadModel.addColumn((Object) "IP Address");
	downloadModel.addColumn((Object) "File");
	downloadModel.addColumn((Object) "Connection Status");
	downloadModel.addColumn((Object) "File Transfer Progress");

	table = new JTable(searchModel);
	//table.getTableHeader().setBackground(Color.black);
	//table.getTableHeader().setForeground(Color.red);

	downloadtable = new JTable(downloadModel);
	//downloadtable.getTableHeader().setBackground(Color.black);
	//downloadtable.getTableHeader().setForeground(Color.orange);
	
	JScrollPane scroll = new JScrollPane (table);
	add(scroll);
	scroll.setBounds(50,70,700,200);

	scroll = new JScrollPane(downloadtable);
	add(scroll);
	scroll.setBounds(50,330,700,130);
    }
    
    public static void addQHit (Integer index, String name, Integer size, String ip, Integer port)
    {
	Object[] newRow = new Object[5];
	newRow[0] = index;
	newRow[1] = name;
	newRow[2] = size;
	newRow[3] = ip;
	newRow[4] = port;
	searchModel.addRow(newRow);
    }

    public static void addFileTransfer(IPAddress ip, String name)
    {
	for (int i = 0; i < downloadtable.getRowCount(); i++) /* If we've already tried downloading the same thing before, update it properly on the table rather
								 than adding a new one. */
	    if (((IPAddress)downloadtable.getValueAt(i,0)).equals(ip) && ((String)downloadtable.getValueAt(i,1)).equals(name))
		updateConnectionStatus(ip, name, "Connecting...");
	Object[] newRow = new Object[4];
	newRow[0] = ip;
	newRow[1] = name;
	newRow[2] = "Connecting...";
	newRow[3] = "0% Complete";
	downloadModel.addRow(newRow);
    }

    public static void updateConnectionStatus(IPAddress ip, String name, String status)
    {
	for (int i = 0; i < downloadtable.getRowCount(); i++)
	    if (((IPAddress)downloadtable.getValueAt(i,0)).equals(ip) && ((String)downloadtable.getValueAt(i,1)).equals(name))
		{
		    downloadModel.setValueAt(status,i,2);
		    break;
		}
    }
    
    public static void updateFileTransferStatus(IPAddress ip, String name, int percent)
    {
	for (int i = 0; i < downloadtable.getRowCount(); i++)
	    if (((IPAddress)downloadtable.getValueAt(i,0)).equals(ip) && ((String)downloadtable.getValueAt(i,1)).equals(name))
		{
		    downloadModel.setValueAt((String)(percent + "% Complete"), i, 3);
		    break;
		}
    }

  class DownloadAction implements ActionListener
  {
    public void actionPerformed(ActionEvent event)
    {
	    int rowIndex = table.getSelectedRow();
	    Integer index = (Integer) table.getValueAt(rowIndex,0);
	    String name = (String) table.getValueAt(rowIndex,1);
	    String ip = (String) table.getValueAt(rowIndex,3);
	    Integer port = (Integer) table.getValueAt(rowIndex,4);
	    Downloader downloader = new Downloader(index.intValue(), name, ip, port.intValue());
	    downloader.start();
    }
  }
    
    class SearchAction implements ActionListener
    {
	public void actionPerformed(ActionEvent event)
	{
	    Query a = new Query (0, searchField.getText()); // All searches are minimum speed 0 for now...
	    NetworkManager.writeToAll(a);
	    Searcher.addSearch(a);
	}
    }
    
  class ClearAction implements ActionListener
  {
    public void actionPerformed(ActionEvent event)
    {
	    searchModel.setRowCount(0);
	    downloadModel.setRowCount(0);
	    Searcher.clear();
    }
  }
}


class MonitorPanel extends JPanel
{
  private static DefaultTableModel monitorModel;
  private static JTable table;
  
  public MonitorPanel()
  {
    setLayout(null);
    
    monitorModel = new DefaultTableModel(0, 0);
    monitorModel.addColumn((Object) "Remote host");
    monitorModel.addColumn((Object) "Port");
    monitorModel.addColumn((Object) "Query");
    
    JTable monitorTable = new JTable(monitorModel);
    
    JScrollPane scroll = new JScrollPane (monitorTable);
    scroll.setBackground(Color.blue);
    add(scroll);
    scroll.setBounds(50, 50, 700, 400);
  }
  
  public void addQuery(String host, int port, String query)
  {
    Object[] newRow = new Object[3];
    newRow[0] = host;
    newRow[1] = new Integer(port);
    newRow[2] = query;
    monitorModel.insertRow(0, newRow);
  }
}



	      
 


