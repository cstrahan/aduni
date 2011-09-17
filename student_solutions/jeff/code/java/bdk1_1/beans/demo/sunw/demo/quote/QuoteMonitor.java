
package sunw.demo.quote;

import java.awt.*;
import java.awt.event.*;
import java.util.Date;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.beans.PropertyChangeSupport;
import java.beans.PropertyChangeListener;



public class QuoteMonitor extends Panel
{
  private transient QuoteListener quoteListener = null;
  private transient QuoteServer quoteServer = null;

  private PropertyChangeSupport changes = new PropertyChangeSupport(this);
  private QuoteEvent currentQuote = null;
  
  private PropertyPanel quotePanel = new PropertyPanel();
  
  private static final String sSymbol = "Stock";
  private static final String sPrice = "Price";
  private static final String sServer = "Server";
  private static final String sStatus = "Status";
  
  Frame dialog = null;


  public QuoteMonitor()
  {
    super();

    String hostName;
    try {
      InetAddress host = InetAddress.getLocalHost();
      hostName = host.getHostName();
    }
    catch (UnknownHostException e) {
      hostName = "mushmouth";
    }

    quotePanel.appendProperty(sSymbol, "sunw");
    quotePanel.appendProperty(sPrice);
    quotePanel.appendProperty(sServer, hostName);
    quotePanel.appendProperty(sStatus, "Initializing ...");
    add(quotePanel);

    Thread initThread = new InitQuoteMonitorThread();
    initThread.start();
  }

  class InitQuoteMonitorThread extends Thread
  {
    public void run()
    {
      QuoteMonitor.this.connectToServer();
    }
  }

  synchronized void connectToServer()
  {
    String host = quotePanel.getPropertyValue(sServer);
    quotePanel.setPropertyValue(sStatus, "Not Connected");

    String quoteServerURL = "rmi://" + host + ":2330/QuoteServer";

    try {
      /* If we're already listening to the QuoteServer then
       * disconnect the old QuoteListener.
       */
      if ((quoteListener != null) && (quoteServer != null)) {
	try {
	  quoteServer.removeQuoteListener(quoteListener);
	}
	catch (RemoteException e) {
	}
      }

      /* Lookup the QuoteServer on the specified host and add 
       * a QuoteListener.
       */
      quoteServer = (QuoteServer)Naming.lookup(quoteServerURL);
      quoteListener = new QuoteListenerImpl(this);
      quoteServer.addQuoteListener(quoteListener);
      quotePanel.setPropertyValue(sStatus, "Connected to " + host);
    }
    catch (Exception e) {
      quotePanel.setPropertyValue(sStatus, "Bad server host");
      quoteServer = null;
      quoteListener = null;
      showBadServerDialog(host);
    }
  }


  public String getStockSymbol()
  {
    return quotePanel.getPropertyValue(sSymbol);
  }

  public void setStockSymbol(String symbol)
  {
    quotePanel.setPropertyValue(sSymbol, symbol);
  }

  
  void closeBadServerDialog()
  {
    dialog.dispose();
    dialog = null;
  }


  class DialogOKHandler implements ActionListener
  {
    public void actionPerformed(ActionEvent e)
    {
      closeBadServerDialog();
    }
  }


  class DialogCloseHandler extends WindowAdapter
  {
    public void windowClosing(WindowEvent e)
    {
      closeBadServerDialog();
    }
  }


  public void showBadServerDialog(String host)
  {
    String message[] = {
      "The QuoteMonitor Bean couldn't connect to the QuoteServer",
      "on host machine \"" + host + "\".  Either specify a hostname",
      "where the QuoteServer is already running or start/restart",
      "one on \"" + host + "\".",
      "  ",
      "You can start the QuoteServer from the demo directory with:",
      "   start nmake -f quote.mk run (Windows)",
      "   gnumake -f quote.gmk run & (Solaris)"
    };

    String title = 
      "QuoteMonitor Warning: \"" + host + "\" Couldn't Connect to Quote Server";

    if (dialog != null) {
      dialog.setTitle(title);
      return;
    }

    // dialog = new Dialog(new Frame(), title, false);
    dialog = new Frame(title);
    dialog.setLayout(new BorderLayout(3, 3));
    dialog.addWindowListener(new DialogCloseHandler());

    Panel messagePanel = new Panel();
    messagePanel.setLayout(new GridLayout(message.length, 1));
    for(int i = 0; i < message.length; i++)
      messagePanel.add(new Label("  " + message[i]));

    Button okButton = new Button(" OK ");
    okButton.addActionListener(new DialogOKHandler());

    Panel buttonPanel = new Panel();
    buttonPanel.add(okButton);

    dialog.add("Center", messagePanel);
    dialog.add("South", buttonPanel);
    dialog.pack();
    dialog.show();
  }


  public String getQuoteServerHost()
  {
    return quotePanel.getPropertyValue(sServer);
  }

  public void setQuoteServerHost(String host)
  {
    quotePanel.setPropertyValue(sServer, host);
    Thread initThread = new InitQuoteMonitorThread();
    initThread.start();
  }

  private void maybeFirePropertyChange(String s, Object oldValue, Object newValue)
  {
    changes.firePropertyChange(s, oldValue, newValue);
  }

  private void maybeFirePropertyChange(String s, double oldValue, double newValue)
  {
    changes.firePropertyChange(s, new Double(oldValue), new Double(newValue));
  }

  private void maybeFirePropertyChange(String s, long oldValue, long newValue)
  {
    changes.firePropertyChange(s, new Long(oldValue), new Long(newValue));
  }

  public void quoteChanged(QuoteEvent x) 
  {
    quotePanel.setPropertyValue(sSymbol, x.getSymbol());
    quotePanel.setPropertyValue(sStatus, "Quote " + x.getDate().toString());
    quotePanel.setPropertyValue(sPrice, String.valueOf(x.getPrice()));

    QuoteEvent oldQuote = currentQuote;
    currentQuote = x;

    if (oldQuote != null) {
      maybeFirePropertyChange("symbol", oldQuote.getSymbol(),  x.getSymbol());
      maybeFirePropertyChange("date", oldQuote.getDate(),  x.getDate());
      maybeFirePropertyChange("price", oldQuote.getPrice(),  x.getPrice());
      maybeFirePropertyChange("bid", oldQuote.getBid(),  x.getBid());
      maybeFirePropertyChange("ask", oldQuote.getAsk(),  x.getAsk());
      maybeFirePropertyChange("open", oldQuote.getOpen(),  x.getOpen());
      maybeFirePropertyChange("volume", oldQuote.getVolume(),  x.getVolume());
    }
  }


  public void addPropertyChangeListener(PropertyChangeListener x)
  {
    changes.addPropertyChangeListener(x);
  }

  public void removePropertyChangeListener(PropertyChangeListener x)
  {
    changes.removePropertyChangeListener(x);
  }


  private void writeObject(ObjectOutputStream s)
    throws IOException
  {
    s.defaultWriteObject();
  }

  private void readObject(ObjectInputStream s)
    throws ClassNotFoundException, IOException 
  {
    s.defaultReadObject();
    Thread initThread = new InitQuoteMonitorThread();
    initThread.start();
  }
}


