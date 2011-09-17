
package sunw.demo.quote;

import java.util.Properties;
import java.util.Hashtable;
import java.util.Vector;
import java.util.EventObject;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.rmi.RMISecurityManager;
import java.net.InetAddress;
import java.net.UnknownHostException;


/** 
 * The QuoteServer implementation.  The QuoteServer object
 * starts a thread that polls a quote source and delivers 
 * QuoteEvents to QuoteListeners.
 * <p>
 * Stock information can be collected from one of two sources: "Local" 
 * or "Yahoo".  The latter is a real (delayed) data feed that reports
 * quotes in a format that can be easily parsed.  The "Local" quote 
 * source is just a random number generator.  The advantage of 
 * the local source is that it can produce quotes quickly, this
 * can make demos more interesting.
 */

public class QuoteServerImpl 
  extends UnicastRemoteObject 
  implements QuoteServer
{
  private int quoteEventRate = 2;
  private Thread quoteThread = null;
  private String quoteSource = "Local";
  private boolean quoteThreadStopped = false;
  private Vector quoteListeners = new Vector();


  /** 
   * Construct a QuoteServer implementation and start its polling thread.
   */
  QuoteServerImpl() throws RemoteException
  {
    super();
    quoteThread = new QuoteThread(this);
    quoteThread.start();
  }


  /** 
   * Get quotes for every stock symbol some QuoteListener is interested 
   * in and then apply each listeners QuoteListener.quoteChanged()
   * method to a new QuoteEvent object.
   * <p>
   * This method is not synchronized, notifying observers can
   * take a while.
   */

  void deliverQuotes()
  {
    /* Make a local (frozen) copy of the list of the current list of 
     * QuoteListeners.  If there are no listeners, we're done
     */

    Vector l;
    int n;

    synchronized (this)
    {
      l = (Vector)quoteListeners.clone();
      n = l.size();
      if (n == 0) return;
    }

    /* Make a list of the unique stock symbols that quoteListeners are 
     * interested in.
     */

    Vector symbols = new Vector();
    for(int i = 0; i < l.size(); i++) {
      QuoteListener quoteListener = (QuoteListener)l.elementAt(i);
      try {
	String s = quoteListener.getStockSymbol();
	if ((s != null) && !(symbols.contains(s)))
	  symbols.addElement(s);
      }
      catch (RemoteException e) {
      }
    }
    
    /* Get quotes from quoteSource for each symbol and then deliver
     * the QuoteEvents to the QuoteListeners that want them.
     */

    Hashtable eventTable; 

    if (quoteSource.equalsIgnoreCase("Yahoo"))
      eventTable = YahooQuote.getQuotes(this, symbols);
    else 
      eventTable = LocalQuote.getQuotes(this, symbols);

    if (eventTable == null)
      return;

    QuoteListener quoteListener = null;

    for(int i = 0; i < l.size(); i++) {
      try {
	quoteListener = (QuoteListener)l.elementAt(i);
	String symbol = quoteListener.getStockSymbol();
	QuoteEvent e = (QuoteEvent)eventTable.get(symbol.toUpperCase());
	quoteListener.quoteChanged(e);
      }
      catch (RemoteException e) {
	// e.printStackTrace();
	System.err.println("disconnecting listener " + quoteListener);
	quoteListeners.removeElement(quoteListener);
      }
    }
  }


  /** 
   * @return the quote source polling rate in seconds
   */
  public int getQuoteEventRate()
  {
    return quoteEventRate;
  }

  /** 
   * Sets the quote source polling rate.
   * @param x the polling rate in seconds
   */

  public void setQuoteEventRate(int x)
  {
    quoteEventRate = x;
  }


  /**
   * The specified QuoteListeners <b>quoteChanged</b> method will 
   * be called each time new quote data is available.  The QuoteListener
   * object is added to a list of QuoteListeners managed by 
   * this server, it can be removed with removeQuoteListener.
   * 
   * @see #removeQuoteListener
   * @param l the QuoteListener
   */      
  public synchronized void addQuoteListener(QuoteListener x) throws RemoteException
  {
    quoteListeners.addElement(x);
  }

  /** 
   * Remove this QuoteListener from the servers internal list.  If the
   * QuoteListener isn't on the list, silently do nothing.
   * 
   * @see #addQuoteListener
   * @param l the QuoteListener
   */      
  public synchronized void removeQuoteListener(QuoteListener x) throws RemoteException
  {
    quoteListeners.removeElement(x);
  }


  /** 
   * The HTTP proxy host and port number are stored in the System
   * property list under "http.proxyHost" and "http.proxyPort".
   * We just combine the two values into a single object here.
   * @return the HTTP proxy host and port number for this server
   * 
   * @see #setHttpProxy
   */      
  public HttpProxy getHttpProxy() throws RemoteException
  {
    Properties systemProps = System.getProperties();
    String host = systemProps.getProperty("http.proxyHost");
    String portString = systemProps.getProperty("http.proxyPort");
    try {
      int port = Integer.parseInt(portString, 10);
      return new HttpProxy(host, port);
    }
    catch (NumberFormatException e) {
      throw new RemoteException("can't parse port number", e);
    }
  }

  /** 
   * If the specified host is valid and the port number is 
   * positive then set the System properties: "http.proxyHost" 
   * and "http.proxyPort".  Otherwise throw a RemoteException.
   * The HTTP proxy values only matter if the quote source is 
   * "Yahoo", i.e. if we're going to the internet for data.
   * 
   * @param x the new values for host and port
   * @see #getHttpProxy
   */      
  public void setHttpProxy(HttpProxy x) throws RemoteException
  {
    String host = x.getHost();
    int port = x.getPort();

    if (port < 0)
      throw new RemoteException("invalid port number");

    InetAddress addr = null;
    try {
      addr = InetAddress.getByName(host);
    } 
    catch (UnknownHostException e) {
      throw new RemoteException("not a valid host", e);
    }

    Properties systemProps = System.getProperties();
    systemProps.put("http.proxyHost", host);
    systemProps.put("http.proxyPort", String.valueOf(port));
  }


  /**
   * @return a string that represents the source for quote data
   * @see #setQuoteSource
   */
  public String getQuoteSource() throws RemoteException
  {
    return quoteSource;
  }

  /**
   * Specify where quote data will come from.  If quoteSource is "Local"
   * then the sunw.demo.LocalQuote is used, it generates moderately
   * random data.  If quoteSource is "Yahoo" then the sunw.demo.YahooQuote
   * class is used, it collects quote data from the YahooQuote web site.
   * 
   * @param x must be "Yahoo" or "Local"
   * @see #getQuoteSource
   */
  public void setQuoteSource(String x) throws RemoteException
  {
    quoteSource = x;
  }


  boolean isQuoteThreadStopped()
  {
    return quoteThreadStopped;
  }


  void stopQuoteThread()
  {
    quoteThreadStopped = true;
  }
}



class QuoteThread extends Thread
{
  private QuoteServerImpl quoteServerImpl;

  QuoteThread(QuoteServerImpl x)
  {
    super();
    quoteServerImpl = x;
  }

  public void run()
  {
    try {
      while(!quoteServerImpl.isQuoteThreadStopped()){
	// TBD - get time here
	quoteServerImpl.deliverQuotes();
	// TBD sleep remaining time here
	sleep(quoteServerImpl.getQuoteEventRate() * 1000);
      }
    }
    catch (InterruptedException e) {
      quoteServerImpl.stopQuoteThread();
    }
  }
}
