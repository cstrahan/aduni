package sunw.demo.quote;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.EventListener;


/** 
 * The QuoteListener implementation used by the QuoteMonitor bean.  When
 * quote events arrive we notify the QuoteMonitor directly.
 * 
 * @see sunw.demo.quote.QuoteMonitor
 * @see sunw.demo.quote.QuoteListener
 */

class QuoteListenerImpl extends UnicastRemoteObject implements QuoteListener, EventListener
{
  private QuoteMonitor quoteMonitor;

  /**
   * Construct a remote object reference.
   */
  public QuoteListenerImpl(QuoteMonitor x) throws RemoteException
  {
    super();
    quoteMonitor = x;
  }
  
  /**
   * Just forward the QuoteEvent to the QuoteMonitor.
   */
  public void quoteChanged(QuoteEvent x) throws RemoteException
  {
    quoteMonitor.quoteChanged(x);
  }

  /**
   * @return the QuoteMonitors stock symbol.
   */
  public String getStockSymbol() throws RemoteException
  {
    return quoteMonitor.getStockSymbol();
  }
}

