
package sunw.demo.quote;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.EventListener;



/** 
 * This is the java.rmi.Remote interface that QuoteListeners must implement.
 * QuoteListeners monitor a single stock, QuoteEvents are delivered
 * whenever new data for that stock is available.  The standard
 * event mechanism is used for QuoteEvents with one wrinkle:
 * QuoteListener implementations must extend UnicastRemoteObject.  
 * 
 */

public interface QuoteListener extends Remote
{
  /**
   * Called by the QuoteServer each time a new QuoteEvent is availble
   * for the stock symbol returned by getStockSymbol.
   * @param x timestamped quote data for one stock
   * @see #getStockSymbol()
   */
  void quoteChanged(QuoteEvent x) throws RemoteException;

  /**
   * Called by the QuoteServer before polling the quote source
   * to discover what stock we're monitoring.
   * 
   * @return the symbol for the stock we're interested in
   */
  String getStockSymbol() throws RemoteException;
}


