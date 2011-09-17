
package sunw.demo.quote;

import java.rmi.Remote;
import java.rmi.RemoteException;


/** 
 * This is the java.rmi.Remote interface that QuoteServers must implement,
 * it's the interface seen by remote clients the QuoteMonitor bean.
 *
 * @see sunw.demo.quote.QuoteServerImpl
 */

public interface QuoteServer extends Remote 
{
  void addQuoteListener(QuoteListener x) throws RemoteException;
  void removeQuoteListener(QuoteListener x) throws RemoteException;

  HttpProxy getHttpProxy() throws RemoteException;
  void setHttpProxy(HttpProxy x) throws RemoteException;
}
