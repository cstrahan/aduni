
package sunw.demo.quote;

import java.util.Properties;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;
import sun.rmi.registry.RegistryImpl;


class QuoteServerApp
{
  static final int registryPort = 2330;
  static QuoteServerImpl server = null;
  static RegistryImpl registry = null;

  /** 
   * Start a QuoteServer running and bind it to "QuoteServer"
   * in the RMI name registry on the local machine.  
   */
  public static void main(String args[])
  {
    Properties systemProps = System.getProperties();
    systemProps.put("http.proxyHost", "webcache1.Eng");
    systemProps.put("http.proxyPort", "8080");

    QuoteServerApp app = new QuoteServerApp();
    QuoteServerGUI gui = new QuoteServerGUI(app);
    gui.show();

    try {
      registry = new RegistryImpl(registryPort);
      server = new QuoteServerImpl();
      registry.rebind("QuoteServer", server);

      gui.refreshAll();
    }
    catch (Exception e) {
      e.printStackTrace();
      return;
    }
    
    gui.showStatus("Ready");
  }
}






