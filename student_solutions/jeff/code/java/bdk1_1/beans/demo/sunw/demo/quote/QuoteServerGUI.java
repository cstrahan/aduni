
package sunw.demo.quote;

import java.awt.*;
import java.awt.event.*;
import java.rmi.RemoteException;


class QuoteServerGUI extends Frame
{
  private QuoteServerApp quoteServerApp;
  private PropertyPanel propsPanel = new PropertyPanel();

  private static String sProxyHost = "HTTP Proxy Host";
  private static String sProxyPort = "HTTP Proxy Port";
  private static String sSource = "Quote Source";
  private static String sStatus = "Server Status";


  QuoteServerGUI(QuoteServerApp x)
  {
    quoteServerApp = x;

    setLayout(new BorderLayout());
    setTitle("QuoteServer");
    addWindowListener(new WindowEventHandler());

    propsPanel = new PropertyPanel();
    propsPanel.appendProperty(sProxyHost, "<unknown>");
    propsPanel.appendProperty(sProxyPort, "<unknown>");
    propsPanel.appendProperty(sSource, "Local");
    propsPanel.appendProperty(sStatus, "Iniitializing ...");

    propsPanel.addPropertyActionListener(sProxyHost, new ChangeProxyHost());
    propsPanel.addPropertyActionListener(sProxyPort, new ChangeProxyPort());
    propsPanel.addPropertyActionListener(sSource, new ChangeQuoteSource());

    propsPanel.setPropertyEditable(sProxyHost, true);
    propsPanel.setPropertyEditable(sProxyPort, true);
    propsPanel.setPropertyEditable(sSource, true);

    add("Center", propsPanel);
    pack();
  }


  void showStatus(String x)
  {
    propsPanel.setPropertyValue(sStatus, x);
  }


  void refreshAll()
  {
    try {
      HttpProxy proxy = QuoteServerApp.server.getHttpProxy();
      propsPanel.setPropertyValue(sProxyHost, proxy.getHost());
      propsPanel.setPropertyValue(sProxyPort, String.valueOf(proxy.getPort()));
    }
    catch (RemoteException e) {
      e.printStackTrace();
    }
  }


  void setHttpProxy()
  {
    try {
      String host =  propsPanel.getPropertyValue(sProxyHost);
      String port =  propsPanel.getPropertyValue(sProxyPort);
      HttpProxy proxy = new HttpProxy(host, Integer.valueOf(port).intValue());
      QuoteServerApp.server.setHttpProxy(proxy);
      propsPanel.setPropertyValue(sStatus, "Ready");
    }
    catch (RemoteException e) {
      // e.printStackTrace();
      propsPanel.setPropertyValue(sStatus, "Bad HTTP proxy host/port");
    }
  }


  class ChangeProxyHost implements ActionListener
  {
    public void actionPerformed(ActionEvent e)
    {
      setHttpProxy();
    }
  }


  class ChangeProxyPort implements ActionListener
  {
    public void actionPerformed(ActionEvent e)
    {
      setHttpProxy();
    }
  }


  class ChangeQuoteSource implements ActionListener
  {
    public void actionPerformed(ActionEvent e)
    {
      String source = propsPanel.getPropertyValue(sSource);

      if (source.equalsIgnoreCase("local")) 
	source = "Local";

      else if (source.equalsIgnoreCase("yahoo"))
	source = "Yahoo";

      else {
	propsPanel.setPropertyValue(sStatus, "Source must be Local or Yahoo");
	source = null;
      }

      try {
	if (source != null) {
	  QuoteServerApp.server.setQuoteSource(source);
	  propsPanel.setPropertyValue(sSource, source);
	  propsPanel.setPropertyValue(sStatus, "Ready");
	}
      }
      catch (RemoteException x) {
	propsPanel.setPropertyValue(sStatus, "Set quote source failed!");
      }
    }
  }


  class WindowEventHandler extends WindowAdapter
  {
    public void windowClosing(WindowEvent e) 
    {
      System.exit(0);
    }
  }
}



