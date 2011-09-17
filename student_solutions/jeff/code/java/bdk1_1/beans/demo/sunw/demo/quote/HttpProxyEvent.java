
package sunw.demo.quote;

import java.util.EventObject;


public class HttpProxyChangedEvent extends EventObject
{
  private HttpProxy httpProxy;

  HttpProxyChangedEvent(QuoteServer source,  HttpProxy x)
  {
    super(source);
    httpProxy = x;
  }

  HttpProxy getHttpProxy() {return httpProxy;}
}




