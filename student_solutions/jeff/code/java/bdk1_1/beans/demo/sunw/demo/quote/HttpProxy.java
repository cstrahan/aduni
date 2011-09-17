
package sunw.demo.quote;

import java.io.Serializable;


public class HttpProxy implements Serializable
{
  private String host;
  private int port;

  HttpProxy(String hostArg, int portArg)
  {
    host = hostArg;
    port = portArg;
  }

  String getHost() {return host;}
  int getPort() {return port;}
}

