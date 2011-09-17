
package sunw.demo.quote;

import java.util.Vector;
import java.util.Date;
import java.util.Hashtable;


/**
 * Produces random stock quotes.  Useful for demos where
 * and internet connection isn't available or the market
 * has closed.
 *
 * @see sunw.demo.quote.YahooQuote 
 */

public class LocalQuote
{
  static private final double dx = Math.PI / 8;
  static private int x = 0;

  static Hashtable getQuotes(QuoteServer server, Vector symbols)
  {
    Hashtable rv = new Hashtable();
    for(int i = 0; i < symbols.size(); i++) {
      String symbol = ((String)symbols.elementAt(i)).toUpperCase();
      int iprice = (int)((50.0 * Math.random()) + (10.0 * (1.0 + Math.sin(x++ * dx))));
      double price = iprice + ((int)(Math.random() * 8.0) * 0.125);
      double bid = 0.9 * price;
      double ask = 1.1 * price;
      double open = 25.0;
      long volume = x * 100000;
      Date date = new Date();
      QuoteEvent event = new QuoteEvent(server, symbol, date, price, bid, ask, open, volume);
      rv.put(symbol, event);
    }
    
    return rv;
  }
}


