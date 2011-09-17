
package sunw.demo.quote;


import java.util.Vector;
import java.util.Date;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.Serializable;
import java.net.URL;
import java.text.SimpleDateFormat;


public class YahooQuote implements Serializable
{
  static Hashtable getQuotes(QuoteServer x, Vector symbols)
  {
    String symbolsString = "";
    for(int i = 0; i < symbols.size(); i++) {
      String symbol = (String)symbols.elementAt(i);
      symbolsString += ((i != 0) ? "," : "") + symbol.toUpperCase();
    }

    String quoteURLString =
      "http://quote.yahoo.com/download/javasoft.beans?SYMBOLS=" + 
      symbolsString + 
      "&format=sl";

    Hashtable rv = new Hashtable();
    BufferedReader in = null;

    // SimpleDateFormat dateFormat = new SimpleDateFormat("M/dd/yy");
    // SimpleDateFormat timeFormat = new SimpleDateFormat("hh:mma");

    try {
      URL quoteURL = new URL(quoteURLString);
      in = new BufferedReader(new InputStreamReader(quoteURL.openStream()));

      String line;

      while(null != (line = in.readLine())) {
	StringTokenizer t = new StringTokenizer(line, ",\"");

	String symbol = t.nextToken();
	double price = Double.valueOf(t.nextToken()).doubleValue();
	
	// Date date = dateFormat.parse(t.nextToken());
	t.nextToken();
	Date date = new Date();

	// Date lastTrade = timeFormat.parse(t.nextToken()); 
	t.nextToken();
	Date lastTrade = date;

	double change = Double.valueOf(t.nextToken()).doubleValue();
	double open = Double.valueOf(t.nextToken()).doubleValue();
	double bid = Double.valueOf(t.nextToken()).doubleValue();
	double ask = Double.valueOf(t.nextToken()).doubleValue();
	int volume = Integer.valueOf(t.nextToken()).intValue();
	
	QuoteEvent qe = new QuoteEvent(x, symbol, date, price, bid, ask, open, volume);
	rv.put(symbol.toUpperCase(), qe);
      }
    }
    catch (Exception e) {
      e.printStackTrace();
    }
    finally {
      try {
	if (in != null)
	  in.close();
      }
      catch (IOException e) {
	e.printStackTrace();
      }
    }
    
    return rv;
  }
}
