
package sunw.demo.quote;

import java.util.Date;
import java.util.EventObject;


/** 
 * Simple encapsulation of quote data for a single stock.  Note that 
 * this class is serializable because EventObject implements 
 * java.io.Serializable.  The QuoteServer communicates with remote 
 * QuoteListeners (used by the QuoteMonitor bean) via QuoteEvents.
 * 
 * @see sunw.demo.quote.QuoteServer
 * @see sunw.demo.quote.QuoteListener
 */

public class QuoteEvent extends EventObject
{
  private String symbol;
  private Date date;
  private double price;
  private double bid;
  private double ask;
  private double open;
  private long volume;

  QuoteEvent(
    QuoteServer source,
    String symbol,
    Date date,
    double price,
    double bid,
    double ask,
    double open,
    long volume)
  {
    super(source);
    this.symbol = symbol;
    this.date = date;
    this.price = price;
    this.bid = bid;
    this.ask = ask;
    this.open = open;
    this.volume = volume;
  }  

  public String getSymbol() {return symbol;}
  public Date getDate() {return date;}
  public double getPrice() {return price;}
  public double getBid() {return bid;}
  public double getAsk() {return ask;}
  public double getOpen() {return open;}
  public long getVolume() {return volume;}
}

