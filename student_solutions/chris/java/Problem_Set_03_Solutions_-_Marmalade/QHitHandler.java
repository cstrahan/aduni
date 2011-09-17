import java.util.*;

/**QHitHandler will process query hits that match queries forwarded by us, not queries
   that originated from us (which will be handled by searcher).*/

class QHitHandler extends Thread
{
    //instance variables
    QueryHit queryHit;
    IPAddress queryIP;   /**ipAddress of original query is what matters.  We need this
		            to route queryHit appropriately*/
    IPAddress ip;   //ipAddress of node that sent query hit.  Not important.
    Query queryMatch;

    public QHitHandler (IPAddress ip, QueryHit queryHit)
    {
	this.queryHit = queryHit; 
	this.ip = ip; 
    }

    public void run()
    {
	if (QHandler.qt.containsKey(queryHit)) //note that this time we check for presence of query in table
	    {
		queryMatch = (Query) QHandler.qt.get((Packet) queryHit);  /**Matching query hit is used as the key to
									     obtain the original query.*/
		queryIP = queryMatch.getIP();  //Get original query in order to extract its addressing information.
		NetworkManager.writeToOne(queryIP, queryHit);  //Pass qHit and addressing information onto Network Manager.
	    }
    }
}


