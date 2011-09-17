import java.util.*;

class QHandler extends Thread
{
    //static variables
    public static Map qt;  //query table

    //instance variables
    Query query;
    QueryHit queryHit;
    IPAddress myIP;
    IPAddress queryIP; 
    ResultSet searchResult;
    int numHits;
    int port;
    int speed;
    ResultSet result;
    byte[] serventID;
    byte[] queryID;
   
    public QHandler (IPAddress queryIP, Query query)
    {
	this.query = query;
	query.setIP(queryIP);  //set IPAddress of query
    }

    public static void initQueryTable()
    {
	qt = new Hashtable (5000);
    }

    //QHandler handles incoming queries.  
    public void run()
    {
	//hmmm.. this seems like potential bug.  I want to check that query is not in table.  But even if query table contains key,
	//that does not necessarily mean it is in table, b/c two queries can have SAME HASHCODE VALUE.  I need to have some other means.
	//Will talk to Rusty @ this on Monday.

	if (!qt.containsKey(query)) //check that query is not already in table
	    {
		Searcher.inform(query); // Give information to the Search Monitor panel
		NetworkManager.writeButOne(query.getIP(), query);  /*Query is forwarded to all connected nodes
								     except one from which query came. */
		qt.put((Packet) query, query);    //add query to table, indexed by its unique MessageID
		searchResult = SharedDirectory.search(query.getSearchString());  //check shared directory for query match
		numHits = searchResult.getSize();

		if (numHits != 0)   //package a query hit to send out if there is at least one query match
		    {
			queryID = query.getMessageID();
			port = Mine.getPort();
			myIP = Mine.getIPAddress();
			speed = Mine.getSpeed();
			serventID = Mine.getServentIdentifier();
			queryHit = new QueryHit(numHits, port, myIP, speed, searchResult, serventID, queryID);
			NetworkManager.writeToOne(query.getIP(), queryHit);  //send qHit back to node that sent original query
		    }
	    }
    }
}




