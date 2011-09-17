/**
   QueryHandler.java
   Jan. 25
   @author Gary Dmytryk
   @version $Id: QueryHandler.java,v 1.8 2001/01/28 21:55:26 jeff Exp $
*/

import java.util.*;


public class QueryHandler {

    private IndexWrapper index;
    /**
       Constructor
    */
  QueryHandler(ControlHandler control) {
    // a test
    this.control = control;
    index = new IndexWrapper("./mp3");
    control.setIndex(index);
  }
    
    /**
       Constructor for initiating a query.
       Will take input from user interface whenever that is built
       for both minSpeed and searchString.
    */
    public void startQuery(QueryObject queryobject) {
	
//  	int payloadDescriptor = -181;
//  	int ttl = 10;
//  	// I will set this search.
//  	searchString = "Fear of Music";
//  	int payloadLength = searchString.length() + 2;
//  	short minSpeed = 56;
//  	QueryObject queryobject = new QueryObject (payloadDescriptor,
//  						   ttl,
//  						   payloadLength,
//  						   minSpeed,
//  						   searchString);
	queryobject.searchString = queryobject.searchString.toLowerCase();
	Gateway.deliver(queryobject);
    }

    /**
       Pull the search string out of the incoming Query and pass
       it on to the search routine.
    */
    public void startSearch(QueryObject queryobject) {

      control.report("Received Query, sending to wackadex.");
      Vector vector = new Vector();
      String search = queryobject.getSearchString();

      vector = index.searchIndex(search);
  
	if (vector.size() > 0) {
    control.report("Query Search: Found results");
	    byte [] messageId = queryobject.messageId;
	    int payloadDescriptor  = Utility.QUERY_HIT;
	    int ttl = queryobject.ttl;
	    int hops = queryobject.hops;
	    int payloadLength = queryobject.payloadLength;
	    int numHits = vector.size();
	    short port = (short)Utility.getPort();
	    byte[] ipAddress = Utility.getIp();
	    int minSpeed = queryobject.minSpeed;
	    Vector resultSet = vector;
	    byte[] serventID = Utility.getServentId();
	    
	    QueryHitObject queryhitobject = new QueryHitObject(
							       messageId, payloadDescriptor,
							       ttl, hops, payloadLength,
							       numHits, port,
							       ipAddress, minSpeed,
							       resultSet, serventID);

	    // Send to gateway
      // System.out.println("Sending qHit to socket " + queryobject.getSocketId());
	    Gateway.deliver(queryhitobject, queryobject.getSocketId());
	}
  else
  {
    control.report("Query search: No results.");
  }
  
    }
    
  // fields
  private ControlHandler control;
}
