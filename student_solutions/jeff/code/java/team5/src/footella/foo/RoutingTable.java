import java.util.*;
import java.math.*;


/**
   Should probably just have different tables for each kind of message -
   will get around to reorganizing this if there's enough time -
   @version $Id: RoutingTable.java,v 1.2 2001/01/28 16:54:23 jeff Exp $
*/
public class RoutingTable extends Thread
{
    private static Map oldRouteTable = new HashMap();
    private static Map newRouteTable = new HashMap();
    private static Map oldQueryHitTable = new HashMap();
    private static Map newQueryHitTable = new HashMap();

    public static final byte[] DUMMYPINGID = {1, 1, 1, 1};
    public static final byte[] DUMMYQUERYID = {127, 127, 127, 127};
    
    public void run()
    {
	try
	    {
		oldRouteTable = newRouteTable;
		newRouteTable = new HashMap();
		oldQueryHitTable = newQueryHitTable;
		newQueryHitTable = new HashMap();
		sleep(30000);
	    }
	catch(InterruptedException e) { }
    }

    /**	@param ipId is an ipAddress (4-byte - dummy if ping or query) for everything but QueryHits;
	QueryHits get 16-byte serventId's.
	@return true if message not seen before - ie MessageObject should be created and sent to
	Handler; false if message previously seen - ie kill message.
    */
    public static boolean receiveCheck(byte[] messageId, int payloadDescriptor, byte[] ipId,
				      int socketId)
    {
	// unbelievably dumb - can only put OBJECTS in a hashtable
	Integer sock = new Integer(socketId); 

	if (payloadDescriptor == 0x81) // QUERYHIT
	    {
		QueryHitKey key = new QueryHitKey(messageId, ipId);
		if (oldQueryHitTable.containsKey(key)) return false; // query hit seen before
		if (newQueryHitTable.put(key, sock) != null) return false; 
		else return true; // new query hit
	    }
	else
	    {
		RouteKey key = new RouteKey(messageId, payloadDescriptor, ipId);
		if (oldRouteTable.containsKey(key)) return false; // message seen before
		if (newRouteTable.put(key, sock) != null) return false;
		else return true; // new message
	    }
    }

    /**
       @return a socketId (or -1 as a no-socketId value):
       if message == Ping: send to all sockets EXCEPT socketId;
       if message == Pong: send ONLY to socketId (if null, don't send);
       if message == Query: send to all sockets EXCEPT socketId;
       if message == QueryHit: send ONLY to socketId (if null, don't send);
       if message == Push: send ONLY to socketId (if null, don't send).
    */
    public static int sendCheck(MessageObject message)
    {
	byte[] messageId = message.getMessageId();
	int payloadDescriptor = message.getPayloadDescriptor();
	
	if (payloadDescriptor == 0x00)  // PING
	    {
		// not necessary in this case to look up socketId;
		// just take from Ping object: if it is a routed ping, return
		// incoming socket; if it originates here, return -1.

		return(message.getSocketId());
	    }

	else if (payloadDescriptor == 0x01) // PONG
	    {
		// check to see if there was a corresponding ping received;
		// if so, return its socketId; if not, return -1 (ie WE
		// initiated the ping, so the pong stops with us).
		
		int pingDescriptor = 0x00;
		byte[] ipAddress = DUMMYPINGID;
		RouteKey key = new RouteKey(messageId, pingDescriptor, ipAddress);
		Object onew = newRouteTable.get(key);
		Object oold = oldRouteTable.get(key);
		if (onew != null && onew.getClass() == Integer.class)  // getClass() for error check
		    {
			int sock = ((Integer)onew).intValue();
			return sock;
		    }
		if (oold != null && oold.getClass() == Integer.class)
		    {
			int sock = ((Integer)oold).intValue();
			return sock;
		    }
		else return -1;		
	    }

	else if (payloadDescriptor == 0x80) // QUERY
	    {
		// as with ping
		return (message.getSocketId());
	    }

	else if (payloadDescriptor == 0x81) // QUERYHIT
	    {
		// as with pong
		int queryDescriptor = 0x80;
		byte[] ipAddress = DUMMYQUERYID;
		RouteKey key = new RouteKey(messageId, queryDescriptor, ipAddress);
		Object onew = newRouteTable.get(key);
		Object oold = oldRouteTable.get(key);
		if (onew != null && onew.getClass() == Integer.class)
		    {
			int sock = ((Integer)onew).intValue();
			return sock;
		    }
		if (oold != null && oold.getClass() == Integer.class)
		    {
			int sock = ((Integer)oold).intValue();
			return sock;
		    }
				    
		else return -1; // if -1, QueryHit is (probably) for us
	    }

	else if (payloadDescriptor == 0x40) // PUSH
	    {
		Push push = (Push)message;
		byte[] serventId = push.getServentId();
		QueryHitKey key = new QueryHitKey(messageId, serventId);
		Object onew = newQueryHitTable.get(key);
		Object oold = oldQueryHitTable.get(key);
		if (onew != null && onew.getClass() == Integer.class)
		    {
			int sock = ((Integer)onew).intValue();
			return sock;
		    }
		if (oold != null && oold.getClass() == Integer.class)
		    {
			int sock = ((Integer)oold).intValue();
			return sock;
		    }
		else return -1;  // if -1, Push is (probably) for us
	    }
	
	else return -1;
		 
    }
    /*    
    public static void main(String[] args)
    {
	// testing...
	byte[] messageId = {5, 34, 67, 124, 102, 56, 5, 0, 32, 57,
			    98, 100, 23, 45, 90, 126};
	int payloadDescriptor = 0x01;
	byte[] ipAddress = {123, 125, 15, 127};
	byte[] serventId = {34, 6, 3, 6, 45, 73, 78, 43, 42, 90,
			    15, 68, 93, 0, 121, 126};
	RouteKey rkey1 = new RouteKey(messageId, payloadDescriptor, ipAddress);
	RouteKey rkey2 = rkey1;
	QueryHitKey qhkey1 = new QueryHitKey(messageId, serventId);
	QueryHitKey qhkey2 = qhkey1;
	//	System.out.println("" + ((Integer)routeTable.get(rkey1)).intValue());
	System.out.println(RoutingTable.receiveCheck(messageId, 0x01, ipAddress, 1));
	System.out.println("" + ((Integer)routeTable.get(rkey1)).intValue());
	System.out.println(RoutingTable.receiveCheck(messageId, 0x01, ipAddress, 2));
	System.out.println("" + ((Integer)routeTable.get(rkey1)).intValue());
	System.out.println(RoutingTable.receiveCheck(messageId, 0x81, serventId, 3));
	System.out.println("" + ((Integer)queryHitTable.get(qhkey1)).intValue());
	System.out.println(RoutingTable.receiveCheck(messageId, 0x81, serventId, 4));
	System.out.println("" + ((Integer)queryHitTable.get(qhkey1)).intValue());
	System.out.println(rkey1.equals(rkey2));
	System.out.println(qhkey1.equals(qhkey2));
	System.out.println(rkey1.hashCode() + " " + rkey2.hashCode());
	System.out.println(qhkey1.hashCode() + " " + qhkey2.hashCode());
	
	}*/
}

class RouteKey
{
    private BigInteger messageId;
    private BigInteger payloadDescriptor;
    private BigInteger ipAddress;
    
    public RouteKey(byte[] messageId, int payloadDescriptor, byte[] ipAddress)
    {
	this.payloadDescriptor = BigInteger.valueOf(payloadDescriptor);
	this.messageId = new BigInteger(messageId);
	this.ipAddress = new BigInteger(ipAddress);
    }
    
    /**
       Multiplies messageId and ipAddress and adds payloadDescriptor*17; then converts
       to an int.  (...??? whatever)
    */
    public int hashCode()
    {
	return ((messageId.multiply(ipAddress).
		 add(payloadDescriptor.multiply(BigInteger.valueOf(17)))).intValue());
    }
    
    public boolean equals(Object other)
    {
	if (other != null && this.getClass() == other.getClass())
	    {
		RouteKey otherRouteKey = (RouteKey)other;
		return (this.messageId.equals(otherRouteKey.messageId) &&
			this.payloadDescriptor.equals(otherRouteKey.payloadDescriptor) &&
			this.ipAddress.equals(otherRouteKey.ipAddress));
	    }
	else return false;
    }
}

class QueryHitKey
{
    private BigInteger messageId;
    private BigInteger serventId;
    
    public QueryHitKey(byte[] messageId, byte[] serventId)
    {
	this.messageId = new BigInteger(messageId);
	this.serventId = new BigInteger(serventId);
    }
    
    public int hashCode() { return messageId.multiply(serventId).intValue(); }
    
    public boolean equals(Object other)
    {
	if (other != null && this.getClass() == other.getClass())
	    {
		QueryHitKey otherQueryHitKey = (QueryHitKey)other;
		return (this.messageId.equals(otherQueryHitKey.messageId) &&
			this.serventId.equals(otherQueryHitKey.serventId));
	    }
	return false;
    }
}
    




