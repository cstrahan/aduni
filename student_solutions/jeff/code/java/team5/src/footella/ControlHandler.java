/**
   ControlHandler.java
   part of footella
   @author JMR
   @version $Id: ControlHandler.java,v 1.24 2001/01/31 18:32:22 jeff Exp $

   The ControlHandler is the interface between the GUI and the
   footella engine, which consists of the gateway, server 
   index and object handlers. Generally it reports user 
   selections and also prompts the GUI to refresh information.
*/
import java.io.*;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.table.*;
import java.util.*;

public class ControlHandler
{
  /**
     Constructor
     @param server The Server
  */
  ControlHandler (MessageServer server) {
    this.server = server;
    this.connections = new Vector();
    this.results = new Vector();
    this.downloads = new Vector();
    this.monitor = new Vector();
    this.messages = new Vector();
    this.downloadHandler = new DownloadHandler(this);

    Gateway.setControlHandler(this);
    Gateway.setQueryHandler(new QueryHandler(this));
    Gateway.setQueryHitHandler(new QueryHitHandler(this));
 }

  // A few accessors and mutators, related to table data
  
  /**
     Sets the HostPanel
     @param hostPanel
  */
  public void setHostPanel(HostPanel hostPanel) { 
    this.hostPanel = hostPanel; 
  }

  /**
     Sets the QueryPanel
     @param queryPanel
  */
  public void setQueryPanel(QueryPanel q) { this.queryPanel = q; }

  /**
     Gets the data representation of the HostTable
     @return a Vector made up of ConnectionObjects
  */
  public Vector getHostVector() { return connections; }
  public void setDownloadPanel(DownloadPanel d) { this.downloadPanel = d; }
  public Vector getDownloadVector() { return downloads; }
  public Vector getMonitorVector() { return monitor; }
  public void setMessagePanel(MessagePanel m) {  this.messagePanel = m; }
  public Vector getMessageVector() { return messages; }
  /**
     Sets the current File Index. Used mostly for uploads.
     @param index the file index
  */
  public void setIndex(IndexWrapper index) {
    this.index = index;
  }
  
  /**
     Gets the current DownloadHandler
  */
  public DownloadHandler getDownloadHandler() { return downloadHandler; }
  /**
     Gets the data representation of the QueryTable
     @return a Vector made up of QueryTableObjects
  */
  public Vector getQueryVector() { return results; }

  /**
     Clears the results vector (generally, a new search has
     been called).
  */
  public void clearResults() {
    results.clear();
    queryPanel.poke();
  }

  /**
     Adds a result to the result vector
     @param q The incoming result (The ResultTableObject
     contains all the information needed for a download)
  */
  public void addResult(ResultTableObject q) {
    // this will add up to MAX_QUERY results
    if(results.size() < MAX_QUERY) {
      results.add(q);
      queryPanel.poke(1);
    }
  }

  // -------------------------------------------------------
  // a few mundane accessors
  public void setShowIncoming(boolean b) { showIncoming = b; }
  public void setShowMessages(boolean b) { showMessages = b; }
  public void setShowError(boolean b) { showError = b; }
  public void setQueryShow(boolean b) { showQuery = b; }

  /**
     Reports the arrival of an object
  */
  public void report(Object object) {
    if(showMessages) {
      if(messages.size() >= MAX_MESSAGES)
        messages.remove(messages.size() - 1);
      messages.insertElementAt(object.toString(), 0);
      messagePanel.poke();
    }
  }
  
  /**
     Reports a message
  */
  public void report(String string) { 
    if(showMessages) {
      if(messages.size() >= MAX_MESSAGES)
        messages.remove(messages.size() - 1);
      messages.insertElementAt(string, 0);
      messagePanel.poke();
    }
  }

  /**
     Reports an error
  */
  public void reportErr(String string) {
    if(showError) {
      if(messages.size() >= MAX_MESSAGES)
        messages.remove(messages.size() - 1);
      messages.insertElementAt(string, 0);
      messagePanel.poke();
    }
  }

  /**
     Reports an incoming object
  */
  public void reportIncoming(Object object) {
    if(showIncoming) {
      if(messages.size() >= MAX_MESSAGES)
        messages.remove(messages.size() - 1);
      messages.insertElementAt(object.toString(), 0);
      messagePanel.poke();
    }
  }

  /**
     Reports a new connection (probably shouldn't be used
  */
  public void newIncomingConnection(int socketId) {
    if(showMessages)
    System.out.println("> " + "new incoming connection on socket "
                       + socketId);
  }
  // -------------------------------------------------------

  /**
     Adds a connection to the connections vector
     @param handler The socket Handler containing the Socket
     @param type Incoming or Outgoing
     @return true if addition was successful
  */
  public boolean addConnection(SocketHandler handler, int type) {
    ConnectionObject object = new ConnectionObject(handler, type);
    connections.add(object);
    report(new String(("There are currently " + 
                       connections.size() + " connections open")));
    
    hostPanel.poke();
    return true;
  }

  /**
       Poke is called when some data has been changed that
       the user interface should know about
    */
  public void poke() { 
    hostPanel.poke();
    // queryPanel.poke();
    downloadPanel.poke();
  }

  public void poke(int message) {
    if(message == Utility.DOWNLOAD) 
      queryPanel.poke();
  }

  /**
     Asks the server to open a new Connection
     @param host The hostname
     @param port the port number
  */
  public boolean openConnection(String host, int port) {
    if(server.openConnection(host, port)) {
      return true;
    }
    else return false;
  }

  /**
     Removes the last connection. Not used right now.
  */
  public boolean removeLastConnection()
  {
    ConnectionObject o = (ConnectionObject)connections.get(connections.size() - 1);
    o.disconnect();
    connections.remove(connections.size() - 1);
    connections.trimToSize();
    poke();
    return true;
  }

  /**
     Removes a particular socket connection.
     @param socketId The socket to remove
     @return true on success
  */
  public boolean removeConnection(int socketId) {
    System.out.println("Removing socket " + socketId);
    for(int i = 0 ; i < connections.size() ; i++) {
      ConnectionObject o =  (ConnectionObject)connections.get(i);
      // Make sure socket is still alive!
      if(o.getSocketId() == socketId) {
        connections.remove(i);
        connections.trimToSize();
        if(o.type == 0) Utility.currentIncoming--;
        else Utility.currentOutgoing--;
        poke();
        return true;
      }
    }
    return false;
  }
  
  /**
     This method delivers an object (usually a query) to
     all live connections
     @param object The object to send
  */
  public void deliver(MessageObject object) {
      //  report and deliver
      for (int i = 0; i < connections.size(); i++)
	  {
	      ConnectionObject o = (ConnectionObject)connections.get(i);
	      report(new String("Delivering to Socket " + o.getSocketId()));
	  }
      Gateway.deliver(object, connections);
  }

  public SocketHandler getHandlerFromId(int id) {
    for(int i = 0 ; i < connections.size() ; i++) {
      ConnectionObject o = (ConnectionObject)connections.get(i);
      if(o.getSocketId() == id)
        return o.getHandler();
    }
    // no such thing!
    return null;
  }

    public ConnectionObject getConnObjFromId(int id)
    {
	for(int i = 0; i < connections.size(); i++)
	    {
		ConnectionObject o = (ConnectionObject)connections.get(i);
		if(o.getSocketId() == id) return o;
	    }
	return null; // if no such thing
    }
    
  /**
     Initializes a user download. Ask the download handler to start a
     download thread.
     @param row The row which contains the file particulars
  */
  public void download(int row) {
    ResultTableObject object = (ResultTableObject)results.get(row);
    DownloadObject transfer = new DownloadObject(object);
    downloads.add(transfer);
    downloadHandler.sendDownloadRequest(object, transfer);
  }

  public String getFileName(int fileIndex) {
    return index.lookupPath(new Integer(fileIndex));
  }

  public int getFileSize(int fileIndex) {
    return index.lookupFileSize(new Integer(fileIndex));
  }

  public void addQuery(String search) {
    if(showQuery) {
      if(monitor.size() >= MAX_QUERY)
        monitor.remove(monitor.size() - 1);
      monitor.insertElementAt(search, 0);
      downloadPanel.poke();
    }
  }


  public void updateNumberOfConnections() { hostPanel.poke(); }

  public void removeCompletedDownloads() {
    int i = 0;
    while(i < downloads.size()) {
      DownloadObject object = (DownloadObject)downloads.get(i);
      if(object.isDone()) {
        downloads.remove(i);
        downloads.trimToSize();
      }
      else i++;
    }
    queryPanel.poke();
  }

  public void removeDownloadAt(int i) {
    DownloadObject object = (DownloadObject)downloads.get(i);
    object.kill();
    downloads.remove(i);
    downloads.trimToSize();
    queryPanel.poke();
  }

  //  public void addUploadObject(UploadObject object) {} // <<< Add this in!

  // Fields
  private static int MAX_QUERY = 30;
  private static int MAX_MESSAGES = 50;
  private static boolean showIncoming = false;
  private static boolean showQuery = true;
  private static boolean showError = true;
  private static boolean showMessages = false;
  private IndexWrapper index;
  private MessageServer server;    // the server
  private DownloadHandler downloadHandler;
  private HostPanel hostPanel; // displays host information
  private DefaultTableModel model; // hostTable model
  public Vector connections;       // open hosts 
  private QueryPanel queryPanel;   // displays query information
  private Vector results;          // current list of results 
  private DownloadPanel downloadPanel;
  private MessagePanel messagePanel;
  private Vector downloads;
  private Vector monitor;
  private Vector messages;
}












