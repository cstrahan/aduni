/**
   ControlHandler.java
   part of footella
   @author JMR
   @version $Id: ControlHandler.java,v 1.13 2001/01/28 22:33:16 jeff Exp $

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
     Sets the DownloadPanel
     @param downloadPanel
  */
  public void setDownloadPanel(DownloadPanel d) { this.downloadPanel = d; }

  /**
     Gets the data representation of the DownloadTable
     @return a Vector made up of ConnectionObjects
  */
  public Vector getDownloadVector() { return downloads; }
  
  /**
     Gets the data representation of the HostTable
     @return a Vector made up of ConnectionObjects
  */
  public Vector getHostVector() { return connections; }

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
    results.add(q);
    queryPanel.poke();
  }

  // -------------------------------------------------------
  /**
     Reports the arrival of an object
  */
  public void report(Object object) {
    if(showMessages) 
      System.out.println("> " + object);
    poke();
  }
  
  /**
     Reports a message
  */
  public void report(String string) { 
    if(showMessages)
      System.out.println("> " + string); 
    poke();
  }

  /**
     Reports an error
  */
  public void reportErr(String string) {
    System.out.println("[===> " + string); 
  }

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
    
    poke();
    return true;
  }

  /**
       Poke is called when some data has been changed that
       the user interface should know about
    */
  public void poke() { 
    queryPanel.poke();
    downloadPanel.poke();
    hostPanel.poke();
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
    for(int i = 0 ; i < connections.size() ; i++) {
      ConnectionObject o =  (ConnectionObject)connections.get(i);
      if(o.isLive()  /* && o.shouldSend() */) {
        // report and deliver
        report(new String("Delivering to Socket " + o.getSocketId()));
        Gateway.deliver(object, o.getSocketId());
      }
    }
  }

  /**
     Initializes a user download. Ask the download handler to start a
     download thread.
     @param row The row which contains the file particulars
  */
  public void download(int row) {
    ResultTableObject object = (ResultTableObject)results.get(row);
    TransferObject transferObject = new TransferObject(object);
    downloadHandler.sendDownloadRequest(object, transferObject);
    downloads.add(transferObject);
    downloadPanel.poke();
  }

  public String getFileName(int fileIndex) {
    return index.lookupPath(new Integer(fileIndex));
  }

  public int getFileSize(int fileIndex) {
    return index.lookupFileSize(new Integer(fileIndex));
  }

  // Fields
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
  private Vector downloads;
}
