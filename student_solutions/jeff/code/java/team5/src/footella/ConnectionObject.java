/**
   ConnectionObject.java
   part of footella
   @author JMR
   @version $Id: ConnectionObject.java,v 1.4 2001/01/31 16:10:48 jeff Exp $
*/
import java.io.*;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.table.*;
import java.util.*;

class ConnectionObject {
  ConnectionObject(SocketHandler handler, int type) {
    this.handler = handler;
    this.type = type;
  }

  public Object[] getInfo() {
    Object[] temp = new Object[] {
        new String(getAddress() + ":" + getPort()), getType(),
          new Integer(getSent()), 
          new Integer(getRec()), 
          getStatus()};
    return temp;
  }
  public int getSent() { return handler.getSent(); }
  public int getRec() {  return handler.getRec(); }
  public int getSocketId() { return handler.getSocketId(); }
  public void disconnect() { handler.disconnect(); }
  public SocketHandler getHandler() {return handler; }
  
  public boolean isLive() {
    return handler.live;
  }
  
  public int getPort() { return handler.getHostPort(); }
  public String getAddress() { return handler.getAddress(); }
  public String getStatus() { 
    if(handler.live)
      return "Connected";
    else
      return "Disconnected";
    }
  public String getType() {
    if(type == INCOMING) return "Incoming";
  else return "Outgoing";
  }

  public boolean shouldSend() {
    if(type == INCOMING) return false;
    else return true;
  }

  public int type;
  private SocketHandler handler;
  public static final int INCOMING = 0;
  public static final int OUTGOING = 1;
  public static final int CONNECTED = 0;
  public static final int DISCONNECTED = -1;
}

  
