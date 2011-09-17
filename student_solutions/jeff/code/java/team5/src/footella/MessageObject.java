/**
   MessageObject.java
   part of footella
   
   @author JMR
   @version $Id: MessageObject.java,v 1.8 2001/01/28 15:58:49 jeff Exp $


   The main object responsible for message encapsulation.
   This class holds the payload descriptor information as
   well as which socket created it.
*/
public class MessageObject
{
  /**
     Constructor
  */
  MessageObject(int payloadDescriptor, int ttl, int payloadLength) 
  {
    messageId = new byte[16];
    generateMessageId(messageId);
    this.payloadDescriptor = payloadDescriptor;
    this.ttl = ttl;
    this.hops = 0;
    this.payloadLength = payloadLength;
  }


  /**
     A full constructor
  */
  MessageObject(byte[] tempBuffer, int payloadDescriptor,
                int ttl, int hops, int payloadLength)
  {
    // initialize the messageId array
    this.messageId = new byte[16];
    id = ++objectId;
    System.arraycopy(tempBuffer, 0, this.messageId, 0, 16);
    this.payloadDescriptor = payloadDescriptor;
    this.ttl = ttl;
    this.hops = hops;
    this.payloadLength = payloadLength;
  }
  
  /**
     This method generates an unique messageId(hopefully)
     @param  buffer Will write to the first 16 bytes of the buffer
  */
  public static void generateMessageId(byte[] tempBuffer) {
    // Implement this! 
    for(int i = 0; i < 16; i++)
      tempBuffer[i] = 0;
    double foo = Math.random();
    counter = (int)Math.floor(foo * 100);
    String temp = "g!=" + Utility.getAddress() + counter++;
    byte[] tempByte = temp.getBytes();
    if(tempByte.length < 16)
      System.arraycopy(tempByte, 0, tempBuffer, 0, tempByte.length);
    else
      System.arraycopy(tempByte, 0, tempBuffer, 0, 16);
  }

  public int getPayloadDescriptor() { return payloadDescriptor; }
  public int getTtl() { return ttl; }
  public int getHops() { return hops; }
  public int getSocketId() { return socketId; }
  public int getPayloadLength() { return payloadLength; }
  public void decTtl() {ttl++; }
  public void incHops() {hops++; }
  public void setSocketId(int n) {socketId = n; }
  public byte[] getMessageId() {
    // return a copy, just in case
    byte[] temp = new byte[16];
    System.arraycopy(messageId, 0, temp, 0, 16);
    return messageId;
  }
  
  public String toString() {
    String temp = "";
    for (int i = 0; i < 16; i++) 
      temp += (char)messageId[i];

    // the message id cannot be safely printed
    return "[<id registered> | " + 
      payloadDescriptor + " | " + ttl + " | " +
      hops + " | " + payloadLength + "]";
  }

  // Fields
  int id;
  int socketId;
  byte[] messageId;
  int payloadDescriptor;
  int ttl;
  int hops;
  int payloadLength;
  private static int counter = 0;
  
  // Let's give each object and id number...
  static int objectId = 0;
}

    
