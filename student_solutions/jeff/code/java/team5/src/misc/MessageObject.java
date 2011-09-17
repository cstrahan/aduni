/**
   MessageObject.java
   The main object responsible for message encapsulation.
   This class holds the payload descriptor information as
   well as which socket created it.
   
   @author JMR
   @version $Id: MessageObject.java,v 1.2 2001/01/24 11:58:18 eklempne Exp $
*/
public class MessageObject
{
  /**
     Constructor
  */
  MessageObject(int payloadDescriptor, int ttl, int payloadLength)
  {
    byte[] temp = new byte[16];
    generateMessageId(temp);
    new MessageObject(temp, payloadDescriptor, 
                  ttl, hops, payloadLength);
  }

  MessageObject(byte[] messageId, int payloadDescriptor,
                int ttl, int hops, int payloadLength)
  {
    // initialize the messageId array
    this.messageId = new byte[16];
    id = ++objectId;
    System.arraycopy(messageId, 0, this.messageId, 0, 16);
    this.payloadDescriptor = payloadDescriptor;
    this.ttl = ttl;
    this.hops = hops;
    this.payloadLength = payloadLength;
  }
  
  // Methods
  /**
     This method generates an unique messageId(hopefully)
     @return The id, 16 bytes long
  */
  private static void generateMessageId(byte[] id) {
    // Implement this! 
    for(int i = 0; i < 16; i++) id[i] = (byte)i; // for now
  }

  public int getPayloadDescriptor() { return payloadDescriptor; }
  public int getTtl() { return ttl; }
  public int getHops() { return hops; }
  public int getSocketId() { return socketId; }
  public int getPayloadLength() { return payloadLength; }
  public byte[] getMessageId() {
    // return a copy, just in case
    byte[] temp = new byte[16];
    System.arraycopy(messageId, 0, temp, 0, 16);
    return messageId;
  }
  
  public void decTtl() {ttl++; }
  public void incHops() {hops++; }
  public void setSocketid(int n) {socketId = n; }
  public String toString() {
    String temp = "";
    for (int i = 0; i < 16; i++) 
      temp += messageId[i];

    return "Message Object id #" + id + "["+
      temp + "," +
      payloadDescriptor + "," + ttl + "," +
      hops + "," + payloadLength + "]";
  }

  // Fields
  int id;
  int socketId;
  byte[] messageId;
  int payloadDescriptor;
  int ttl;
  int hops;
  int payloadLength;


  // Let's give each object and id number, which could be
  // Useful later
  static int objectId = 0;

  // test method
  public static void main(String[] args) {
    MessageObject temp = new MessageObject(1, 5, 48283);
    System.out.println(temp.toString());

    MessageObject temp2 = new MessageObject(0, 19, 142);
    System.out.println(temp2.toString());
  }    
}

    


