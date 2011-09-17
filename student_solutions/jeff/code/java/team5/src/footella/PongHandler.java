/**
   PongHandler.java
   part of footella
   @version $Id: PongHandler.java,v 1.4 2001/01/24 23:48:51 jeff Exp $
*/
public class PongHandler
{
  Gateway gateway;
  
  public PongHandler(Gateway gateway) 
  {
    this.gateway = gateway;
  }

  /**
     Receives a Pong object from the Gateway
     @param incomingPong The incoming Object
  */
  public static void receivePong(Pong incomingPong)
  {
    //    System.out.println("Received Pong!"); // testing
    //    System.out.println(incomingPong);

    // do something, probably report message to the control structure
    // or so on...
  }

  // Don't know WHAT we want to do with a pong when we get it...
}
