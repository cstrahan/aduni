/**
   PingThread.java
   part of footella
   @author JMR
   @version $Id: PingThread.java,v 1.3 2001/01/31 16:10:49 jeff Exp $
   
   Periodically sends out pings.
*/

class PingThread extends Thread {
  PingThread(ControlHandler control) {
    this.control = control;
    control.report("Ping Handler initialized.");
  }

  public void run() {
    for(;;) {
      try {
        if((Utility.currentIncoming + Utility.currentOutgoing) > 0) {
          Ping ping = new Ping();
          control.deliver(ping);
        }
        sleep(30000);
      }
      catch(Exception e) {
        control.reportErr("Error sending Ping");
        this.interrupt();
      }
    }
  }
  private ControlHandler control;
}
