/**
   DownloadThread.java
   part of footella
   @author JMR
   @version $Id: DownloadThread.java,v 1.11 2001/01/31 18:32:22 jeff Exp $

   A thread that controls the download of a file
*/
import java.io.*;
import java.net.*;

public class DownloadThread extends Thread {
  DownloadThread(ControlHandler control, ResultTableObject object, DownloadObject transfer)
  {
    this.control = control;
    this.object = object;
    this.transfer = transfer;
  }
  public void run() {
    System.out.println("DOWNLOAD THREAD STARTED");
    String downloadDir = Utility.downloadPath + "/"; // error checking eventually
    String fileName = object.getFileName();
    int fileIndex = object.getFileIndex();
    int fileSize = object.getFileSize();
    String host = object.getHostName();
    int port = object.getPort();

    try {
      Socket socket = new Socket(host, port);

      BufferedReader iS = 
        new BufferedReader(new InputStreamReader(socket.getInputStream()));
      BufferedWriter outStream = 
        new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));

      String message = "GET /get/" + fileIndex + "/" + fileName + 
        " HTTP/1.0\r\n" + "User-Agent: Footella\r\n\r\n";
        
      outStream.write(message, 0, message.length());
      outStream.flush();


      // Get the response. Right now, this is really not all that impressive,
      // in that I'm assuming it's the right thing.
      String connection[] = {"", "" , "", "", ""} ;
      for(int j = 0; j < connection.length; j++) {
        int i = 0;
        while(i < 1024) {
          char temp = (char)iS.read();
          if(temp == '\r') {
            connection[j] += temp;
            iS.read();
            i = 1025;
          }
          connection[j] += temp;
          i++;
        }
        connection[j].trim();
        control.report(new String("] " +connection[j]));
      }
      // Do the download
      FileOutputStream outFile = new FileOutputStream(downloadDir + fileName);
			BufferedInputStream inStream = new BufferedInputStream(socket.getInputStream());
      int remainingBytes = fileSize;
      int available;

      boolean done = false;
      byte[] buffer = new byte[100000];
      while(!done) {               // not done
        available = inStream.available();
        // System.out.println(available + " available. reading ...");
        inStream.read(buffer, 0, available);
        outFile.write(buffer, 0, available);
        outFile.flush();
        transfer.updateTransfer(fileSize - remainingBytes);
        control.poke(Utility.DOWNLOAD);
        remainingBytes -= available;
        // System.out.println(remainingBytes + " remaining");
        sleep(100);
        if(transfer.STOP) done = true;
        if(remainingBytes <= 0) { 
          done = true;
          transfer.complete();
          control.poke(Utility.DOWNLOAD);
        }
      }

      inStream.close();          // close stream
      outFile.close();           // close file
      socket.close();            // close socket

      // report exit status
      if(! transfer.STOP)
        control.report(new String("Success. File " + fileName + " downloaded."));
      else control.report(new String("Download of " + fileName + " aborted."));
    }
    catch(Exception e) {
      transfer.error();
      control.poke(Utility.DOWNLOAD);
      control.reportErr(new String("DOWNLOAD ERROR: " + e));
    }
  }
  private ControlHandler control;
  private ResultTableObject object;
  private DownloadObject transfer;
}
