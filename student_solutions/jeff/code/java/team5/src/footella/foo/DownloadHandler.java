/**
   DownloadHandler.java
   part of footella

   @author JMR
   @version $Id: DownloadHandler.java,v 1.2 2001/01/28 19:28:19 jeff Exp $

   This handler takes care of file transfers, working
   closely with the socket handlers
*/
import java.net.*;
import java.io.*;

public class DownloadHandler {
  /**
     Constructor
  */
  DownloadHandler(ControlHandler control) {
    this.control = control;
  }
  
  /**
     Receives a GET request, and starts the transfer if possbile
     @return <code>true</code> on success
  */
  public boolean receiveDownloadRequest(Socket socket, BufferedReader iS,
                                        Writer oS) {
    try {
      System.out.println("Download Handler Received a GET");
      // at this point, all we've gotten is the "GET"


      // get the first line
      String connection = "GET";
      int i = 0;
      while(i < 1024) {
        char temp = (char)iS.read();
        if(temp == '\r') {
          connection += temp;
          iS.read();
          i = 1025;
        }
        connection += temp;
        i++;
      }
      connection.trim();
      System.out.println("] " +connection);

      String connection2 = "";
      i = 0;
      while(i < 1024) {
        char temp = (char)iS.read();
        if(temp == '\r') {
          connection2 += temp;
          iS.read();
          i = 1025;
        }
        connection2 += temp;
        i++;
      }
      connection2.trim();
      System.out.println("] " + connection2);

      String connection3 = "";
      i = 0;
      while(i < 1024) {
        char temp = (char)iS.read();
        if(temp == '\r' || temp == '\n') {
          connection3 += temp;
          iS.read();
          i = 1025;
        }
        connection3 += temp;
        i++;
      }
      connection3.trim();
      System.out.println("] " + connection3);

      // Some agents don't have the fourth line!
//        String connection4 = "";
//        i = 0;
//        while(i < 1024) {
//          char temp = (char)iS.read();
//          if(temp == '\r' || temp == '\n') {
//            connection4 += temp;
//            iS.read();
//            i = 1025;
//          }
//          connection4 += temp;
//          i++;
//        }
//        connection4.trim();
//        System.out.println("] " + connection4);
        
      // we have a winner!
      System.out.println("End of request");
      // grab the file index number
      String index = "";
      for(i = 9; i < connection.length(); i++) {
        char foo = connection.charAt(i);
        if(foo == '/') break;
        index += foo;
      }
      int fileNumber = Integer.parseInt(index); // error checking?

      System.out.println("Index should be: " + fileNumber);

      // get the file path from wackadex
      String fileName = control.getFileName(fileNumber);
      int fileSize = control.getFileSize(fileNumber);

      // send the response
      String message = "HTTP 200 OK\r\nServer: Gnutella\r\n" + 
        "Content-type:application/binary\r\n" +
        "Content-length: " +  fileSize  + "\r\n\r\n";
      oS.write(message, 0, message.length());
      oS.flush();

      // initiate download....
			DataOutputStream outStream = new DataOutputStream(socket.getOutputStream());
      FileInputStream inFile = new FileInputStream(fileName);
      System.out.println("File opened.");

			boolean done = false;
      int remainingBytes = fileSize;
      byte[] buffer = new byte[1024];
			while (!done)	{
        if(remainingBytes < 1024) {
          inFile.read(buffer, 0, remainingBytes);
          outStream.write(buffer, 0, remainingBytes);
          outStream.flush();
          done = true;
        }
        else {
          inFile.read(buffer, 0, 1024);
          outStream.write(buffer, 0, 1024);
          remainingBytes -= 1024;
        }
      }
      inFile.close();           // close file
      socket.close();         // close socket
      System.out.println("Success. File successfully transfered.");
      return true;
    }  
    catch(Exception e) {
      System.out.println("UPLOAD ERROR: " + e);
      return false;
    }
  }
  /**
     Intitates a GET request
     @return <code>true</code> on success
  */
  public boolean sendDownloadRequest(ResultTableObject object) {
    DownloadThread download = new DownloadThread(control, object);
    download.start();
    return true;
  }
  private ControlHandler control;
}
