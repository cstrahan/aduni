/**
   DownloadThread.java
   part of footella
   @author JMR
   @version $Id: DownloadThread.java,v 1.1 2001/01/28 22:02:47 jeff Exp $

   A thread that controls the download of a file
*/
import java.io.*;
import java.net.*;

public class DownloadThread extends Thread {
  DownloadThread(ControlHandler control, ResultTableObject object)
  {
    this.control = control;
    this.object = object;
  }
  public void run() {
    String downloadDir = "/opt/home/jeff/download/";
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
      String connection[] = {"", "" , "", "", "", ""} ;
      for(int j = 0; j < 5; j++) {
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
        System.out.println("] " +connection[j]);
      }
      // Do the download
      FileOutputStream outFile = new FileOutputStream(downloadDir + fileName);
			DataInputStream inStream = new DataInputStream(socket.getInputStream());

      int BLOCK = 1024;
			boolean done = false;
      int remainingBytes = fileSize;
      byte[] buffer = new byte[BLOCK];
			while (!done)	{
        if(remainingBytes < BLOCK) {
          inStream.read(buffer, 0, remainingBytes);
          outFile.write(buffer, 0, remainingBytes);
          outFile.flush();
          done = true;
        }
        else {
          inStream.read(buffer, 0, BLOCK);
          outFile.write(buffer, 0, BLOCK);
          outFile.flush();
          remainingBytes -= BLOCK;
          sleep(10);
        }
      }

      inStream.close();         // close stream
      outFile.close();           // close file
      socket.close();         // close socket
      System.out.println("Success. File successfully transfered.");
    }
    catch(Exception e) {
      System.out.println("DOWNLOAD ERROR: " + e);
    }
  }
  private ControlHandler control;
  private ResultTableObject object;
}
