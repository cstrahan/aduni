/**
   DownloadObject.java
   @author JMR
   @version $Id: DownloadObject.java,v 1.1 2001/01/31 17:26:25 jeff Exp $

   The 
*/

class DownloadObject
{
  DownloadObject(ResultTableObject object)
  {
    this.object = object;
    fileSize = object.getFileSize();
    fileName = object.getFileName();
  }

  public String[] getInfo() {
    String[] temp = new String[3];
    temp[0] = fileName;
    temp[1] = object.getHost();
    temp[2] = status;
    return temp;
  }

  public void kill() { STOP = true; }

  public void updateTransfer(int t) {
    double foo = ((t + .000001) / fileSize) * 100;
    status = t + " of " + fileSize + " (" + (short)foo + "% )";
  }

  public boolean isDone() { return done; }

  public void complete() {
    status = "Download Complete";
    done = true;
  }

  public void error() {
    status = "An Error Occurred";
    done = true;
  }
  
  protected  boolean STOP = false;
  private boolean done = false;
  private String fileName;
  private int fileSize;
  private String status;
  private int transfer;
  private ResultTableObject object;
}

    
