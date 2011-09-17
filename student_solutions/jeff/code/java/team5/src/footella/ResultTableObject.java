import java.util.*;

class ResultTableObject {
  ResultTableObject(ResultObject r, QueryHitObject q)
  {
    result = r;
    queryHitObject = q;
  }

  public String[] getInfo() {
    String temp[] = new String[3];
    temp[0] = getFileName();
    temp[1] = getSize();
    temp[2] = getHost();

    return temp;
  }

  public String getFileName() {
    return result.getName();
  }
  public String getSize() { 
    int temp = result.getFileSize();
    String foo = "" + temp;
    return foo;
  }
  
  public int getFileSize() {
    return result.getFileSize();
  }

  public int getFileIndex() {
    return result.getFileIndex();
  }
  public String getHostName() {
    byte[] foo = queryHitObject.getIp();
    String temp = "";
    temp = ((int)foo[0] &0xff) + "." + ((int)foo[1] &0xff) + "." +
      ((int)foo[2] &0xff) + "." + ((int)foo[3] &0xff);
    return temp;
  }

  public int getPort() {
    return queryHitObject.getPortNum();
  }

  public String getHost() {
    byte[] foo = queryHitObject.getIp();
    String temp = "";
    temp = ((int)foo[0] &0xff) + "." + ((int)foo[1] &0xff) + "." +
      ((int)foo[2] &0xff) + "." + ((int)foo[3] &0xff)+ ":" + 
      queryHitObject.getPortNum();
    return temp;
  }
    
  private ResultObject result;
  private QueryHitObject queryHitObject;
}

  
