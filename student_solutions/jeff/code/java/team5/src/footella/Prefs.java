/**
   Prefs.java
   @author AJP
   @version $Id: Prefs.java,v 1.6 2001/01/31 20:12:02 tpryor Exp $

   Simple class for saving state between processes.
*/

import java.io.*;
import java.util.*;

class Preferences{

  public PrefsRecord prefrec;

  Preferences() {
    readPrefs();
  }
    
  public PrefsRecord getPrefsRecord() {
    return prefrec;
  }
  
  public boolean readPrefs() {
    try {
      File prefsFile = new File(".foot_prefs");
      if(prefsFile.canRead())
      {
	System.out.println("reading preferences file...");
        FileInputStream istream = new FileInputStream(".foot_prefs");
        ObjectInputStream p = new ObjectInputStream(istream);
        prefrec = (PrefsRecord) p.readObject();
        istream.close();
      }
      else {
        // create a file!
        System.out.println("creating preferences file...");
        Vector v = new Vector();
        v.add(new String("gnutellahosts.com"));
        savePrefs(0, 10, v , ".",
                  ".", 6346);
	readPrefs();
      }
    }
    catch(Exception e) {
      System.out.println("Couldn't read preferences.");
    }
    return true;
  }

    
  public boolean savePrefs(int minSpeed, int TTL, Vector hosts,
                           String downloadPath, String searchPath,
                           int portNum)
  {
    try
    {
      System.out.println("TRYING TO SAVE PREFS");
      FileOutputStream ostream = new FileOutputStream(".foot_prefs", false);
      ObjectOutputStream q = new ObjectOutputStream(ostream);
      q.writeObject(new PrefsRecord(minSpeed, TTL, hosts, downloadPath,
                                    searchPath, portNum));
      q.flush();
      ostream.close();
    }
    catch(IOException io)
    {
      System.out.println("Couldn't write to preference file.");
    }
    return true;
  }
}

class PrefsRecord implements Serializable{
 
  int minSpeed;
  int TTL;
  Vector hosts;
  String downloadPath;
  String searchPath;
  int portNum;
    
  private void writeObject(java.io.ObjectOutputStream out) throws IOException 
  {
    out.defaultWriteObject();
  }
    
  private void readObject(java.io.ObjectInputStream in) throws IOException, ClassNotFoundException
  {
    in.defaultReadObject();
	
  }

  public String getSearchPath(){return new String(searchPath);}
  public String getDownloadPath(){return new String(downloadPath);}
  public int getMinSpeed(){return minSpeed;}
  public int getTTL(){return TTL;}
  public int getPortNum(){return portNum;}
  public Vector getHosts(){return new Vector(hosts);}

  PrefsRecord(int ms, int ttl, Vector h, String dp, String sp, int pn)
  {
    this.minSpeed = ms;
    this.TTL = ttl;
    this.hosts = h;
    this.downloadPath = dp;
    this.searchPath = sp;
    this.portNum = pn;
  }
}






