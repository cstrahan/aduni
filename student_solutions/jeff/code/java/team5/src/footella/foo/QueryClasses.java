//methods and data structures for handling queries

//query hit is just a zap back telling where and how to get the file

//it will only be sent by a machine meeting the minimum speed requirement
//the number of hits in our case is the number of files whose name
//matches the search string

//ex:: querystring "super"


//supergrover, superman, supersonic, supercollider, superduper, 
//superfluous_grooves, superhuman, superglue, supersavior,
//superdog, etc...

//and all on this machine, coming back at ya in the result set
//which is part of the beautiful QueryHit object which is of theoretically
//unlimited size-- we'll have to limit it to some max # of results- 200...??
//play with different levels-- users should be able to define searches
//well enough to work within this contraint.


//fullname:
//there exists a hashtable with each filename mapped to a key

//search:
//there also exists a hashtable with keys based on
//each filename cut into
//smaller strings down to the four letter level and
//mapped back to their full string pathname.


//worry about better search implementation later - we should be grabbing 
//file header info as well
//for our search criteria- filename is rather useless really.

//"top40":
//each request will look through for an exact string match of a top40
//list first?

//the problem here is that it may be rare to find someone who is not wildcarding
//out of convenience...

//not necessarily... someone knows more... there must be a discussion board
//or mailing list for people implementing servents... try the gnutella site...

//ok, interestingly a query hit has to get routed back through a chain
//and somewhere in memory we need to match it's messageID (corresponding to
//the query's messageID i believe) to a socket to send it back down--
//if the socket connection is no more, just drop the queryHit in the snow...

//package it up according to the 
//protocol and give it to the gatekeeper in a QueryHit messageObject
//and save it's guid.origin==socket.destination in a field


//query hits should be...

import MessageObject.*;
import java.util.*;
import java.util.HashMap.*;
import java.io.*;


//noticed that MessageObject sets hops to zero? ee?
//need to be able to initialize all data fields in constructor,
//especially messageID... 
//                            okay, you got it! Had to shuffle 
//                            this constructor a bit though -J


class QueryObject extends MessageObject
{
  short minSpeed;
  byte[] searchbytes;
  String searchString;
  
  QueryObject(byte[] messageId, int payloadDescriptor, 
              int ttl,    int hops, 
              int payloadLength, 
              short minSpeed,
              String searchString)
  {
    super(messageId, payloadDescriptor, ttl, hops, payloadLength);
    this.minSpeed = minSpeed;
    this.searchString = searchString.trim();
    // Why 3? I wrote it, just not sure yet why it works
    this.searchbytes = new byte[this.searchString.length() + 3];
    for(int i = 0; i < this.searchString.length(); i++)
	    this.searchbytes[i] = (byte) this.searchString.charAt(i);
    this.searchbytes[this.searchString.length() + 1] = (byte) 0;
    this.searchbytes[this.searchString.length() + 2] = (byte) 0;
  }

  QueryObject(int payloadDescriptor, int ttl, short minSpeed, String searchString)
  {
    super(payloadDescriptor, ttl, (searchString.length() + 2));
    this.minSpeed = minSpeed;
    this.searchString = searchString;
    // Why 3? I wrote it, just not sure yet why it works
    this.searchbytes = new byte[searchString.length() + 3];
    for(int i = 0; i < searchString.length(); i++)
	    this.searchbytes[i] = (byte) searchString.charAt(i);
    this.searchbytes[searchString.length() + 1] = (byte) 0;
    this.searchbytes[searchString.length() + 2] = (byte) 0;
  }

  /**
     Ye olde constructor
  */
  QueryObject(int payloadDescriptor, int ttl, int payloadLength, short minSpeed,
              String searchString)
  {
    super(payloadDescriptor, ttl, payloadLength);
    this.minSpeed = minSpeed;
    this.searchString = searchString;
    // Why 3? I wrote it, just not sure yet why it works
    this.searchbytes = new byte[searchString.length() + 3];
    for(int i = 0; i < searchString.length(); i++)
	    this.searchbytes[i] = (byte) searchString.charAt(i);
    this.searchbytes[searchString.length() + 1] = (byte) 0;
    this.searchbytes[searchString.length() + 2] = (byte) 0;
  }

  public String getSearchString() {
    return searchString;
  }
  
  public short getMinSpeed() {
    return minSpeed;
  }
  
  public String toString() {
    return "QUERY [speed " +minSpeed + " | " + searchString + "]";
  }
}

class QueryHitObject extends MessageObject
{
  int numHits;
  short portNum;
  int ipAddress;
  byte[] ip;
  int speed;
  Vector resultSet; //list of ResultObjects
  byte[] serventID;

  QueryHitObject(byte[] messageId, int payloadDescriptor, int ttl, int hops,
                 int payloadLength, int numHits,short portNum, 
                 byte[] ipAddress, int speed, Vector resultSet,
                 byte[] serventID)
  {
    super(messageId, payloadDescriptor, ttl, hops, payloadLength);
    this.numHits = numHits;
    this.portNum= portNum;
    this.ip = ipAddress;
    this.speed = speed;
    this.resultSet = resultSet;
    this.serventID= serventID;
  }

  QueryHitObject(int payloadDescriptor, int ttl, int payloadLength, int numHits,
                 short portNum, int ipAddress, int speed, Vector resultSet,
                 byte[] serventID)
  {
    super(payloadDescriptor, ttl, payloadLength);
    this.numHits = numHits;
    this.portNum= portNum;
    this.ipAddress= ipAddress;
    this.speed = speed;
    this.resultSet = resultSet;
    this.serventID= serventID;
  }

    public int getNumHits() { return numHits;}
    public short getPortNum() { return portNum;}
    public int getIpAddress() { return ipAddress;}
    public byte[] getIp() { return ip; }
    public int getSpeed() {return speed;}
    public Vector getResultSet() {return resultSet;}

    //MessageObject is wacked - it accidentally returns messageID rather than temp;
    
    public byte[] getServentID() {
	byte[] temp = new byte[16];
	System.arraycopy(serventID, 0, temp, 0, 16);
	return temp;}

  public String toString()
  {
    return "[QUERY HIT]";
  }
  
}


class ResultObject {
  
  ResultObject(int fIndex, int fSize, String fName)
  {
    this.fileIndex = fIndex;
    this.fileSize = fSize;
    this.name = fName;
    
    this.fileName = new byte[fName.length() + 3];
    for(int i = 0; i < fName.length(); i++)
	    this.fileName[i] = (byte) fName.charAt(i);
    this.fileName[fName.length() + 1] = (byte) 0;
    this.fileName[fName.length() + 2] = (byte) 0;
    this.nameByteLength = fileName.length;
  }

  int fileIndex;
  int fileSize;
  byte[] fileName;
  int nameByteLength;
  String name;

  public void printFileName() 
    {
	System.out.println(new String(fileName));
    }
  public int getFileIndex(){return fileIndex;}
  public int getFileSize(){return fileSize;}
  public String getName() {  return name; }
  
    public int getNameByteLength(){return nameByteLength;}
  public byte[] getFileNameBytes() 
  {
    byte[] temp = new byte[fileName.length];
    for(int i = 0; i < fileName.length; i++) temp[i] = fileName[i]; 
    return  temp;
  }   
}


class IndexWrapper{

    
    String dirPath;
    Wackadex wax;

    IndexWrapper(String directoryPath)
    {
	this.dirPath = directoryPath;
	this.wax = new Wackadex(dirPath);
	wax.printFileList();
    }

    public String getDirPath() {return new String(dirPath);}
    
    public boolean setDirPath(String newDirPath) 
    {
	//check to see that newDirPath is a valid directory
	//if not, verify that it possible to create it
	//if so, ask for confirmation to create (with dialog to change)
	//if not, ask for new path (dialog change without confirmation option)
	//if DirPath previously existed, call buildFileIndex()
	//if DirPath was the same, don't call buildFileIndex()!
	return false;
    }
    //************************************************************************
    //* IndexWrapper stuff-    QueryDriver is allowed to use these methods.  *
    //************************************************************************

    public int getNumFilesAvailable(){return (wax.getNumFiles());}

    public int getKBAvailable(){return (wax.getTotalKB());}

    //jeff
    public String lookupPath(Integer fin)
    {return (wax.lookupPath(fin));}

    public int lookupFileSize(Integer fin)
    {return (wax.lookupFileSize(fin));}

    public String lookupFileName(Integer fin)
    {return (wax.lookupFileName(fin));}

    public Vector searchIndex(String searchString)
    {
	Vector results = new Vector(); //ArrayList of ResultObjects
	Vector transmogrifier = new Vector();
	FileIndexObject tempFIO;
	transmogrifier = wax.searchMe(searchString);
	
	for(int x = 0; x < transmogrifier.size(); x++)
	    {
		//trans contains file index numbers (Integers) use basicFileIndex.get( )
            tempFIO =  ((FileIndexObject) 
			      wax.basicFileIndex.get(transmogrifier.get(x)));
	
	    results.add(new ResultObject(
		 tempFIO.getFileIndex(),
		 (new Long(tempFIO.getFileSize())).intValue(),
		 tempFIO.getFileName()));
             
	    }
	
	
	return results;
    }
    //thus we can use a binary search to find a given name in each [subvector]
    //every time a file is added, add it to these lists.
    //need a data structure that is a substring (should be a pointer to a substring but whatever) 
    //and a pointer to a full file access string "dirpath/filename"
}


class FileIndexObject implements Comparable{
    static int uniqueFileIDs = 0;
    int fileIndex;
    String fullName;
    String fileName;
    long fileSize;

    FileIndexObject(String filename, String fullname, long fileSize)
    {
	this.fileIndex = uniqueFileIDs;
	this.fullName = new String(fullname);
	this.fileName = new String(filename);
	this.fileSize = fileSize;
	uniqueFileIDs++;
    }
    
    public String getFullName() {return new String(fullName);}
    public String getFileName() {return new String(fileName);}
    public boolean compareStringAt(int index, String searchstring)
    {
	return(fileName.regionMatches(true, index, 
	  searchstring, 0, searchstring.length()));
    }
    public long getFileSize()
    {
	return fileSize;
    }
    public int compareTo(Object foobar)
    {
       return (this.fileName.compareTo(
		  ((FileIndexObject) foobar).getFileName()));
    }
    public int getFileIndex() {return fileIndex;}
}
        

class Wackadex{

    public Hashtable basicFileIndex; //one-to-one hash of FIObj's
    Vector byteMe; 
    int numFiles;
    int totalKB;

    //subvectors need only contain integers
    //or three byte mutant subints... regardless, their
    //order will tell all, and i can set their order with
    //with temporary lists

    //i'm going to change this later
    //so that they have a hash code and a fileindexnum
    //each will be twice the size but much faster
    //as there will be almost no string comparison (yay)

    public int getNumFiles() {return numFiles;}

    public int getTotalKB() {return totalKB;}
    //jeff
    public String lookupPath(Integer fin)
    {
	return (((FileIndexObject) 
		 basicFileIndex.get(fin)).getFullName());
    }

    public int lookupFileSize(Integer fin)
    {
	 return ((int) ((FileIndexObject) 
		 basicFileIndex.get(fin)).getFileSize());
    }

    public String lookupFileName(Integer fin)
    {
	 return (((FileIndexObject) 
		 basicFileIndex.get(fin)).getFileName());
    }

    public void printFileList()
    {
	for(int w = 0; w < 1; w++)	
	    for(int x = 0; x < ((Vector) byteMe.get(w)).size(); x++)
		System.out.println( (String) ((FileIndexObject) 
	           basicFileIndex.get(
	              ((Vector) byteMe.get(w)).get(x))).getFileName());
    }

    public Vector makeFileList(File dirtyfile)
    {
	File[] promQueen = dirtyfile.listFiles();
	Vector creepingStrings = new Vector();
	//How To Recurse Without Flossing or Brushing
	for(int i = 0; i < promQueen.length; i++)
	    {
	      if(promQueen[i].isFile() == true)
		 {
		     creepingStrings.add(promQueen[i].getAbsolutePath()
					 + "|||||"  + promQueen[i].length());
		     numFiles++;
		     totalKB = totalKB + ((int) promQueen[i].length() / 1000) + 1;
		  }
		else if(promQueen[i].isDirectory())
		    {
			creepingStrings.addAll(creepingStrings.size(), 
					       makeFileList(promQueen[i]));
		    }
	    }
	return creepingStrings;
    }

    Wackadex(String dirpath)
    {
	totalKB = 0;
	numFiles = 0;
	File topdir = new File(dirpath);
      	Vector pathStrings = makeFileList(topdir);
	Vector funkyFileStrings = new Vector();
	String[] tempFileStrings = new String[pathStrings.size()];
	
	String lonely_and_abused = new String();
	
	for(int i = 0; i < pathStrings.size(); i++)
	    {
		lonely_and_abused = (String) pathStrings.get(i);
		String theWholePath = new String();
		
		StringTokenizer alfred = 
		    new StringTokenizer(lonely_and_abused, "|||||");
		theWholePath = alfred.nextToken();
	       
	     
		int filelength = Integer.parseInt(alfred.nextToken(), 10);
		
		String trashMe = new String();
		
		StringTokenizer binky = 
		    new StringTokenizer(theWholePath, "/");
		
		while(binky.hasMoreTokens())
		    trashMe = binky.nextToken();
		
		lonely_and_abused = trashMe.toLowerCase() + "|||||" + theWholePath
		    + "|||||" + filelength;

		tempFileStrings[i] = new String(lonely_and_abused);
	    }
	
	
	//tempFileStrings[ = (String[]) funkyFileStrings.toArray();
	Arrays.sort(tempFileStrings);
	Vector orderedFileNames = new Vector();
	
	basicFileIndex = new Hashtable( (int)
			(tempFileStrings.length * 1.25 + 500)); 
    //construct and add fileindexobjects,
    //using fileIndex number as key
	for(int x = 0; x < tempFileStrings.length; x++)
	    {
		StringTokenizer smurfchunk = new StringTokenizer(
			   new String(tempFileStrings[x]), new String("|||||"));  

		String fileOnly = smurfchunk.nextToken();
	        String fullPath = smurfchunk.nextToken();
	int fsize = Integer.parseInt(smurfchunk.nextToken(), 10);
		orderedFileNames.add(fileOnly);
                
		basicFileIndex.put((Object) new Integer(x), (Object) new FileIndexObject(
			  fileOnly, fullPath, (long) fsize));
	    }
      
	byteMe = new Vector(); //list of Vectors
	for(int v = 0; v < 50; v++)
	    byteMe.add(new Vector());

	//each filled with ints
	//with different orderings of fileIndex number
	//ordering based on forshortened substring

	for(int u = 0; u < 50; u++)
	    {
		Vector colvect = (Vector) byteMe.get(u);
		
		String[] FIOarray = new String[orderedFileNames.size()];

		for(int q = 0; q < orderedFileNames.size(); q++)
		    FIOarray[q] = (String) orderedFileNames.get(q); 
		
		Vector FIOvect = new Vector();

		for(int h = 0; h < FIOarray.length; h++)
		    if(FIOarray[h].length() > u)
			FIOvect.add(new String(FIOarray[h].substring(u) + "|||||" + h));
				
		String[] FIOarray2 = new String[FIOvect.size()];

		for(int v = 0; v < FIOvect.size(); v++)
		    {
		    FIOarray2[v] = (String) FIOvect.get(v); 

		    }
		
		Arrays.sort(FIOarray2);
		
		for(int m = 0; m < FIOvect.size(); m++)
		    {
			StringTokenizer bongo = new StringTokenizer(FIOarray2[m], "|||||");
			bongo.nextToken();
			colvect.add( new Integer(
			    Integer.parseInt( bongo.nextToken() ) ) );
		    }
	    }

    }
    
    //call search me, get set of ints, if it's a one word search
    //it's party time, make an object and send her off

    //if multiple words are being used we can call this again
    //then use a couple Vectors to easily find the overlap without 
    //overdoing the whole coding thing

    //but how do i find all the matches given there can be more than one?
    //if we find a hit we may have to dredge the list for more (ugly).
    //to prevent this I will need a vector of objects that contain a
    //a local order key value

    //or better just create a class that contains the parameters so
    //you don't pass them around so much and waste time/ stack space
    //and cycles...


    //must use the hash encoding Alan suggested... much faster!
    // 0 - A/a
    // 25 - Z/z
    // 26 - 0
    // 27 - 1
    // 28 - 2
    // 29 - 3,4
    // 30 - 56789
    // 31 - ...

    //if a hash code is the same, 

    class BinarySpotlight
    {  
	
	Vector boojum;
	String sWord;
	int vectNum;
	int sWordlength;

	BinarySpotlight(String searchWord)
	{
	    this.sWord = searchWord;
	    this.sWordlength = sWord.length();
	}

	public void newVector(Vector sinister, int vnum)
	{
	    this.boojum = sinister;
	    this.vectNum = vnum;
	}

	//returns index of matching value, or -666
	public int binarySearch(int base, int top)
	{
	    //problem, 2, 1, 0... narrows to 2, 1...
	    //goes high, and stays 2, 1 forever repeated
	    //need to fix this somehow, need to go north when we get down to these small areas
	    //...

	    int x = ((int) ((top + base) / 2));
	    int substrlen =  ((String) ((FileIndexObject) 
   basicFileIndex.get(boojum.get(x))).getFileName()).length() - vectNum;
	   
        if(sWordlength <= substrlen)
	    {
	    int direction = ((String) ((String) ((FileIndexObject) 
   basicFileIndex.get(boojum.get(x))).getFileName()).substring(vectNum, 
	  	 vectNum + sWordlength )).compareTo(sWord);
      	    if( (top - base) > 0)  //get rid of this?
		{
		if (direction > 0)  return (binarySearch(base, x ));
		else if (direction == 0)
		    {
			return x;
		    }
		else 
		    {
			return (binarySearch(x + ((base + top)%2) , top ));
		    }
		}
	    else 
		{
		    if (direction == 0) return x;
		    else 
			{
			    return -666;
			}
		}
	    }
	else //if substr is shorter than searchString, they cannot match 
	    {
		if (base == top) return -666;
		else if( ((String) ((String) ((FileIndexObject) 
   basicFileIndex.get(boojum.get(x))).getFileName()).substring(vectNum, 
			   vectNum + substrlen)).compareTo(sWord)  > 0 )
	return (binarySearch(base, x));
	        else
		return (binarySearch(x + ((base + top)%2), top));
	    }
	}

	//once we've spotted a valid hit,
	//pan out spreads up or down until a non-match or limit
	//is found in this direction..
  	public Vector panOut(int startingLocation)
	{
	    Vector tempIntVect = new Vector();
	    int y = startingLocation;
	    int loc;
	    int x = 0;
	    int comparison;
             if (sWordlength <= ((String) ((FileIndexObject) 
        basicFileIndex.get(boojum.get(y))).getFileName()).length() - vectNum )
		 {
	            comparison = ((String) ((String) ((FileIndexObject) 
                     basicFileIndex.get(boojum.get(y))).getFileName()).substring(vectNum, 
			vectNum + sWord.length())).compareTo(sWord);
	           while((y < boojum.size()) && (comparison == 0)
			 && (sWordlength <= ((String) ((FileIndexObject) 
   basicFileIndex.get(boojum.get(y))).getFileName()).length() - vectNum ) )
		   {
		    tempIntVect.add( new Integer((int) ((Integer) boojum.get(y)).intValue() ) );
		    x++;
		    y++;
		 if ((y < boojum.size()) && (sWordlength <= ((String) ((FileIndexObject) 
		    basicFileIndex.get(boojum.get(y))).getFileName()).length() - vectNum ) )
		     comparison = ((String) ((String) ((FileIndexObject) 
                     basicFileIndex.get(boojum.get(y))).getFileName()).substring(vectNum, 
			vectNum + sWord.length())).compareTo(sWord);
		    else comparison = -666;
		    }
                 }
	    y = startingLocation - 1;
            if ((y >= 0) && sWordlength <= ((String) ((FileIndexObject) 
        basicFileIndex.get(boojum.get(y))).getFileName()).length() - vectNum )
		{
		    comparison = ((String) ((String) ((FileIndexObject) 
		      basicFileIndex.get(boojum.get(y))).getFileName()).substring(vectNum, 
			   vectNum + sWord.length())).compareTo(sWord);
	    
	         while((y >= 0 ) && 
		 (comparison == 0) && (sWordlength <= ((String) ((FileIndexObject) 
   basicFileIndex.get(boojum.get(y))).getFileName()).length() - vectNum ) )
		     {
		     tempIntVect.add( new Integer((int) ((Integer) boojum.get(y)).intValue() ) );
		     x++;
		     y--;
		     if (sWordlength <= ((String) ((FileIndexObject) 
   basicFileIndex.get(boojum.get(y))).getFileName()).length() - vectNum )
		     comparison = ((String) ((String) ((FileIndexObject) 
   basicFileIndex.get(boojum.get(y))).getFileName()).substring(vectNum, 
			   vectNum + sWord.length())).compareTo(sWord);
		     else comparison = -666;
	     	     }
		}
	       return tempIntVect;
	}
    }
  
    public Vector searchMe(String searchWord)
    {
	//binary search your way through byteMe's columns
	//(50 - searchString length) times
	Vector ultraSmurfy = new Vector();
	BinarySpotlight argonaut = new BinarySpotlight(searchWord); 
	int binaryspotnum;
	Vector littleOldVector;

	for(int s = 0; s < (50 - searchWord.length()); s++)
	    {
		littleOldVector = (Vector) byteMe.get(s);
		argonaut.newVector(littleOldVector, s);
		if(!littleOldVector.isEmpty())
		    {
		binaryspotnum = argonaut.binarySearch( 0, littleOldVector.size() - 1 );
		if(binaryspotnum != -666)
		    ultraSmurfy.addAll( argonaut.panOut(binaryspotnum) );
		    }
	    }
	
	return ultraSmurfy;
    }

}

//**Gary look at this **
//**test code block*********************************************************
class QueryClasses{

public static void main(String[] args)
    {
	IndexWrapper testIndex = new IndexWrapper(new String("."));
	Vector searchResults1 = new Vector();
	String searchStr = new String("class");
	searchResults1 = testIndex.searchIndex(searchStr);
	System.out.println("search was: '" + searchStr + "'.");
	System.out.println("results are...");
	for(int x = 0; x < searchResults1.size(); x++)
	    ((ResultObject) searchResults1.get(x)).printFileName();
    }

}
//**test code block end****************************************************

class QueryDriver{

  
    short myMinSpeed;
    byte myTTL;
    String fileRootPath;
    //must either add a "SocketID" field to QueryHitObject to MessageObject...
    //or will the gateway maintain this list-- too many lists, we should handle them seperately
    //depending on the task
    
    ArrayList query_hit_or_miss; 
    
    //a list of sockets and messageID's
    //^^^^^^^^^^Erica is doing this...
    //the messageID match a returned query hit to a given socketID
    //so we know where to stuff it (if the socket still exists)
    //this could be a hashtable or whatever- it's just an ordered lookup list
    //to handle a query hit that will be passed on (such as one that appears in this list)
    //decrement ttl, increment hops, passback object to gateway (with socketID)
    

    QueryDriver(short myMinSpeed, byte myTTL, String fileRootPath)
    {
	this.myMinSpeed = myMinSpeed;
	this.myTTL = myTTL;
	this.fileRootPath = fileRootPath;
  
    }

    public boolean beginQuery(String search)
    {
	QueryObject queryObj = new QueryObject( (byte) 0x80 , myTTL, 
						search.length() + 4, //payload length 
					       myMinSpeed, search);
	return true;
    }

    public MessageObject handleQuery(MessageObject queep)
    {
	MessageObject messObj = queep;
	QueryHitObject queryHitObj;
	QueryObject queryObj;
	//do stuff
	return messObj;
    }



}









































