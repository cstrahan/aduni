
/**
   QueryClasses.java
   @author AJP
   @version $Id: QueryClasses.java,v 1.21 2001/01/31 20:11:29 tpryor Exp $

   Query/QueryHit related classes and search index.
*/



//methods and data structures for handling queries

//which is part of the beautiful QueryHit object which is of theoretically
//unlimited size-- we'll have to limit it to some max # of results- 200...??

//To do:

//<1>need to handle redundant records (not a big problem)
//<2>need to handle multiword searches (two searches, call 
//firstWordResultVector.retainAll(secondWordResultVector))

//<3>Next step: later there will exist a second hashtable (one long hashcoded hashtable,
//rather than a vector of vectors) with keys based on
//each filename cut recurrently forshortened by one to
//smaller strings down to the six letter level and
//mapped back to their full index number
//IOW, goodbye Wackadex, hello double hash index (faster)
    //hash encoding 
    // 0 - A/a
    // 25 - Z/z
    // 26 - 0
    // 27 - 1
    // 28 - 2
    // 29 - 3,4
    // 30 - 56789
    // 31 - ...

import MessageObject.*;
import java.util.*;
import java.util.HashMap.*;
import java.io.*;

//****** QueryObject BEGIN *********************************************

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

  public String getSearchString() 
    {
	return searchString;
    }
  
  public short getMinSpeed() 
    {
	return minSpeed;
    }
  
  public String toString() 
    {
    return "QUERY [speed " +minSpeed + " | " + searchString + "]";
    }
}

//***** QueryObject END ***********************************************


//***** QueryHitObject BEGIN ******************************************
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

    public int getNumHits() 
    { 
	return numHits;
    }
    
    public short getPortNum() 
    { 
	return portNum;
    }
    
    public int getIpAddress() 
    { 
	return ipAddress;
    }
    
    public byte[] getIp() 
    { 
	return ip; 
    }
    
    public int getSpeed() 
    {
	return speed;
    }

    public Vector getResultSet() 
    {
	return resultSet;
    }

    public byte[] getServentID() 
    {
	byte[] temp = new byte[16];
	System.arraycopy(serventID, 0, temp, 0, 16);
	return temp;
    }

  public String toString()
  {
    return "[QUERY HIT]";
  }
  
}
//***** QueryHitObject END ********************************************


//***** ResultObject BEGIN ********************************************
//ResultObjects contain individual query matches
//they contain the information  specified in the gnutella protocol
//as returned by a query hit object

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
  
    public int getFileIndex()
    {
	return fileIndex;
    }
  
    public int getFileSize()
    {
	return fileSize;
    }
  
    public String getName() 
    {  
	return name; 
    }
  
    public int getNameByteLength()
    {
	return nameByteLength;
    }
  
    public byte[] getFileNameBytes() 
    {
	byte[] temp = new byte[fileName.length];
	for(int i = 0; i < fileName.length; i++) temp[i] = fileName[i]; 
	return  temp;
    }   
}
//***** ResultObject END **********************************************



//***** IndexWrapper BEGIN*********************************************
//IndexWrapper is what QueryHandler (or Gateway, or the Controller, if these are not divided only by class
//rather than seperate threads) uses to ask things of the file index. If multiple threads need to talk
//to the file index, synchronization needs to be added.

class IndexWrapper{
    
    String dirPath; //currently the only way to rebuild the file 
    //index is to instantiate a new IndexWrapper which makes a new Wackadex
    //all this is done in Wackadex's contructor - is there any reason not to just  
    //re-call buildFileIndex(...)?
    Wackadex wax;

    IndexWrapper(String directoryPath)
  {
    this.dirPath = directoryPath;
    this.wax = new Wackadex(dirPath);
    // wax.printFileList();  //make sure it worked
    Utility.setNumFiles(getNumFilesAvailable());
    Utility.setSharedSize(getKBAvailable());
    }

    public boolean setDirPath(String newDirPath) 
    {
	//not implemented - eventually this will be exposed in indexwrapper
	//as of now, it does nothing
	//<1> check to see that newDirPath is a valid directory
	//if not, verify that it possible to create it
	//if so, ask for confirmation to create (with dialog to change)
	//if not, ask for new path (dialog change without confirmation option)
	//<2> if DirPath previously existed, call buildFileIndex()
	//if DirPath was the same, don't call buildFileIndex()
	return false; //nada surf
    }

    //************************************************************************
    //*   IndexWrapper stuff- QueryHandler can use the following methods.    *
    //************************************************************************

    public String getDirPath() 
    {
	return new String(dirPath);
    }

    public int getNumFilesAvailable()
    {
	return (wax.getNumFiles());
    }

    public int getKBAvailable()
    {
	return (wax.getTotalKB());
    }

    public String lookupPath(Integer fin)
    {
	return (wax.lookupPath(fin));
    }

    public int lookupFileSize(Integer fin)
    {
	return (wax.lookupFileSize(fin));
    }

    public String lookupFileName(Integer fin)
    {
	return (wax.lookupFileName(fin));
    }

    public Vector searchIndex(String searchString)
    {
	Vector results = new Vector(); //will contain ResultObjects
	Vector transmogrifier = new Vector();
	FileIndexObject tempFIO;
	transmogrifier = wax.searchMe(searchString);
	
	for(int x = 0; x < transmogrifier.size(); x++)
	    {
		//transmogrifier contains fileIndex numbers (which are Integers) use basicFileIndex.get( )
            tempFIO =  ((FileIndexObject) 
			      wax.basicFileIndex.get(transmogrifier.get(x)));
	
	    results.add(new ResultObject(
		 tempFIO.getFileIndex(),
		 (new Long(tempFIO.getFileSize())).intValue(),
		 tempFIO.getUCFileName())); 
	    }	
	//
	return results;
    }
}
//***** IndexWrapper END **********************************************


//***** FileIndexObject BEGIN *****************************************
//FileIndexObject are held by the basicFileIndex Hashtable contained by Wackadex
//they allow lookup of everything important about a file
//by a lookup key known as the fileIndex (Integer)

//a secondary Vector table (later a single Hashtable using hashcode keys) 
//uses ordered FileIndex numbers to find substring matches, then references
//the FileIndexObject for the information needed to create a ResultObject

class FileIndexObject implements Comparable{
    static int uniqueFileIDs = 0;
    int fileIndex;
    String fullName;
    String fileName;
    String ucFileName;
    long fileSize;
   
    FileIndexObject(String filename, String ucFileName, String fullname, long fileSize)
    {
	this.fileIndex = uniqueFileIDs;
	this.fullName = new String(fullname);
	this.ucFileName = new String(ucFileName);
	this.fileName = new String(filename);
	this.fileSize = fileSize;
	uniqueFileIDs++;  //no two records alike, non-hashcode hash table key
    }
    
    public String getFullName() 
    {
	return new String(fullName);
    }

    public String getFileName() 
    {
	return new String(fileName);
    }
    //get the upper case file name - for constructing result objects
    public String getUCFileName()
    {
	return new String(ucFileName);
    }

    //query string compared to equally sized substring at position "index"
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

    public int getFileIndex() 
    {
	return fileIndex;
    }
}
//***** FileIndexObject END *****************************************

        
//***** Wackadex BEGIN **********************************************
//how to use Wackadex: use the IndexWrapper class methods!

//If you're coding changes in Wackadex, here's how it operates:
 
    //Call Wackadex.searchMe(searchWord), get back a set of Integers
    //make a Vector of ResultObjects and send her off

    //if multiple words are being used we can call this again
    //then use a couple Vectors to easily find the overlap 
    //using Vector1.retainAll(Vector2);
    //look at the comments for class BinarySpotlight for 
    //detail on how Wackadex.searchMe(searchWord) works

class Wackadex{

    public Hashtable basicFileIndex; //one-to-one list of FileIndexObjects
                                     //ordered alphabetically by name
    Vector byteMe; //a vector of vectors- to be replaced by that hashcoded hashtable
    int numFiles; //set in makeFileList()
    int totalKB;  //set in makeFileList()

    //**accessor functions begin *************************
    public int getNumFiles() {return numFiles;}

    public int getTotalKB() {return totalKB;}

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
    //**accessor functions end **************************
    
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

    //** Wackadex constructor begin *****
    Wackadex(String dirpath)
    {
	totalKB = 0;
	numFiles = 0;
	File topdir = new File(dirpath);
      	Vector pathStrings = makeFileList(topdir);
	Vector funkyFileStrings = new Vector();
	String[] tempFileStrings = new String[pathStrings.size()];	
	String lonely_and_abused = new String();
	
	//Stringify each filename, path, size, rearrange the strings
	//so that the lowercased filename leads, and alphabetically sort them
	for(int i = 0; i < pathStrings.size(); i++)
	    {   
		//the follow code extracts and reorders filename, path, filelength
		lonely_and_abused = (String) pathStrings.get(i);
		String theWholePath;
		StringTokenizer alfred = 
		    new StringTokenizer(lonely_and_abused, "|||||");
		theWholePath = new String(alfred.nextToken());
		int filelength = Integer.parseInt(alfred.nextToken(), 10);
		String trashMe = new String();
		//set up a string tokenizer to extract a filename from a path
		StringTokenizer binky = 
		    new StringTokenizer(theWholePath, "/");
		//get the file name- the last token
		while(binky.hasMoreTokens())
		    trashMe = binky.nextToken();
		//lonely and abused stringifies what will be data fields in FileIndexObject
		//<1> the lowercased filename(for the alphabetical sort)
		//<2> the canonical filename(for returning search results)
		//<3> the canonical path (for file downloads)
		//<4> the length of the file (for results and downloads)
		lonely_and_abused = trashMe.toLowerCase() + "|||||" + trashMe + "|||||" + theWholePath
		    + "|||||" + filelength;
		tempFileStrings[i] = new String(lonely_and_abused);
	    }

        //put the stringified fields in alphabetical order
	Arrays.sort(tempFileStrings);

	Vector orderedFileNames = new Vector();
	
	basicFileIndex = new Hashtable( (int)
			(tempFileStrings.length * 1.25 + 500));
 
	//build the basicFileIndex- construct and add FileIndexObjects,
	//using fileIndex number as the hashtable key
	//necessary to reparse the stringified file information
	for(int x = 0; x < tempFileStrings.length; x++)
	    {
		StringTokenizer smurfchunk = new StringTokenizer(
			   new String(tempFileStrings[x]), new String("|||||"));  

		String fileOnly = smurfchunk.nextToken();
		String ucFileName = smurfchunk.nextToken();
	        String fullPath = smurfchunk.nextToken();
	int fsize = Integer.parseInt(smurfchunk.nextToken(), 10);
		orderedFileNames.add(fileOnly);
                
		basicFileIndex.put((Object) new Integer(x), (Object) new FileIndexObject(
			  fileOnly, ucFileName, fullPath, (long) fsize));
	    }
      
	byteMe = new Vector(); //list of Vectors (columns of
	                       //differently sorted fileIndex numbers)
	for(int v = 0; v < 50; v++)
	    byteMe.add(new Vector());

	//each column now gets filled with fileIndex Integers
	//with different orderings o
	//based on alphabetically sorted n-character-forshortened substrings

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
    //**Wackadex constructor end******

    //**inner class BinarySpotlight begin*******************************
    //
    //passes it a references vectors of ordered fileIndex numbers
    //binarySearch walks through the vector in ~(log n) moves
    //if it gets a hit it calls panOut( /**vector position*/ )
    //panOut() used plenty of hard-to-read bounds checking to
    //collect all the surrounding matches and return their fileIndex numbers.
    //binarySearch() method requires newVector() to be called first
    class BinarySpotlight
    {  
	
	Vector boojum;
	String sWord;
	int vectNum;
	int sWordlength;
	
	//Don't use binarySearch() method until newVector( ) is called
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

	//binarySearch searches through a vector 
	//of fileIndex numbers
	//that are ordered by an alphabetical sort of 
	//their n-character-forshortened-corresponding filename 
	//and return the vector positon of a matching file, 
	//or -666 if no match exists
	public int binarySearch(int base, int top)
	{
	    int x = ((int) ((top + base) / 2));
	    int substrlen =  ((String) ((FileIndexObject) 
					basicFileIndex.get(boojum.get(x))
					).getFileName()
			      ).length() - vectNum;
	   
        if(sWordlength <= substrlen)
	    {
	    int direction = ((String) ((String) ((FileIndexObject) 
						 basicFileIndex.get(boojum.get(x))
						 ).getFileName()
				       ).substring(vectNum, 
						   vectNum + sWordlength )
			     ).compareTo(sWord);

      	    if( (top - base) > 0)  
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
					      basicFileIndex.get(boojum.get(x))
					      ).getFileName()
				    ).substring(vectNum, 
						vectNum + substrlen)
			  ).compareTo(sWord) > 0 )
		    return (binarySearch(base, x));
	        else
		    return (binarySearch(x + ((base + top)%2), top));
	    }
	}
	
	//once we've spotted a valid hit,
	//pan out spreads up or down until a non-match or limit
	//is found in this direction..

	//panOut() code is admittedly ugly- 
	//the bounds checking requires gruesome casting
	//and string comparisons- not pretty, but it works.

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
						       basicFileIndex.get(boojum.get(y))
						       ).getFileName()
					     ).substring(vectNum, 
							 vectNum + sWord.length())
				   ).compareTo(sWord);

		     while((y < boojum.size()) && (comparison == 0)
			   && (sWordlength <= ((String) ((FileIndexObject) 
							 basicFileIndex.get(boojum.get(y))
							 ).getFileName()
					       ).length() - vectNum ) 
			   )
			 {
			     tempIntVect.add( new Integer((int) ((Integer) boojum.get(y)).intValue() ) );
			     x++;
			     y++;
			     if ((y < boojum.size()) 
				 && (sWordlength <= ((String) (
							       (FileIndexObject) 
							       basicFileIndex.get(boojum.get(y))
							       ).getFileName()
						     ).length() - vectNum ) 
				 )
				 comparison = ((String) ((String) ((FileIndexObject) 
								   basicFileIndex.get(boojum.get(y))
								   ).getFileName()
							 ).substring(vectNum, 
								     vectNum + sWord.length())).compareTo(sWord);
			     else comparison = -666;
			 }
                 }
	     y = startingLocation - 1;
	     if ((y >= 0) && sWordlength <= ((String) ( (FileIndexObject) 
							basicFileIndex.get(boojum.get(y))
							).getFileName()
					     ).length() - vectNum )
		 {
		     comparison = ((String) ((String) ((FileIndexObject) 
						       basicFileIndex.get(boojum.get(y))
						       ).getFileName()
					     ).substring(vectNum, 
							 vectNum + sWord.length()
							 )
				   ).compareTo(sWord);
		     
		     while((y >= 0 ) && 
			   (comparison == 0) && (sWordlength <= ((String) ( (FileIndexObject) 
									    basicFileIndex.get(boojum.get(y))
									    ).getFileName()).length() - vectNum ) )
			 {
			     tempIntVect.add( new Integer((int) ((Integer) boojum.get(y)).intValue() ) );
			     x++;
			     y--;
			     if (sWordlength <= ((String) ( (FileIndexObject) 
							    basicFileIndex.get(boojum.get(y))
							    ).getFileName()).length() - vectNum )
				 comparison = ((String) ((String) ( (FileIndexObject) 
								    basicFileIndex.get(boojum.get(y))
								    ).getFileName()).substring(vectNum, 
											       vectNum + sWord.length())
					       ).compareTo(sWord);
			     else comparison = -666;
			 }
		 }
	     return tempIntVect;
	}
    }
    //**inner class BinarySpotlight end*************************************

    //** searchMe(searchWord) sifts through byteMe's columns matching a given
    //single search word
    public Vector searchMe(String searchWord)
    {
	//binary search your way through byteMe's columns
	//(50 - searchString length) times
	Vector ultraSmurfy = new Vector();  // all of the ResultObjects we'll hand back 
	BinarySpotlight argonaut = new BinarySpotlight(searchWord); 
	int binaryspotnum; //position of a matched record in a given ordered vector
	Vector littleOldVector; //this reference re-used for each column 

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
//***** Wackadex END *******************************************************





//**test code block BEGIN **************************************************
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
//**test code block END*****************************************************











































