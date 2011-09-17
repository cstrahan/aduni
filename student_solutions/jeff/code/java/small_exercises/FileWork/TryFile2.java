import java.io.*;
import java.util.Date;

public class TryFile2
{
    public static void main(String[] args)
    {
	// Create an object that is a directory
	File myDir = new File("/home/jeffrey/code/java/FileWork");
	System.out.println(myDir.getAbsolutePath() +
			   (myDir.isDirectory()?" is " : " is not ") +
			   "a directory.");
	System.out.println("The parent of " + myDir.getName()
			   + " is " + myDir.getParent());

	// Define a filter
	FilenameFilter select = new FileListFilter("F", "java");
		
	// get the contents of the directory
	File[] contents = myDir.listFiles(select);
	
	if(contents != null)
	    {
		System.out.println("\nThe " + contents.length
				   + " matching items in the directory " +
				   myDir.getName() + " are:");
		for(int i = 0; i < contents.length; i++)
		    System.out.println(contents[i].getName() + " is a " +
				       (contents[i].isDirectory() ?
					"directory" : "file") +
				       " last modified " +
				       new Date(contents[i].lastModified()));
	    }
	else
	    System.out.println(myDir.getName() + " is not a directory");
				       
	return;
    }
}
