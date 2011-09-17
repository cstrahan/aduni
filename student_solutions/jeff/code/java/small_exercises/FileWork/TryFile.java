import java.io.*;

public class TryFile
{
    public static void main(String[] args)
    {
	// Create an object that is a directory
	File myDir = new File("/home/jeffrey/code/java/FileWork");
	System.out.println(myDir +
			   (myDir.isDirectory()?" is " : " is not ") +
			   "a directory.");

	// Create an object that is a file
	File myFile = new File(myDir, "foo.bar");
	System.out.println(myFile +
			   (myFile.exists() ? " does":" does not") +
			   " exist.");
	System.out.println("You can" +
			   (myFile.canRead()? " ":" not ") + "read " +
			   myFile);
	System.out.println("You can" +
			   (myFile.canWrite()? " ":" not ") + "write " +
			   myFile);
	return;
    }
}
