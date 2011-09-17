import java.io.*;
import java.util.*;

public class SharedDirectory
{
    static ArrayList filestoshare = new ArrayList();
    static ArrayList filesizes = new ArrayList();
    static File savepath;
    static int numfiles = 0;
    static long bytes = 0;

    public SharedDirectory(String sharepath, String savepath)
    {
	generateFileList(new File(sharepath));
	this.savepath = new File(savepath);
    }
    
    public static void generateFileList(File directorytosearch)
    {
	String[] filenames = directorytosearch.list(); // All of the files and directories in the current folder.

	for (int i = 0; i < filenames.length; i++)
	    {
		File f = new File(directorytosearch.getPath(), filenames[i]); // All of the File objects in the current folder.
		if (f.isHidden() || !(f.canRead())) // Don't ever report the existence of hidden files or ones for which we have incorrect permissions.
		    continue;
		if (f.isDirectory()) {
		    generateFileList(f);} // Recurse through all subfolders.
		else
		    {
		    filestoshare.add(f); // Each file will have a unique index in this ArrayList, which will become the Gnutella File Index for the Result Set.
		    Integer size = new Integer((int)(f.length()));

		    filesizes.add(size); // Sure wish ArrayLists could hold ints rather than Integers.

		    numfiles++;
		    bytes += size.intValue();
		    }
	    }
    }

    public static ResultSet search(String query) // Eventually, this would be better as a wackadexy thing, but for now it's linear.
    {
	ArrayList r = new ArrayList(); /* Because of the way ResultSets work, we have to know how many hits we have before we assemble our ResultSet, so we'll
					  store them temporarily here.  A better design decision would have been to have ResultSet use ArrayLists internally.
					  If we have time, we'll make the change. */

	for (int i = 0; i < filestoshare.size(); i++)
		if ((((File)filestoshare.get(i)).getName()).indexOf(query) != -1) // Check to see if the query is a substring of the filename
		    {
			r.add(new Integer(i)); // File index
			r.add(new Integer(((Integer)filesizes.get(i)).intValue())); /* This legerdemain is necessary because Integers are objects, but we want
											  to copy their values, not their pointers. */
			r.add(((File)filestoshare.get(i)).getName()); // Add the name of the matching file.
		    }
	ResultSet results = new ResultSet((int)(r.size() / 3));  /* Now we know how many answers we're giving, so we can dimension the ResultSet properly.
								    We divide by three because we also store our index and file size. */
	for (int i = 0; i < r.size(); i += 3) // Yipes! ---
	    results.addResult(((Integer)r.get(i)).intValue(), ((Integer)r.get(i + 1)).intValue(), ((String)r.get(i + 2)));
	
	return (results);
    }

    public static boolean validate(int index, String filename)
    {
	if (index < numfiles)
	    return (((File)filestoshare.get(index)).getName().equals(filename));
	else
	    return (false);
    }

    public static File getFile(int index) // This method should only be called after the index has been validated.
    {
	return ((File)filestoshare.get(index));
    }

    public static int getFileSize(int index) // Call only after validating.
    {
	return (((Integer)filesizes.get(index)).intValue());
    }

    public static int getOurNumFiles()
    {
	return (numfiles);
    }

    public static int getOurKb()
    {
	return ((int)(bytes / 1000));
    }

    public static File getOurSavePath()
    {
	return (savepath);
    }
}





