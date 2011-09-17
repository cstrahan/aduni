import java.io.*;
import java.util.Date;

public class FileListFilter implements FilenameFilter
{
    private String name;
    private String extension;

    // Constructor
    public FileListFilter(String name, String extension)
	{
	    this.name = name;
	    this.extension = extension;
	}

    public boolean accept(File directory, String filename)
	{
	    boolean fileOK = true;

	    // If there is a name filter specified,check the file name
	    if(name != null)
		fileOK &= filename.startsWith(name);

	    // If there is an extension filter, check extension
	    if(extension != null)
		fileOK &= filename.endsWith('.' + extension);
	    return fileOK;
	}
}
    
	
