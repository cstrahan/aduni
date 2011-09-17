package sun.beanbox;

import java.io.*;
import java.util.*;
import java.util.zip.*;

/**
 * This class implements a simple utility for creating files in the JAR
 * (Java Archive) file format. The JAR format is based on the ZIP file
 * format, with optional meta-information stored in a MANIFEST entry.
 *
 * It borrows from JDK's jar tool.
 */

public
class JarAccess {
    static final String MANIFEST = "META-INF/MANIFEST.MF";
    static final char SEPARATOR = File.separatorChar;

    private File jarName;	// the (relative to WD) name of the JarFile
    private File dirName;	// the (relative to WD) name of the base directory
    private String beanName;	// the (relative to base) beanFileName
    private String[] fileNames;	// all (relative to base) file names

    /**
     * A command line interface.
     * Usage is:
     *	jar [-bean BeanFileName] [-dir Directory] JarName fileNames...
     */

    public static void main(String args[]) {
	if (args.length <= 1) {
	    error("not enough arguments");
	    usageError();
	}
	JarAccess tool = new JarAccess();
	if (! tool.parseArgs(args)) {
	    usageError();
	}
	tool.createJarFile();
    }

    /**
     * Create a Jar from parsed argument data
     */

    private void createJarFile() {
	try {
	    OutputStream out = new FileOutputStream(jarName);
	    if ((beanName == null) && (dirName == null)) {
		create(out, fileNames);
	    } else {
		create(out, dirName, beanName, fileNames);
	    }
	} catch (Exception ex) {
	    System.err.println("caught exception: "+ex);
	    ex.printStackTrace();
	    System.exit(1);
	}
    }

    /**
     * Create a new JAR file;
     * Given a base directory, a beanfilename, and a set of files names,
     * these two relative to the base directory
     *
     * if baseDir is null, it means WD
     * if beanFIle is null, it means generate no MANIFEST
     *
     * Generates a *non-signed* MANIFEST
     */
    public static void create(OutputStream out,
			      File baseDir,
			      String beanFile,
			      String[] files)
	throws IOException
    {
	int start = 0;
	if (beanFile != null) {
	    start = 1;
	}
	JarEntrySource[] data = new JarEntrySource[files.length + start];
	if (beanFile != null) {
	    data[0] = makeManifestEntry(beanFile);
	}
	for (int i = 0; i<files.length; i++) {
	    data[i+start] = new JarEntrySource(entryName(files[i]),
					       new File(baseDir, files[i]));
	}
	create(out, data);
    }

    /**
     * An InputStream with the data about the Manifest
     */
    public static JarEntrySource makeManifestEntry(String beanName) {
	StringBuffer s = new StringBuffer("Manifest-Version: 1.0\n");
	s.append("\n");
	s.append("Name: "+beanName+"\n");
	s.append("Java-Bean: True\n");
	s.append("\n");
	return new JarEntrySource(MANIFEST,
				  new StringBufferInputStream(s.toString()));
    }

    /**
     * Creates a new ZIP file with a bunch of files
     */
    public static void create(OutputStream out,
		       String[] files) throws IOException {
	ZipOutputStream zos = new ZipOutputStream(out);
	for (int i = 0; i < files.length; i++) {
	    addEntry(zos, new JarEntrySource(new File(files[i])));
	}
	zos.close();
    }

      /**
     * Creates a new ZIP file with a bunch of entries
     */
    public static void create(OutputStream out,
		       JarEntrySource[] entries) throws IOException {
	ZipOutputStream zos = new ZipOutputStream(out);
	for (int i = 0; i < entries.length; i++) {
	    addEntry(zos, entries[i]);
	}
	zos.close();
    }

    private static String entryName(String name) {
	name = name.replace(File.separatorChar, '/');
	if (name.startsWith("/")) {
	    name = name.substring(1);
	} else if (name.startsWith("./")) {
	    name = name.substring(2);
	}
	return name;
    }

    /*
     * Adds a new file entry to the ZIP output stream.
     */
    static void addEntry(ZipOutputStream zos,
			 JarEntrySource source) throws IOException {
	String name = source.getName();
	if (name.equals("") || name.equals(".")) {
	    return;
	}

	//	long size = source.getLength();

	ZipEntry e = new ZipEntry(name);

	e.setTime(source.getTime());
	boolean markOnly = source.isMarkOnly();

	if (markOnly) {
	    e.setMethod(ZipEntry.STORED);
	    e.setSize(0);
	    e.setCrc(0);
	}
	zos.putNextEntry(e);
	if (! markOnly) {
	    byte[] buf = new byte[1024];
	    int len;
	    InputStream is = new BufferedInputStream(source.getInputStream());
	    while ((len = is.read(buf, 0, buf.length)) != -1) {
		zos.write(buf, 0, len);
	    }
	    is.close();
	}
	zos.closeEntry();
    }

    /*
     * Extracts specified entries from JAR file.
     */
    private static void extract(InputStream in,
				String files[]) throws IOException {
	ZipInputStream zis = new ZipInputStream(in);
	ZipEntry e;
	while ((e = zis.getNextEntry()) != null) {
	    if (files == null) {
		extractFile(zis, e);
	    } else {
		String name = e.getName().replace('/', File.separatorChar);
		for (int i = 0; i < files.length; i++) {
		    if (name.startsWith(files[i])) {
			extractFile(zis, e);
			break;
		    }
		}
	    }
	}
    }

    /*
     * Extracts next entry from JAR file, creating directories as needed.
     */
    private static void extractFile(ZipInputStream zis, ZipEntry e)
	throws IOException
    {
	File f = new File(e.getName().replace('/', File.separatorChar));
	if (e.isDirectory()) {
	    if (!f.exists() && !f.mkdirs() || !f.isDirectory()) {
		throw new IOException(f + ": could not create directory");
	    }
	} else {
	    if (f.getParent() != null) {
		File d = new File(f.getParent());
		if (!d.exists() && !d.mkdirs() || !d.isDirectory()) {
		    throw new IOException(d + ": could not create directory");
		}
	    }
	    OutputStream os = new FileOutputStream(f);
	    byte[] b = new byte[512];
	    int len;
	    while ((len = zis.read(b, 0, b.length)) != -1) {
		os.write(b, 0, len);
	    }
	    zis.closeEntry();
	    os.close();
	}
    }

    /*
     * Lists contents of JAR file.
     */
    private static void list(InputStream in, String files[])
	throws IOException
    {
	ZipInputStream zis = new ZipInputStream(in);
	ZipEntry e;
	while ((e = zis.getNextEntry()) != null) {
	    String name = e.getName().replace('/', File.separatorChar);
	    /*
	     * In the case of a compressed (deflated) entry, the entry size
	     * is stored immediately following the entry data and cannot be
	     * determined until the entry is fully read. Therefore, we close
	     * the entry first before printing out its attributes.
	     */
	    zis.closeEntry();
	    if (files == null) {
		printEntry(e);
	    } else {
		for (int i = 0; i < files.length; i++) {
		    if (name.startsWith(files[i])) {
			printEntry(e);
			break;
		    }
		}
	    }
	}
    }

    /*
     * Prints entry information.
     */
    private static void printEntry(ZipEntry e)	throws IOException {
	output(e.getName());
    }

    /**
     * Parse the arguments
     */
    private boolean parseArgs(String args[]) {
	int l = args.length;
	int i;
	for (i = 0; i<l; i++) {
	    if (args[i].equals("-bean")) {
		if (i+1 >= l) {
		    error("-bean option needs to be followed by an argument");
		    return false;
		}
		beanName = args[i+1];
		i += 1;
	    } else if (args[i].equals("-dir")) {
		if (i+1 >= l) {
		    error("-dir option needs to be followed by an argument");
		    return false;
		}
		dirName = new File(args[i+1]);
		i += 1;
	    } else {
		break;
	    }
	}
	if (i+1 >= l) {
	    error("not enough arguments");
	    return false;
	}
	jarName = new File(args[i]);
	i += 1;
	fileNames = new String[l-i];
	for (int j=0; j<l-i ; j++) {
	    fileNames[j] = args[j+i];
	}
	// printArgs();
	return true;
    }

    /**
     * Print the argumens read, for debugging
     */
    private void printArgs() {
	System.err.println("jarName: "+jarName);
	System.err.println("dirName: "+dirName);
	System.err.println("beanName: "+beanName);
	System.err.println("fileNames: "+fileNames);
	if (fileNames != null) {
	    for (int i=0; i<fileNames.length; i++) {
		System.err.println("fileNames["+i+"]: "+fileNames[i]);
	    }
	}
    }

    /**
     * Print usage information
     */
    private static void usageError() {
	error("Usage: JarAccess [-bean BeanFileName] [-dir Directory] jarFileName files...");
	error("  Create a jar; does no object signing");
	error("Options:");
	error("  -bean  BeanFileName is the FileName of a Bean to be tagged");
	error("  -dir   Directory is the directory from where to interpret file names");
	error("         so the manifest will have relative names");
	System.exit(1);
    }

    /**
     * Print an output message
     */
    protected static void output(String s) {
	System.err.println(s);
    }

    /**
     * Print an error message
     */
    protected static void error(String s) {
	System.err.println(s);
    }

}
