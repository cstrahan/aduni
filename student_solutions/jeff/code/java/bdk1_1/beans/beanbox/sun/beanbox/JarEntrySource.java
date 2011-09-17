package sun.beanbox;

/**
 * Used to request creating Jars
 */

import java.io.InputStream;
import java.io.FileInputStream;
import java.io.File;

public class JarEntrySource {
    public JarEntrySource(File file) {
	this.markOnly = false;
	this.name = file.getPath().replace(File.separatorChar, '/');
	this.file = file;
    }

    public JarEntrySource(String name, File file) {
	this.markOnly = false;
	this.name = name;
	this.file = file;
    }

    public JarEntrySource(String name, InputStream is) {
	this.markOnly = false;
	this.name = name;
	this.is = is;
    }

    public JarEntrySource(String name) {
	this.markOnly = true;
	this.name = name;
    }
    /**
     * Accessors
     */
    public boolean isMarkOnly() {
	return markOnly;
    }

    public long getTime() {
	if (file != null) {
	    return file.lastModified();
	} else {
	    return 0;		// ??
	}
    }

    public long getLength() {
	if (file != null) {
	    return file.length();
	} else {
	    return 0;		// ??
	}
    }

    public String getName() {
	return name;
    }

    public InputStream getInputStream() {
	if (file != null) {
	    try {
		return new FileInputStream(file);
	    } catch (Exception ex) {
		return null;
	    }
	} else {
	    return is;
	}
    }

    private boolean markOnly;	// only mark in the ZIP
    private String name;	// the name of this entry
    private InputStream is;	// the input stream for the entry
    private File file;		// if a file
    private long modifiedTime;	// if an inputstream
}
