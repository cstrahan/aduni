package sun.beanbox;

/**
 * A class loader that is used byt the JarLoader to
 * load the classes and resources.
 *
 * Class instances are given an cookie string to identify their
 * resources but our client code creates a single instance only.  This
 * is because it is impossible in general to pass values across from
 * classes in one classloader to another.
 *
 * Some of the code used here probably should be part of JDK1.(2, I guess);
 * much of it is in the sun.* classes in JDK1.1 but that is not good enough
 * since our code only depends on jdk1.1.
 *
 * This class loader can load from:
 *   - the system classes/resources (CLASSPATH)
 *   - JAR files
 *   - specific files
 *   - specific overrides for resources
 *
 *  This code still needs a cleanup pass...
 */

import java.io.*;
import java.util.*;
import java.util.zip.*;
import java.net.*;
import java.awt.*;


public class SimpleClassLoader extends ClassLoader {
    public final static String urlPrefix = "SIMPLE";
    private static final String protocolPathProp = "java.protocol.handler.pkgs";
    private static boolean debug = false; // debugging
    private static boolean keepLoading = true; // aggresively load classes
    private String cookie;	// name of the jar file

    /*
     * all instances, by cookie.  We really only instantiate one, see next, but
     * we are leaving the machinery around in case we need it again later
     */
    private static Hashtable loaders = new Hashtable();

    /*
     * The only SimpleClassLoader we actually instantiate
     */
    public static SimpleClassLoader ourLoader;

    /* 
     * Additional directory from where to look for resources...
     * (after CLASSPATH and loaded JARs).
     * -- currently unused --
     */
    private String localResourceDirectory;

    /*
     * Overrides for local resources
     */
    private Hashtable localOverrides = new Hashtable();

    /**
     * Create a SipleClassLoader.  It is identified by a cookie string
     */
    private SimpleClassLoader(String cookie, String dir) {
	this.cookie = cookie;
	this.localResourceDirectory = dir;
	loaders.put(cookie, this);
    }

    /**
     * Resource and Mime Type HashTables
     */
    private Hashtable resourceHash = new Hashtable();
    private Hashtable mimeHash = new Hashtable();

    /** A hash table of AppletClassEntry's that define .class files in their
     *  raw form.  We <B>do not</B> define these classes immediately
     *  in defineClassFromBytes(), instead the byte arrays are kept
     *  around and classes are defined lazily on demand, and the byte
     *  array is removed from the Hashtable at that time.
     *  <P>
     *  By defining classes lazily, we prevent the case where a
     *  derived class was found in a jar before its super class also
     *  in the jar.  The derived class cannot be defined before it's
     *  super class has even been loaded from the archive.
     *
     *  classes are hashed by fully qualified name - "my.package.MyApplet"
     */

    private Hashtable rawClasses = new Hashtable();


    /**
     * Set some bytecodes as a definition for a class.
     * Do not actually define the class until later
     */

    public void defineClassFromBytes(String name, byte[] buf) {
	rawClasses.put(name, buf);
    }

    /**
     * Define (& link) a class based on what was acquired previously in the JAR file
     * Should catch all exceptions and return non-null iff definition is valid
     */

    private Class applyDefinition(String name, boolean resolve) {
	byte buf[] = (byte []) rawClasses.get(name);
	rawClasses.remove(name); // release the bytecodes...
	if (buf == null) {
	    return null;
	} else {
	    Class c = null;
	    try {
		c = super.defineClass(null, buf, 0, buf.length);
		if (c != null && resolve) {
		    resolveClass(c);
		}
	    } catch (ClassFormatError e) {
		System.err.println("The definition for "+name+" in the JAR file");
		System.err.println("has a format error.");
		return null;
	    } catch (NoClassDefFoundError e) {
		// We will print a message higher up, possibly before or earlier than
		// in this invocation pass through applyDefinition.
		return null;
	    }
	    // Check that the loaded class has the name we expect
	    if (!c.getName().equals(name)) {
		System.err.println("\nWARNING: file name versus class name mismatch");
		String fname = name.replace('.', '/') + ".class";
		System.err.println("    JAR entry \"" + fname + "\" was expected "
			+ "to contain class \"" + name + "\"");
		System.err.println("    but instead contained class \"" + c.getName() + "\"");
		System.err.println("    This may cause future class-loading problems.\n");
	    }
	    return c;
	}
    }

    /**
     * Define a class from a file
     */

    private static byte[] getByteArray(String fileName) throws IOException {
	File f = new File(fileName);
	int length = (int)f.length();
	byte buff[] = new byte[length];

	InputStream is = new FileInputStream(fileName);
	int read = 0;
	while (read < length) {
	    int r = is.read(buff, read, length-read);
	    if (r < 0) {
		break;
	    }
	    read += r;
	}
	return buff;
    }

    /**
     * Helper function; load a class from a file
     */
    public Class loadClassFromFile(String fileName) throws ClassNotFoundException {
	InputStream is;
	try {
	    byte buf[] = getByteArray(fileName);
	    Class c = super.defineClass(null, buf, 0, buf.length);
	    if (c != null) {
		resolveClass(c);
	    }
	    if (c==null)
	      throw new ClassNotFoundException(fileName);
	    return c;
	} catch (Exception ex) {
	    debug("LoadFromFile/caught "+ex+" when loading from file "+fileName);
	    throw new ClassNotFoundException(fileName);
	}
    }

    /**
     * Load a class from this class loader.
     *
     * @exception  ClassNotFoundException  if the class could not be found.
     */
    public Class loadClass(String name) throws ClassNotFoundException {
	return loadClass(name, false);
    }

    /**
     * This is the main method for ClassLoaders, that is being redefined
     *
     * @exception  ClassNotFoundException  if the class could not be found.
     */

    protected Class loadClass(String name, boolean resolve)
	    throws ClassNotFoundException {
	
	/* We check to see if we've already loaded it (i.e. in the cache) */
	Class cl = findLoadedClass(name);

	// NOTE! the ordering is currently inverted from that in AppletClassLoader

	// If the class hasn't already been loaded from the JAR,
	// we first try looking in the JAR:
	if (cl == null) {
	    // First try to load the class from the JAR:
	    cl = applyDefinition(name, resolve);
	}

	// If the class isn't in the JAR, try the system classloader:
	if (cl == null) {
	    try {
		cl = findSystemClass(name);
		return cl;
	    } catch (ClassNotFoundException e) {
		// Drop through.
	    }
	}
	if (cl == null) {
	    throw new ClassNotFoundException(name);
	}
	if (resolve) {
	    resolveClass(cl);
	}
	return cl;
    }

    /**
     * Interface to Beans.instantiate.
     * Name is that of the bean.
     * The stream producer provides the serialized representation of the bean
     */

    public Object instantiate(String name, InputStreamProducer isp) 
	throws ClassNotFoundException, IOException {
	String sname = name.replace('.', '/');
	Object back = null;
	try {
	    setLocalResourceSource(sname+".ser", isp);
	    back = java.beans.Beans.instantiate(this, name);
	} finally {
	    localOverrides.remove(sname+".ser");
	}
	return back;
    }

    /**
     * The resource stuff
     */

    /**
     * Assign an InputStream as the source for a given property name
     * This value comes first after the system resources
     */

    // This method can be private
    public void setLocalResourceSource(String name, InputStreamProducer isp) {
	localOverrides.put(name, isp);
    }

    void putClassResource(String name, String type) {
	resourceHash.put(name, "A CLASS FILE");
	mimeHash.put(name, type);
    }

    void putLocalResource(String name, byte[] data, String type) {
	resourceHash.put(name, data);
	mimeHash.put(name, type);
    }

    public URL getResource(String name) {
	URL back = getSystemResource(name);
	if (back != null) {
	    return back;
	}
	return getLocalResource(name);
    }

    public InputStream getResourceAsStream(String name) {
	InputStream back = getSystemResourceAsStream(name);
	if (back != null) {
	    return back;
	}
	return getLocalResourceAsStream(name);
    }


    /**
     * Return a URL to the desired resource.
     */
    public URL getLocalResource(String name) {
	// Check if there is an override
	Object o;
	o = localOverrides.get(name);
	if (o == null) {
	    // Check on the JAR objects
	    o = resourceHash.get(name);
	}
	if (o == null && localResourceDirectory != null) {
	    // Try in localResourceDirectory
	    File f = new File(localResourceDirectory, name);
	    if (f.exists()) {
		o = new Integer("1"); // anything will do
	    }
	}
	if (o != null) {
	    // Create a URL to refer to this resource
	    try {
		URL url = new URL("simpleresource",
				  "",
				  "/"+urlPrefix+
				  cookie+"/+/"+name);
		return url;
	    } catch (Exception e) {
		debug("Exception "+e+" while building a resource URL");
		return null;
	    }
	}
	return null;
    }

    private InputStream getLocalResourceAsStream(String name) {
	Object o;
	// Check if there is an override
	o = localOverrides.get(name);
	if (o!=null) {
	    return ((InputStreamProducer) o).getInputStream();
	}
	// Check data loaded from JAR
	o = resourceHash.get(name);

	if (o != null) {
	    if (o instanceof String) {
		// This is a .class entry...
		// no access to .class files through getResource() in 1.1
		throw new SecurityException("No access through getResource() to .class in 1.1");
	    }

	    byte[] buf = (byte []) o;
	    return new ByteArrayInputStream(buf);
	}
	if (localResourceDirectory != null) {
	    // Now try in localResourceDirectory
	    File f = new File(localResourceDirectory, name);
	    try {
		return new FileInputStream(f);
	    } catch (Exception ex) {
		return null;
	    }
	}
	return null;
    }

    /**
     * Returns an InputStream on the resource
     */

    public static SimpleClassLoader createLoader(String cookie,
						 String dir) {
	SimpleClassLoader back = getLoader(cookie);
	if (back != null) {
	    if (!back.localResourceDirectory.equals(dir)) {
		throw new Error("internal error!");
	    }
	    return back;
	} else {
	    return new SimpleClassLoader(cookie, dir);
	}
    }

    private static SimpleClassLoader getLoader(String cookie) {
	return (SimpleClassLoader) loaders.get(cookie);
    }

    // get the local resource object...

    public static Object getLocalResource(String cookie,
					  String name) {
	SimpleClassLoader cl = getLoader(cookie);

	// Check if there is an override
	Object o = cl.localOverrides.get(name);
	if (o!=null) {
	    return ((InputStreamProducer) o).getInputStream();
	}
	// Check data loaded from JAR
	String type = (String) cl.mimeHash.get(name);
	if (type!=null) {
	    // Came from JAR
	    o = cl.resourceHash.get(name);

	    if (o instanceof String) {
		// This is a .class entry...
		// no access to .class files through getResource() in 1.1
		throw new SecurityException("No access through getResource() to .class in 1.1");
	    }

	    byte[] buf = (byte []) o;
	    if (type.startsWith("image")) {
		return ((Toolkit.getDefaultToolkit()).createImage(buf)).getSource();
	    } else {
		return new ByteArrayInputStream(buf);
	    }
	}
	if (cl.localResourceDirectory != null) {
	    // Check into localResourceDirectory
	    File f = new File(cl.localResourceDirectory, name);
	    if (f.exists()) {
		try {
		    URL url = new URL("file",
				      "",
				      f.getAbsolutePath());
		    return url.getContent();
		} catch (Exception e) {
		    throw new Error("no such resource"); // should it be a security error?
		}
	    }
	}
	return null;
    }

    // REMIND - simplify the whole cookie thing.

    public static InputStream getLocalResourceAsStream(String cookie,
						       String name) {
	SimpleClassLoader cl = getLoader(cookie);
	return cl.getLocalResourceAsStream(name);
    }

    /**
     * Define a class from the bytecodes that were collected early...
     * Will return false if there were problems applying the definitions
     *
     * This is only invoked if we are defining classes up front.
     * Code will be removed if we don't discover any problems in testing.
     */

    public synchronized boolean applyDefinitions(Vector classList) {
	// go through the bytecode arrays defininng.
	// NOTE: sometimes a forward reference forces a class
	// to get defined early; in that case, skip it!

	boolean back = true;
	for (Enumeration k = classList.elements();
	     k.hasMoreElements();) {
	    String classname = (String) k.nextElement();
	    Class c = findLoadedClass(classname);
	    if (c == null) {
		// Not yet defined, do so.
		c = applyDefinition(classname, true);
		if (c == null) {
		    if (back == true) {
			System.err.println("NOTE: There are classes that cannot be defined in this JAR file");
			System.err.println("    Some of these classes will cause the failure of defining or linking ");
			System.err.println("    other classes that depend on them.");
			if (keepLoading) {
			    System.err.println("NOTE: To simplify debugging JAR files, we will proceed loading classes");
			    System.err.println("    although this may lead eventually to an UnknownError or the like");
			    System.err.println();
			}
		    }
		    System.err.println("Class "+classname+" could not be defined from JAR file");
		    back = false;
		}
	    }
	}
	return back;
    }

    /**
     * Debugging stuff
     */
    private static void debug(String msg) {
	if (debug) {
	    System.err.println("SimpleClassLoader:: "+msg);
	}
    }

    /**
     * Initialization of statics
     */
    static {
	// Add this protocol type to the http properties
	Properties newP = new Properties(System.getProperties());
	newP.put(protocolPathProp,
		 newP.getProperty(protocolPathProp)+"|sun.beanbox");
	System.setProperties(newP);
	// *The* loader instance
	ourLoader = createLoader("BeanBox", null);
    }
}
