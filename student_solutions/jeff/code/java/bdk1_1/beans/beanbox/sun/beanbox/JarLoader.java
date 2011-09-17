package sun.beanbox;

/**
 * Read the contents of a JAR file.
 *
 */

import java.io.*;
import java.util.*;
import java.util.zip.*;
import java.beans.*;
import java.net.*;
import java.awt.*;

public class JarLoader {

    private static boolean debug = false; // debugging
    private InputStream jarStream; // Jar input stream
    private String jarName;	// name of the jar file
    private SimpleClassLoader loader; // the loader instance
    private static boolean warnedAboutNoBeans;


    /**
     * Create a JarLoader to read a JAR and to process its contents.
     * Classes and resources are loaded against a single common class
     * loader instance so that things like "adaptor class instantiaton"
     * can work.
     *
     * Loading is started with loadIt()
     */
    public JarLoader(String jarName) throws FileNotFoundException {
	// wil check that this file exists, and that is about it.
	debug("("+jarName+")");
	this.jarName = jarName;
	InputStream is = new FileInputStream(jarName);
	jarStream = new BufferedInputStream(is);
	loader = SimpleClassLoader.ourLoader;
    }

    /**
     * get the loader we are using
     */
    public ClassLoader getLoader() {
	return loader;
    }

    /*
     * In here for compatibility with older versions of JDK1.1
     */
    private String guessContentTypeFromStream(InputStream is) throws IOException {
	String type;
	type = URLConnection.guessContentTypeFromStream(is);
	// that should be taught about serialized objects.
	
	if (type == null) {
	    is.mark(10);
	    int c1 = is.read();
	    int c2 = is.read();
	    int c3 = is.read();
	    int c4 = is.read();
	    int c5 = is.read();
	    int c6 = is.read();
	    is.reset();
	    if (c1 == 0xAC && c2 == 0xED) {
		type = "application/java-serialized-object";
	    }
	}
	return type;
    }

    /**
     * Load the classes, resources, etc.
     */
    public JarInfo loadJar() throws IOException {
	// load all resources.
	// may raise an exception if the file is not a Jar file.
	ZipInputStream zis = null;
	Manifest mf = null;
	Vector classList = new Vector(); // the classes
	Vector serList = new Vector(); // the serialized objects

	byte buffer[] = new byte[1024];
	boolean empty = true;

	try {
	    zis = new ZipInputStream(jarStream);
	    ZipEntry ent = null;

	    while ((ent = zis.getNextEntry()) != null) {
	        empty = false;

		String name = ent.getName();
		String type = null;

		/* the object we're loading */
		ByteArrayOutputStream baos = new ByteArrayOutputStream();

		/* NOTE: We don't know the size of an entry until
		   we've reached the end of one because of
		   compression. This means we can't just do a get size
		   and read in the entry. */

		for (;;) {
		    int len = zis.read(buffer);
		    if (len < 0) {
			break;
		    }
		    baos.write(buffer, 0, len);
		}

		byte[] buf = baos.toByteArray();
		int size = buf.length;
		
		if (Manifest.isManifestName(name)) {
		    type = "manifest/manifest";
		}

		if (type == null) {
		    InputStream tmpStream = new ByteArrayInputStream(buf);
		    type = guessContentTypeFromStream(tmpStream);
		    tmpStream.close();
		}

		if (type == null) {
		    type = "input-stream/input-stream";
		}

		if (type.startsWith("application/java-serialized-object") ||
		    type.startsWith("application/x-java-serialized-object")) {
		    // Else make the data available as a local stream.
		    loader.putLocalResource(name, buf, type);

		    // Tag it as a serialized object for the bean list.
		    String sername = name.substring(0, name.length() - 4);
		    sername = sername.replace('/', '.');
		    serList.addElement(sername);

		} else if (type.startsWith("application/java-vm") ||
			   type.startsWith("application/x-java-vm")) {
		    /*
		     * In JDK1.1, a classloader needs not provide access to the
		     * bytecodes of a .class resource.  But it needs to provide
		     * a URL for it (e.g. for URL math computation).
		     *
		     * The bytecodes are kept (and later released) by the
		     * SimpleClassLoader instance.
		     * Here we just tag the existance of the object.
		     */

		    loader.putClassResource(name, type);

		    /* remove the .class suffix */
		    String classname = name.substring(0, name.length() - 6);
		    classname = classname.replace('/', '.');
		    loader.defineClassFromBytes(classname, buf);
		    classList.addElement(classname);

		} else if (type.equals("manifest/manifest")) {
		    mf = new Manifest(buf);

		} else {
		    // Else make the data available as a local stream.
		    loader.putLocalResource(name, buf, type);
		}
	    }

	} catch (IOException ex) {
	    debug("IOException loading archive: " + ex);
	    throw ex;
	} catch (Throwable th) {
	    debug("Caught " + th + " in loadit()");
	    th.printStackTrace();
	    throw new IOException("loadJar caught: " + th);
	} finally {
	    if (zis != null) {
		try {
		    zis.close();
		} catch (Exception ex) {
		    // ignore
		}
	    }
	}

	// Unfortunately ZipInputStream doesn't throw an exception if you hand
	// it a non-Zip file.  Our only way of spotting an invalid Jar file
	// is if there no ZIP entries.
	if (empty) {
	    throw new IOException("JAR file is corrupt or empty");
	}

	if (! BeanBoxFrame.getDefineOnDemand()) {
	    if (! loader.applyDefinitions(classList)) {
		return null;
	    }
	}
	JarInfo ji = createJarInfo(classList, serList, mf);
	return ji;
    }

    /**
     * Load the JAR file, then apply an action to each bean found
     */
    public static void loadJarDoOnBean(String jarFile, DoOnBean action) 
							throws java.io.IOException {
	JarLoader jl = new JarLoader(jarFile);
	JarInfo ji = jl.loadJar();
	if (ji == null) {
	    System.err.println("JAR file "+jarFile+" did not load properly!");
	    System.err.println("Check for error messages possibly regarding");
	    System.err.println("problems defining classes");
	    return;
	}
	if (ji.getCount() == 0) {
	    System.err.println("Jar file "+jarFile+" didn't have any beans!");
	    if (!warnedAboutNoBeans) {
		// We only print this explanatory message once.
		warnedAboutNoBeans = true;	
		System.err.println("");
	        System.err.println("Each jar file needs to contain a manifest file describing which entries are");
	        System.err.println("beans.  You can should provide a suitable manifest when you create the jar.");
		System.err.println("");
	    }
	}
	for (int i=0; i<ji.getCount(); i++) {
	    String beanName = ji.getName(i);
	    BeanInfo bi = ji.getBeanInfo(i);

	    if (bi == null) {
		// We couldn't load the bean.
		continue;
	    }

	    action.action(ji, bi, beanName);
	}
    }


    /**
     * Create a JarInfo from a manifest and a class list
     */

    private JarInfo createJarInfo(Vector classList,
				  Vector serList,
				  Manifest mf) {
	Hashtable beans;
	Hashtable headersTable = new Hashtable();
	if (mf == null) {
	    // Beans are only identified through a manifest entry.
	    // If we don't have a manfiest, the beans hashtable
	    // should remain empty.
	    beans = new Hashtable();
	} else {
	    beans = new Hashtable();
	    for (Enumeration entries = mf.entries();
		 entries.hasMoreElements();) {
		MessageHeader mh = (MessageHeader) entries.nextElement();
		String name = mh.findValue("Name");
		String isBean = mh.findValue("Java-Bean");
		if (isBean != null && isBean.equalsIgnoreCase("True")) {
		    String beanName;
		    boolean fromPrototype = true;
		    if (name.endsWith(".class")) {
			fromPrototype = false;
			beanName = name.substring(0, name.length() - 6);
		    } else if (name.endsWith(".ser")) {
			beanName = name.substring(0, name.length() - 4);
		    } else {
			beanName = name;
		    }
		    beanName = beanName.replace('/', '.');
		    beans.put(beanName, new Boolean(fromPrototype));
		    headersTable.put(beanName, mh);
		}
	    }
	}

	String beanNames[] = new String[beans.size()];
	boolean fromPrototype[] = new boolean[beans.size()];
	MessageHeader headers[] = new MessageHeader[beans.size()];
	Enumeration keys;
	int i;
	for (keys = beans.keys(), i = 0;
	     keys.hasMoreElements();
	     i++) {
	    String key = (String) keys.nextElement();
	    beanNames[i] = key;
	    fromPrototype[i] = ((Boolean)beans.get(key)).booleanValue();
	    headers[i] = (MessageHeader) headersTable.get(key);
	}

	return new JarInfo(jarName, loader, beanNames, fromPrototype, headers);
    }


    /**
     * Debugging stuff
     */
    private static void debug(String msg) {
	if (debug) {
	    System.err.println("JarLoader:: "+msg);
	}
    }

}
