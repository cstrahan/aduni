package sun.beanbox;

import java.beans.*;
import java.util.Hashtable;

/**
 * Represents a loaded Jar file
 */

public class JarInfo {
    /**
     * Private data
     */
    private SimpleClassLoader classLoader;
    private String[] beanNames;
    private BeanInfo[] beanInfos;
    private boolean[] fromPrototype;
    private MessageHeader[] manifestData;
    private String jarName;
    private static Hashtable beanToJar = new Hashtable();

    // We currently don't care much on whether a bean is from
    // a serialized prototype or not; the only distinction is in the lazy
    // computation of the BeanInfo...

    /** 
     * Create a JarInfo.
     * @param	jarName	The name of the file containing the Jar
     * @param	cl	The ClassLoader instance
     * @param	beanName	The names for all the beans
     * @param	fromPrototype	Whether this bean is from a serialized prototype
     */
    public JarInfo(String jarName,
		   SimpleClassLoader cl,
		   String[] beanNames,
		   boolean[] fromPrototype,
		   MessageHeader[] manifestData) {
	if (beanNames.length != fromPrototype.length) {
	    throw new Error("beanNames and fromPrototype need to have the same length");
	}
	this.jarName = jarName;
	this.classLoader = cl;
	this.beanNames = beanNames;
	this.fromPrototype = fromPrototype;
	this.manifestData = manifestData;
	this.beanInfos = new BeanInfo[beanNames.length];
	for (int i = 0; i < beanNames.length; i++) {
	    beanToJar.put(beanNames[i], jarName); // record where this beanName came from
	    if (fromPrototype[i]) {
		// delay instantiating it
		continue;
	    }
	    // get the BeanInfo data
	    Class c;
	    try {
		c = cl.loadClass(beanNames[i]);
	    } catch (Exception ex) {
		// We don't print an error at this point.  Instead we print
		// an error later, in JarInfo.getInstance.
		// System.err.println("Could not find " + beanNames[i]
		//				+ " in " + jarName);
		continue;
	    }
	    BeanInfo bi;
	    try {
		bi = Introspector.getBeanInfo(c);
	    } catch (Exception ex) {
		System.err.println("JarInfo: couldn't find BeanInfo for "+c+
				   "; caught "+ex);
		continue;
	    }
	    beanInfos[i] = bi;
	    debug("JarInfo:: @ "+i+"; beanName: "+beanNames[i]+
		  "; fromPrototype: "+fromPrototype[i]);
	}
    }

    /**
     * Global information: where did a given bean came from?
     */
    public static String getJarName(String beanName) {
	return (String) beanToJar.get(beanName);
    }

    /**
     * Information on this JarInfo instance
     */

    /**
     * Get the name of the file containing this JAR
     */
    public String getJarName() {
	return jarName;
    }

    /**
     * Get the number of beans in this Jar file
     */
    public int getCount() {
	return beanNames.length;
    }

    /**
     * Get the BeanInfo for the ith bean in this jar file
     */
    public BeanInfo getBeanInfo(int i) {
	if (beanInfos[i] != null) {
	    return beanInfos[i];
	} else {
	    Object b = getInstance(beanNames[i]);
	    if (b != null) {
		Class c = b.getClass();
		BeanInfo bi;
		try {
		    bi = Introspector.getBeanInfo(c);
		} catch (Exception ex) {
		    System.err.println("JarInfo: couldn't find BeanInfo for "+c+"; caught "+ex);
		    return null;
		}
		beanInfos[i] = bi;
		return bi;
	    }
	    return null;
	}
    }

    /**
     * The bean name of this bean
     */
    public String getName(int i) {
	return beanNames[i];
    }

    /**
     * Was this bean from a serialized prototype? -- unused so far?
     */
    public boolean isFromPrototype(String name) {
	return fromPrototype[indexForName(name)];
    }

    /**
     * Get to Manifest Headers for this bean
     */
    public MessageHeader getManifestData(String name) {
	return manifestData[indexForName(name)];
    }

    /**
     * Get a new Bean instance given its name
     */
    public Object getInstance(String name) {
	try {
	    return Beans.instantiate(classLoader, name);
        } catch (Throwable th) {
	    diagnoseInstantiationException(classLoader, name, th);
	    System.err.println("");
	    if (name.indexOf('\\') >= 0) {
		System.err.println("    Note that file names in manifests must use forward "
		   + "slashes \"/\" \n    rather than back-slashes \"\\\"");
	    }
	    return null;
	}
    }

    /**
     * In order to make life easier for beans developers we try to
     * provide detailed diagnostics on any failure in Beans.instantiate
     */
    void diagnoseInstantiationException(SimpleClassLoader cl, String beanName, Throwable realx) {
	System.err.print("\nWARNING: Could not instantiate bean \"" + beanName + "\"");

	if (cl == null) {
	    // This should never happen with user-defined beans.
	    System.err.println(" from the system class-loader");
	    return;
	}

	System.err.println(" from JAR \"" + jarName + "\"");
	
	// Try to find a serialized object with this name
	String serName = beanName.replace('.','/').concat(".ser");
	java.io.InputStream ins = cl.getResourceAsStream(serName);

	if (ins != null) {	
	    System.err.println("    We found a serialized template file \"" + serName + "\"");
	    java.io.ObjectInputStream oins;
	    try {
		oins = new ObjectInputStreamLoader(ins, cl);
	        Object result = oins.readObject();
	    	System.err.println("    An object could be read from the serialized template OK.");
		System.err.println("    But an exception was generated in Beans.instantiate:");
		System.err.println("        " + realx);
	    } catch (java.io.ObjectStreamException ex) {
	    	System.err.println("    But caught an ObjectStreamException while reading the serialized object:");
		System.err.println("        " + ex);
		System.err.println("    This indicates there is a problem with the contents of the template file.");
	    } catch (java.io.IOException ex) {
	    	System.err.println("    But caught an IOException while reading the serialized object:");
		System.err.println("        " + ex);
	    } catch (ClassNotFoundException ex) {
	    	System.err.println("    But caught a ClassNotFoundException while reading the serialized object:");
		System.err.println("        " + ex);
		System.err.println("    This indicates that there was a problem finding a .class file for one");
		System.err.println("    of the serialized objects");
	    } catch (Throwable th) {
	    	System.err.println("    But caught an unexpected exception while reading the serialized object:");
		System.err.println("        " + th);
	    }
	    try {
	        ins.close();
	    } catch (Exception ex) {
	    }
	    return;
	}

	// Try to find a .class file with this name.
	try {
	    String classFileName = beanName.replace('.','/').concat(".class");
	    ins  = cl.getResourceAsStream(classFileName);

	    if (ins == null) {
	        System.err.println("    We couldn't open the class file \"" 
			+ classFileName + "\" in the JAR");
	        return;
	    }

	    // OK. the class file is there.
	    System.err.println("    We found the class file \"" + classFileName + "\"");

	} catch (SecurityException ex) {
	    // Ignore any securityExceptions - they are caused by some JDK versions
	    // being over-paranoid about access to .class files, and aren't
	    // relevant to this analysis.
   	}


	// Figure out the package and class names.
	String pkg= "the default package";
	String className = beanName;
	if (beanName.lastIndexOf('.') > 0) {
	    pkg = "the package \"" + beanName.substring(0, beanName.lastIndexOf('.')) + "\"";
	    className = beanName.substring( beanName.lastIndexOf('.')+1);
	}

	Class cls;
	try {
	    cls = cl.loadClass(beanName);
	} catch (Exception ex) {
	    System.err.println("    But were unable to load the class \"" + beanName + "\" because of");
	    System.err.println("        " + ex);
	    System.err.println("    Common reasons for this failure include:");
	    System.err.println("    (1) The class is not defined in the correct package");
	    System.err.println("        it should be in " + pkg);
	    System.err.println("    (2) The class has not been given the correct name");
	    System.err.println("    it should be called \"" + className + "\"");
	    System.err.println("    (3) The class file contains the wrong class or no class at all");
	    return;
	} catch (Throwable th) {
	    System.err.println("    But were unable to load the class \"" + beanName + "\" because of");
	    System.err.println("        " + th);
	    if (th instanceof ClassFormatError && th.getMessage().equals("Duplicate name")) {
		System.err.println("    This particular error is often caused by having a mismatch between the name of");
		System.err.println("    the .class file and the name of the contained class.");
		System.err.println("    In this case make sure that class file contains a class");
		System.err.println("    called \"" + className + "\" in " + pkg + ".");
	    } else {
	       th.printStackTrace();
	    }
	    return;
	}

	// And we were able to create the class.
	System.err.println("    We located the class \"" + beanName + "\" OK");
	int mods = cls.getModifiers();
	if (!java.lang.reflect.Modifier.isPublic(mods)) {
	    System.err.println("    But the class was not declared public, so we could not create a bean");
	    return;
	}

	// Check for a valid public zero-arg constructor.
	try {
	    Class args[] = { };
	    java.lang.reflect.Constructor cons = cls.getConstructor(args);
	    if (cons == null) {
	        System.err.println("    But the class did not have a zero-arg constructor.");
	        System.err.println("    All beans must provide public zero-arg constructors.");
		return;
	    }
	    mods = cons.getModifiers();
	    if (!java.lang.reflect.Modifier.isPublic(mods)) {
	        System.err.println("    But the class's zero-arg constructor was not declared public");
	        System.err.println("    All beans must provide public zero-arg constructors.");
	        return;
	    }
	} catch (NoSuchMethodException ex) {
	    System.err.println("    But the class did not have a zero-arg constructor.");
	    System.err.println("    All beans must provide public zero-arg constructors.");
	    return;
	} catch (Throwable th) {
	    System.err.println("    Unexpected exception in disgnoseInstantiationException");
	    System.err.println("    " + th);
	    th.printStackTrace();
	    return;
	}

	System.err.println("    The class provides a public zero-arg constructor");
	try {
	    Object obj = cls.newInstance();
	} catch (Throwable th) {
	    System.err.println("    But were unable to create an instance of the class because we");
	    System.err.println("    got an exception while doing Class.newInstance() :" );
	    System.err.println("       " + th);
	    System.err.println("    The stack backtrace at the time of this exception is");
	    th.printStackTrace();
	    return;
	}

	// Now we have no ideas left on why Beans.instantiate would fail.
	System.err.println("    But an exception was generated in Beans.instantiate:");
	System.err.println("        " + realx);
	realx.printStackTrace();
    }


    private int indexForName(String name) {
	for (int i=0; i<beanNames.length ; i++) {
	    if (beanNames[i].equals(name)) {
		return i;
	    }
	}
	return -1;
    }


    // Debugging stuff

    private static boolean debug = false;
    private static void debug(String msg) {
	if (debug) {
	    System.err.println("JarInfo:: "+msg);
	}
    }

}
