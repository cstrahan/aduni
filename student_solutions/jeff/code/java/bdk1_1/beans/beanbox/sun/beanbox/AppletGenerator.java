package sun.beanbox;

/**
 * AppletGenerator.java
 *
 * Generates "Composed" Beans and Applets
 *
 * We will populate a directory with several useful files to create
 * and test a new bean.
 *
 * The generated JAR contains a class that explicitly knows what are its
 * top level beans, which are stored in a single serialized stream
 * together with their hookups.
 * This makes it easy to access the classloader for itself,
 * makes it easy to customize by adding properties, methods, etc.
 *
 */

import java.io.*;
import java.util.*;
import java.awt.Rectangle;
import java.awt.Frame;

public class AppletGenerator {
    
    public AppletGenerator(Frame ff,
			   BeanBox box,
			   File adir,
			   String aname,
			   String jname,
			   File tdir) {
        frame = ff;
	beanBox = box;
	appletName = aname;
	jarName = jname;
	appletDir = adir;
	tmpDir = tdir;
	initialize();
    }

    /**
     * Generate a new Applet from the contents of given BeanBox
     * appletName is the name of the Applet
     * appletDir is the directory where to put all the generated files
     * jarName is the name of the JAR where to package things.
     */

    public static void generate(Frame frame,
				BeanBox bb,
				MakeAppDlg dialog,
				String appletDirName,
				String appletName,
				String jarName)
    {
	File tmpdir = new File(appletDirName, jarName+"_files");
	if (! tmpdir.exists()) {
	    tmpdir.mkdirs();
	}

	if (! tmpdir.isDirectory()) {
	    new ErrorDialog(frame, "Could not create directory "+tmpdir);
	    return;
	}

	File beandir = new File(appletDirName);

	AppletGenerator g = new AppletGenerator(frame, bb, beandir, appletName, jarName, tmpdir);

	try {
	    dialog.updateMessage("Copying Jars...");
	    g.copyJars(); 	// copy needed jar files
	    dialog.updateMessage("Copying Hookups...");
	    g.copyHookups();	// copy the hookups (if any)
	    dialog.updateMessage("Writing and Compiling AppletClass...");
	    g.writeAppletClass(); // write and compile the applet class itself
	    dialog.updateMessage("Writing InitializeData...");
	    g.writeInitializeData(); // initialized data
	    dialog.updateMessage("Writing JarFile...");
	    g.writeJarFile();	// create the Jar itself

	    dialog.updateMessage("Writing README file...");
	    g.writeReadme();	// readme documentation
	    dialog.updateMessage("Writing HTML file...");
	    g.writeHtml();	// a test case
	    dialog.updateMessage("Writing Makefiles...");
	    g.writeMakefiles();	// Makefiles for manual regeneration
	    dialog.updateMessage("Done");

        } catch (Exception  ex) {
	    new ErrorDialog(frame, 
			    ex.toString()+"\n"
			    +"while generating data for "+appletName);
	    ex.printStackTrace();
	}
    }


    /**
     * Initialization
     */

    private void initialize() {
	initializeJars();
	initializeHookups();
	String bdkHome = System.getProperty("user.dir");
	supportJar = new File(new File(bdkHome, "lib"), "support.jar");
	appletClassGen = new AppletClassGenerator(beanBox);
    }

    /* compute the jar dependencies */
    private void initializeJars() {
	longJars = new Hashtable();
	int count = beanBox.getComponentCount();
	for (int i=0; i<count; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    String jarName = JarInfo.getJarName(w.getBeanName());
	    String shortName =
		jarName.substring(jarName.lastIndexOf(File.separator)+1);
	    longJars.put(shortName, jarName);
	}
	String back[] = new String[longJars.size()];
	Enumeration e = longJars.keys();
	for (int i = 0; e.hasMoreElements(); i++) {
	    back[i] = (String) e.nextElement();
	}
	shortJars = back;
    }

    /* compute the hookup dependencies */
    private void initializeHookups() {
	Hashtable h = new Hashtable();
	int count = beanBox.getComponentCount();
	for (int i=0; i<count; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    WrapperEventInfo[] wei = w.getEventHookupInfo();
	    for (int j = 0; j < wei.length; j++) {
		String s  = wei[j].getAdaptorClassName();
                if (! s.equals("sunw.beanbox.PropertyHookup")) {
		    h.put(s, s);
		}
	    }
	}
	hookups = new String[h.size()];
	Enumeration e = h.keys();
	for (int i = 0; e.hasMoreElements(); i++) {
	    hookups[i] = (String) e.nextElement();
	}
    }
  
    /*
     * Generate a bean class
     */

    private void writeAppletClass() throws IOException {
	// dependent classes, as full names
	String jarNames[] = new String[longJars.size()];
	Enumeration e = longJars.elements();
	for (int i = 0; e.hasMoreElements(); i++) {
	    jarNames[i] = (String) e.nextElement();
	}
	boolean ok = appletClassGen.generateClass(appletName, tmpDir, jarNames);
	if (!ok) {
	    throw new IOException("compilation failed!");
	}
    }

    /*
     * generate the serialized data that will be read later.
     * this is an serialied array,
     * first the string (classname of generated bean)
     * then serialized data for each of the components in the bean.
     */

    private void writeInitializeData()  throws IOException {    
	if (appletClassGen.shouldSerializeHiddenStateBeans() == false)
    	return; // do nothing if there is nothing to serialize
    
	File f = new File(tmpDir, appletName + "Data");
	ObjectOutputStream oos = null;
	try {
	    oos = new ObjectOutputStream(new FileOutputStream(f));
	} catch (Exception ex) {
	    System.err.println("could not create output stream for serialized data; "+ex);
	    System.exit(1);
	}
	int count = beanBox.getComponentCount();

	// Count the number of beans with hidden-state that must be serialized.
	int hiddenStateCount = 0;
	for (int i=0; i<count; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    Object bean = w.getBean();
	    if (appletClassGen.beanHasHiddenState(bean)) 		
	    	hiddenStateCount++;
	}

	Object[] data = new Object[hiddenStateCount+1];
	
	int serializedBeanIndex = 1; // index zero is reserved for applet name
	// as a simple and early validation
	data[0] = appletName;
	for (int i=0; i<count; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    Object bean = w.getBean();
	    // serialize out only beans with hidden state
	    if (appletClassGen.beanHasHiddenState(bean))
	    	data[serializedBeanIndex++] = bean;
	}
	// REMINDER -- redundant to removeListeners?
	// Remove all listeners (targets and even the Wrapper itself) - if new event model
	for (int i=0; i<count; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    w.removeListeners();
	} 

	oos.writeObject(data); // write the whole array out
	
	// Restore the Wrapper listening to the bean (if new event model)
	for (int i=0; i<count; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    w.attachListeners();
	} 

	oos.close();
    }

    /**
     * Copy here the hookup classes we need
     * We also copy the sources, as documentation
     */
    private void copyHookups() throws IOException {
	String hookups[] = getHookups();
	for (int i=0; i<hookups.length; i++) {
	    // The class files
	    String s = hookups[i].replace('.', File.separatorChar).concat(".class");
	    File src = new File(s);
	    File dst = new File(tmpDir, s);
	    copyFile(src, dst);
	    // The source files
	    s = hookups[i].replace('.', File.separatorChar).concat(".java");
	    src = new File(s);
	    dst = new File(tmpDir, s);
	    copyFile(src, dst);
	}
    }

    /**
     * Copy here the Jars that this bean depends on
     */
    private void copyJars() throws IOException {
	// copy support jar
	copyFile(supportJar, new File(appletDir, "support.jar"));

	// copy other jars
	String jars[] = getShortJars();
	for (int i=0; i<jars.length; i++) {
	    File src = new File(getLongJar(jars[i]));
	    File dst = new File(appletDir, jars[i]);
	    copyFile(src, dst);
	}
    }

    /**
     * Copy a file across
     */
    private boolean copyFile(File src, File dst) throws IOException {
	if (! src.exists()) {
	    new ErrorDialog(frame, "Internal error: "+src+" does not exist");
	    return false;
	}
	File dstDir = new File(dst.getParent());
	if (! dstDir.exists()) {
	    dstDir.mkdirs();
	}
	if (! dstDir.isDirectory()) {
	    new ErrorDialog(frame, "Could not create directory "+dstDir);
	    return false;
	}

	byte buff[] = new byte[1024];
	int length = (int) src.length();

	InputStream is = new FileInputStream(src);
	OutputStream os = new FileOutputStream(dst);
	int nbytes;
	while ((nbytes = is.read(buff)) > 0) {
	    os.write(buff, 0, nbytes);
	}
	is.close();
	os.close();
	return true;
    }


    /**
     * Generate the JAR file.
     * This is equivalent to doing the corresponding make
     */

    private void writeJarFile() throws IOException {
	// There are potentially two non-Hookup entries in the JAR file.
	// The first entry always exists. Its the name of the applet class.
	// The second exists if and only if we are generating serialized applet data.

	int numNonHookupEntries;
	boolean jarAppletData
	  = appletClassGen.shouldSerializeHiddenStateBeans();
	
	if (jarAppletData) {
	  numNonHookupEntries = 2; // classname plus serialized appletData
	} else {
	  numNonHookupEntries = 1; // classname only
	}

	String[] hookups = getHookups();
	String[] files = new String[hookups.length + numNonHookupEntries];
	int entryIndex = 0;
	files[entryIndex] = appletName+".class";
	
	if (jarAppletData) {    // "next" entry is appletData
	  files[++entryIndex] = appletName+"Data";
	}
	
	++entryIndex;    //hookups being at "next" entry
	for (int i=0; i<hookups.length; i++) {
	  files[i + (entryIndex)]
		= hookups[i].replace('.', File.separatorChar).concat(".class");
	}

	OutputStream out
	    = new FileOutputStream(new File(appletDir, jarName+".jar"));
	JarAccess.create(out, tmpDir, files[0], files);
    }

    /**
     * Generate Makefiles so users can do minor tweaks to the generated Applets
     */

    private final static int win32=0;
    private final static int unix=1;
    private final static int other=99;

    private void writeMakefiles() {
	// A makefile may depend on serialized applet data if and only if the 
	// generated applet must serialize one or more of its nested beans.
	// i.e. if some nested beans have hidden state.

	boolean usesSerializedAppletData
	    = appletClassGen.shouldSerializeHiddenStateBeans();

	writeMakefile(win32, "Makefile", usesSerializedAppletData);
	writeMakefile(unix, "GNUmakefile", usesSerializedAppletData);
    }

    /**
     * Write a makefile of a given type, and a given name
     */
    private void writeMakefile(int type,
			       String mname,
			       boolean usesSerializedAppletData) {
	File f;
	char fsep;		// file separator
	char psep;		// path separator
	String copy;		// the copy command

	switch (type) {
	case win32:
	    f = new File(tmpDir, mname);
	    fsep = '\\';
	    psep = ';';
	    copy = "copy";
	    break;
	case unix:
	    f = new File(tmpDir, mname);
	    fsep = '/';
	    psep = ':';
	    copy = "/bin/cp";
	    break;
	default:
	    return;
	}

	try {
	    IndentedStream p
	    	= new IndentedStream(new PrintWriter(new FileOutputStream(f)),
				     "	");
	    p.pp();

	    p.pp("BDKSUPPORTJAR= \\");
	    p.ip(supportJar.getPath());
	    p.op();

	    p.pp("JARS= \\");
	    p.i();
	    p.pp0("$(BDKSUPPORTJAR)");
	    String jars[] = getShortJars();
	    for (int i=0; i<jars.length; i++) {
		p.pn(" \\");
		p.pp0(".."+fsep+jars[i]);
	    }
	    p.pp();
	    p.op();
	    
	    p.pp0("JARPATH=.."+fsep+"support.jar");
	    for (int i=0; i<jars.length; i++) {
		p.pn0(psep+".."+fsep+jars[i]);
	    }
	    p.pp();
	    p.pp();

	    p.pp("JARFILE= \\");
	    p.ip(".."+fsep+jarName+".jar");
	    p.op();

	    String[] hookups = getHookups();
	    p.pp0("HOOKUPS=");
	    p.i();
	    for (int i=0; i<hookups.length; i++) {
		p.pn(" \\");
		p.pp0(hookups[i].replace('.', fsep).concat(".class"));
	    }
	    p.pp();
	    p.op();

	    p.pp("##");
	    p.pp();

	    p.pp("all:	$(JARFILE)");
	    p.pp();

	    // If we are not using serialized applet data, do not generate
	    // dependencies to it in the makefile.
	    String appletData;
	    if (usesSerializedAppletData) {
		appletData = appletName+"Data";
	    } else {
		appletData = "";
	    }
		
	    p.pp("$(JARFILE): "+appletName+".class $(HOOKUPS) "+appletData);	

	    if (type == unix) {
		p.ip("echo \"Name: "+appletName+".class\" >> manifest.tmp");
		p.pp("echo \"Java-Bean: True\" >> manifest.tmp");
		p.pp("jar cfm $(JARFILE) manifest.tmp "+appletName+".class $(HOOKUPS) "+appletData);	
		p.pp("@/bin/rm manifest.tmp");
		p.op();
	    } else if (type == win32) {
		p.ip("jar cfm $(JARFILE) <<manifest.tmp "+appletName+".class $(HOOKUPS) "+appletData);	
		p.op("Name: "+appletName+".class");
		p.pp("Java-Bean: True");
		p.pp("<<");
		p.pp();
	    }

	    p.pp(appletName+".class: "+appletName+".java $(JARS)");
	    if (type == unix) {
		p.ip("export CLASSPATH; CLASSPATH=."+psep+"$(JARPATH)"+psep+"$(CLASSPATH); \\");
	    } else if (type == win32) {
		p.ip("set CLASSPATH=."+psep+"$(JARPATH)"+psep+"%CLASSPATH%");
	    }
	    p.pp("javac "+appletName+".java");
	    p.op();

	    p.pp(".."+fsep+"support.jar: $(BDKSUPPORTJAR)");
	    p.ip(copy+" $(BDKSUPPORTJAR) .."+fsep+"support.jar");
	    p.op();

	    for (int i=0; i<jars.length; i++) {
		String sname = jars[i];
		String lname = getLongJar(sname);
		p.pp(".."+fsep+sname+": "+lname);
		p.ip(copy+" "+lname+" .."+fsep+sname);
		p.op();
	    }

	    p.close();
	} catch (Exception ex) {
	    status = "Trouble generating "+f.getName()+" file; ex: "+ex.toString();
	}
    }

    private void writeReadme() {
	File f = new File(appletDir, jarName+"_readme");
	try {
	    IndentedStream p
	    	= new IndentedStream(new PrintWriter(new FileOutputStream(f)),
				     "	");

	    p.pp("MakeApplet generates an Applet corresponding to a BeanBox layout.  The");
	    p.pp("data and classes needed for this Applet are packaged into a JAR file.");
	    p.pp("The Applet itself is a Bean and it can be read back into the BeanBox");
	    p.pp("if desired.");
	    p.pp("");
	    p.pp("At construction time, the generated Applet uses serialization data only if");
	    p.pp("it contains beans with hidden-state. Note, other builders may use different");
	    p.pp("techniques to reconstitute a compound bean.");
	    p.pp("");
	    p.pp("This directory contains the following files:");
	    p.pp("");
	    p.pp(jarName+".html");
	    p.pp("	A test html file for displaying the applet.  It requires");
	    p.pp("	a JDK1.1-compliant browser.  The APPLET tag references");
	    p.pp("	the JAR files described next");
	    p.pp("");
	    p.pp(jarName+".jar");
	    p.pp("	This JAR file packages .class files and serialized data, if any,");
	    p.pp("	so it can be referenced by the HTML file.  This JAR file");
	    p.pp("	depends on the JAR files listed next.");
	    p.pp("");
	    p.pp("support.jar");
	    p.pp("	This JAR file contains a few support classes that are");
	    p.pp("	used by the generated classes.");
	    p.pp("");
	    String[] jars = getShortJars();
	    for (int i=0; i<jars.length; i++) {
		p.pp(jars[i]);
	    }
	    p.pp("	These JAR file(s) describe the Beans that were instantiated");
	    p.pp("	in the BeanBox and packaged into MyApplet.");
	    p.pp("");
	    p.pp(jarName+"_files");
	    p.pp("	This directory contains source files, data, if any, and Makefiles.");
	    p.pp("	It is possible to do small adjustments to the generated sources");
	    p.pp("	and create a new JAR file.");
	    p.pp("");

	    p.pp("");
	    p.pp("***************");
	    p.pp("");
	    p.pp("BROWSER SUPPORT");
	    p.pp("");
	    p.pp("The generated applet requires full JDK1.1 support.  The appletviewer");
	    p.pp("tool included in the JDK1.1 distributions can be used.  Alternatively");
	    p.pp("you can use the HotJava browser.");
	    p.pp();

	    p.pp("The latest versions of Navigator and Internet Explorer only");
	    p.pp("partially support JDK1.1.");

	    p.pp();
	    p.pp("The failures in IE4.0 include lack of support for the");
	    p.pp("Class.getResource() call.  In particular, this means that the Juggler");
	    p.pp("bean (in juggler.jar) will not be able to show its image.  IE4.0 does");
	    p.pp("support getResourceAsStream() which is used by the generated Applet");
	    p.pp("to acquire serialized information on its beans.  Note that an Image");
	    p.pp("can be created from an InputStream by first creating a byte array");
	    p.pp("and then using Toolkit.createImage().");
	    
	    p.pp();
	    p.pp("Navigator 4.04 generates a security exception from");
	    p.pp("getResourceAsStream() invocations.  We expect this to be resolved");
	    p.pp("promptly");

	    p.pp();
	    p.pp("HotJava 1.1, and 1.1.1 make a distinction in security checks");
	    p.pp("between the URLs \"file:/<dir>\" and \"file:///<dir>\"; an applet");
	    p.pp("denoted by the first one will work but one using the second will not:");
	    p.pp("it will raise a security exception while doing getResourceAsStream()");

	    p.close();
	} catch (Exception ex) {
	    status = "Trouble generating "+f.getName()+" file; ex: "+ex.toString();
	}
    }

    private String[] getShortJars() {
	return shortJars;
    }

    private String getLongJar(String shortJar) {
	return (String) longJars.get(shortJar);
    }

    private String[] getHookups() {
	return hookups;
    }

    private void writeHtml() {
	File f = new File(appletDir, jarName+".html");
	try {
	    IndentedStream p
		= new IndentedStream(new PrintWriter(new FileOutputStream(f)));
	    p.pp("<html>");
	    p.pp("<head>");
	    p.pp("<title>Test page for "+appletName+" as an APPLET</Title>");
	    p.pp("</head>");
	    p.pp("<body>");
	    p.pp("<h1>Test for "+appletName+" as an APPLET</h1>");
	    p.pp("This is an example of the use of the generated");
	    p.pp(appletName+" applet.  Notice the Applet tag requires several");
	    p.pp("archives, one per JAR used in building the Applet");
	    p.pp("<p>");
	    p.pp("<applet");
	    p.ip("archive=\"./"+jarName+".jar,./support.jar");
	    String[] jars = getShortJars();
	    if (jars.length > 0) {
	    	p.i();
	    }
	    for (int i=0; i<jars.length; i++) {
	    	p.pp(",./"+jars[i]);
 	    }
	    p.op("\"");
	    p.pp("code=\""+appletName+"\"");
	    Rectangle bounds = beanBox.getBounds();
	    p.pp("width="+bounds.width);
	    p.pp("height="+bounds.height);
	    p.op(">");
	    p.pp("Trouble instantiating applet "+appletName+"!!");
	    p.pp("</applet>");
	    p.close();
	} catch (Exception ex) {
	    status = "Trouble generating "+f.getName()+" file; ex: "+ex.toString();
	}
    }

    // ===========
    // Private fields
    // ===========

    private AppletClassGenerator appletClassGen; // generates an applet class for a applet

    private File supportJar;	// Support jar

    private Frame frame;	// the frame for presenting messages
    private BeanBox beanBox;	// source for what we want to pack
    private String appletName;	// the name of the generated Applet
    private String jarName;	// the name of the jarFile (no .jar, no "/")
    private File tmpDir;	// where to generate the mess before packing it up
    private File appletDir;	// where to put the generated files
    private String[] shortJars; // short names for jars used in this beanbox
    private Hashtable longJars; // original names, indexed by short names
    private String[] hookups;	// hookups this beanbox depends on

    private String status;      // the status of this generation
}
