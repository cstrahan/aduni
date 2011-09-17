package sun.beanbox;

import java.io.*;
import java.util.*;
import java.beans.BeanInfo;
import java.beans.BeanDescriptor;
import java.beans.Introspector;
import java.beans.PropertyEditor;
import java.beans.PropertyEditorManager;
import java.beans.PropertyDescriptor;
import java.awt.*;
import java.lang.reflect.Method;

/**
 * Generate a AppletClass for an Applet.
 */
public class AppletClassGenerator {

    /**
     * Write Java statements to reinitialize a beans properties
     * to the indentedStream.
     */ 
     void generateBeanInitialization(Wrapper beanWrapper,
				    IndentedStream indentedStream) {
	BeanPropertyList pl = new BeanPropertyList(beanWrapper);
		
	Wrapper topBeanBoxWraper = beanBox.getTopWrapper();
		
	if (topBeanBoxWraper != null
		&& (topBeanBoxWraper == beanWrapper
		    || topBeanBoxWraper.equals(beanWrapper))) {
	    // generates source code to init a bean using *this* identifier
	    pl.generateInitializationCodeForTopBean(indentedStream);
	} else {
	    // generates source code to init a bean using
	    // a unqiue identifier for nested bean
	    pl.generateInitializationCodeForNestedBean(indentedStream); 
	}
    }
    
    /**
     * A generator for AppletClasses.
     * Box is the beanbox
     * Name is the AppletClass name
     * Dir is the directory where to place the generated .java and .class files
     */
    public AppletClassGenerator(BeanBox box) {
	if (names == null) {
	    names = new Hashtable();
	}
	
	hiddenStateBeans = new Vector();
   
	beanBox = box;
	beanCount = beanBox.getComponentCount();
	for (int i=0; i<beanCount; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    hookupCount += w.getEventHookupCount();
	}
	colorEditor = PropertyEditorManager.findEditor(Color.class);
	fontEditor = PropertyEditorManager.findEditor(Font.class);
	stringEditor = PropertyEditorManager.findEditor(String.class);
	
	findBeansWithHiddenState();
	
    }

    /**
     * Generate a AppletClass for the given BeanBox;
     * Includes compiling it
     *
     * TBD; classes should not be in the default package
     * we should pass around the package name.
     */
    public boolean generateClass(String appletName, File dir, String[] jars) {
	String fileName = generateJavaFile(appletName, dir);

	// create the desired classpath
	StringBuffer classpath = new StringBuffer(dir.getPath());
	for (int i=0; i<jars.length; i++) {
	    classpath.append(File.pathSeparator);
	    classpath.append(jars[i]);
	}
	classpath.append(File.pathSeparator);
	classpath.append(System.getProperty("java.class.path"));
	
	return ClassCompiler.compile(fileName, classpath.toString());
    }

    /**
     * Write the .java file
     */
    private String generateJavaFile(String appletName, File dir) {
	File f = new File(dir, appletName + ".java");
	try {
	    p = new IndentedStream(new PrintWriter(new FileOutputStream(f)));
	} catch (Exception ex) {
	    System.err.println("could not create output stream for codegen; "+ex);
	    System.exit(1);
	}

	p.pp("import java.util.Hashtable;");
	p.pp("import java.io.Serializable;");
	p.pp("import java.io.InputStream;");
	p.pp("import java.io.OutputStream;");
	p.pp("import java.io.ObjectOutputStream;");
	p.pp("import java.io.ObjectInputStream;");
	p.pp("import java.awt.Component;");
	p.pp("import java.awt.Rectangle;");
	p.pp("import java.awt.Dimension;");
	p.pp("import java.applet.Applet;");
	p.pp("import java.lang.reflect.Method;");
	p.pp("import java.beans.Beans;");
	p.pp("import sunw.beanbox.AppletSupport;");
	p.pp("import sunw.beanbox.PropertyHookup;");

	p.pp("");
	p.pp("public class "+appletName+" extends Applet implements Serializable {");
	p.i();

	p.pp();
	p.pp("// Public 0-argument constructor");
	p.pp();

	p.pp("public "+appletName+"() {");
	
	if (shouldSerializeHiddenStateBeans()) {
	    p.ip("InputStream is = this.getClass().getResourceAsStream(\""+
		 appletName+"Data\");");
	    p.pp("if (is == null) {");
	    p.ip("System.err.println(\"Could not locate the Applet data at "+
		 appletName+"Data\");");
	    p.pp("throw new Error(\"Could not locate the Applet data at "+
		 appletName+"Data\");");
	    p.op("}");
	    p.pp("this.setLayout(null);");
	    p.pp("try {");
	    p.ip("ObjectInputStream ois = new ObjectInputStream(is);");
	    p.pp("initContentsFromStream(ois);");
	    p.op("} catch (Exception ex) {");
	    p.ip("System.err.println(\"trouble reading from serialized data: \"+ex);");
	    p.pp("throw new Error(\"trouble reading from serialized data: \"+ex);");
	    p.op("}");
	} else {
	    p.ip("this.setLayout(null);");
	    p.pp("try {");
	    p.ip("initContents();");
	    p.op("} catch (Exception ex) {");
	    p.ip("System.err.println(ex);");
	    p.pp("throw new Error(\"trouble initializing contents: \"+ex);");
	    p.op("}");
	}
	p.op("}");

	p.pp();
	p.pp("// Preferred size");
	p.pp("public Dimension getPreferredSize() {");
	p.ip("return getMinimumSize();");
	p.op("}");

	p.pp();
	p.pp("// Preferred size");
	p.pp("public Dimension getMinimumSize() {");
	Rectangle r = beanBox.getBounds();
	p.ip("return new Dimension("+r.width+", "+r.height+");");
	p.op("}");

	if (shouldSerializeHiddenStateBeans()) {
	    p.pp();
	    generateInitContentsFromStream(appletName);
	}
	
	p.pp();
	generateInitCode(appletName);

	p.pp();
	generateAddConnections();

	p.pp();
	generateReconnections();

	p.pp();
	generateSerializationCode(appletName);

	// based on this BeanBox
	p.pp();
	p.pp("// The fields used to hold the beans");
	for (int i=0; i<beanCount; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    Object bean = w.getBean();
	    p.pp("private "
	                +bean.getClass().getName()
	                +" "
	                +uniqueName(bean)
	                +";"
	                );
	}

	p.pp();
	p.pp("// The hookups");
	int count = 0;
	boolean foundMethodAdaptor = false;
	boolean foundPropertyAdaptor = false;

	for (int i=0; i<beanCount; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    WrapperEventInfo[] wei = w.getEventHookupInfo();

	    for (int j=0; j<wei.length; j++) {
		if (wei[j] instanceof WrapperPropertyEventInfo) {
		    // This is a (bound) property hookup
		    p.pp("private PropertyHookup hookup"+count+";");
		    foundPropertyAdaptor = true;
		} else {
		    // This is a method adaptor -- generate in-line
		    p.pp("private "+wei[j].getAdaptorClassName()+" hookup"+count+";");
		    foundMethodAdaptor = true;
		}
		count += 1;
	    }	    
	}
	if (! foundMethodAdaptor) {
	    p.pp("//   No method adaptors.  A typical example is:");
	    p.pp("//     private MyActionAdaptor hookup1;");
	}
	if (! foundPropertyAdaptor) {
	    p.pp("//   No property adaptors.  A typical example is:");
	    p.pp("//     private PropertyHookup hookup1;");
	}

    	p.pp();
	p.pp("// the loader so we can locate the resource file");
	p.pp("private ClassLoader myLoader;");

	p.pp();
	p.pp("// =========================================== ");
	p.pp("// Support code");
	p.pp("// =========================================== ");
	p.pp("// It really belongs in support.jar but it is here for your reading pleasure");
	p.pp();
	generateSupportCode();
	p.pp("// =========================================== ");
	p.pp("// End of Support code");
	p.pp("// =========================================== ");

	// All Done...
	p.op("}");
	p.close();
	
	return f.getPath();
    }

    private Rectangle getBeanBounds(Wrapper w) {
	Object bean = w.getBean();

	if (bean instanceof Component) {
	    Rectangle wr = w.getBounds();
	    Rectangle br = ((Component) bean).getBounds();
	    return new Rectangle(wr.x+br.x, wr.y+br.y,
				   br.width, br.height);
	} else {
	    return null;
	}
    }

    private void readIntoField(Object bean, int ix) {
        Class cl = bean.getClass();
	String clname = cl.getName();
	String name = uniqueName(bean);
        p.pp(name+" = ("+clname+") data["+ix+"];");
    }

    private void positionWithinField(Object bean, Rectangle bounds) {
	String name = uniqueName(bean);
	if (bounds != null) {
	    p.pp("acquire("+name+", "+
		 "new Rectangle("+bounds.x+", "+bounds.y+", "+
		 bounds.width+", "+bounds.height+"));");
	} else {
	    p.pp("acquire("+name+", null);");
	}
    }

    private void generateAddConnections() {
	int count = 0;
	boolean foundMethodAdaptor = false;
	boolean foundPropertyAdaptor = false;

	p.pp("private void addConnections() {");
	p.ip("try {");
	p.i();
	for (int i=0; i<beanCount; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    Object bean = w.getBean();
	    WrapperEventInfo[] wei = w.getEventHookupInfo();

	    for (int j=0; j<wei.length; j++) {
		if (wei[j] instanceof WrapperPropertyEventInfo) {
		    // This is a (bound) property hookup
		    WrapperPropertyEventInfo wpei = (WrapperPropertyEventInfo) wei[j];		    foundPropertyAdaptor = true;

		    p.pp("hookup"+count+" = addPropertyTarget(");
		    p.ip(uniqueName(bean)+", ");
		    p.pp("\""+wpei.getPropertyName()+"\",");

		    p.pp(uniqueName(wpei.getTargetBean())+",");
		    p.pp("\""+wpei.getSetterName()+"\", "+
			 wpei.getSetterTypes()+");");
		    p.o();
		} else {
		    // This is a method adaptor -- generate in-line
		    foundMethodAdaptor = true;

		    p.pp("hookup"+count+" = new "+
			 wei[j].getAdaptorClassName()+"();");
		    p.pp("hookup"+count+".setTarget("+
			 uniqueName(wei[j].getTargetBean())+");");
		    p.pp(uniqueName(bean)+"."+w.getAdderName(wei[j].getEventSetName())+"("+
			 "hookup"+count+");");
		}
		count += 1;
	    }	    
	    p.pp("//");
	}
	if (! foundPropertyAdaptor) {
	    p.pp("//   No property adaptors.  A typical example is:");
	    p.pp("//     hookup0 = addPropertyTarget(");
	    p.pp("//       ourButton1,");
	    p.pp("//       \"foreground\",");
	    p.pp("//       \"background\",");
	    p.pp("//       \"setBackground\", new String[] {\"java.awt.Font\"});");
	}
	if (! foundMethodAdaptor) {
	    p.pp("//   No method adaptors.  A typical example is:");
	    p.pp("//     hookup0 = new MyActionAdaptor();");
	    p.pp("//     ourButton1.addActionListener(hookup0);");
	}

	p.op("} catch (Exception ex) {");
	p.ip("System.err.println(\"Problems adding a target: \"+ex);");
	p.pp("ex.printStackTrace();");
	p.op("}");
	p.op("}");
	
    }

    private void generateReconnections() {
	int count = 0;
	boolean foundMethodAdaptor = false;
	boolean foundPropertyAdaptor = false;

	p.pp("private void addReconnections() {");
	p.ip("try {");
	p.i();
	for (int i=0; i<beanCount; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    Object bean = w.getBean();
	    WrapperEventInfo[] wei = w.getEventHookupInfo();

	    for (int j=0; j<wei.length; j++) {
		if ((wei[j].getTargetBean() instanceof Externalizable) == false) {
			count +=1;
		        continue;   // don't attempt to reconnect unless the target
				    // is Externalizable
		        
		        // After we are serialized in via readObject() all
		        // of our connections are pre-existing, thanks to the Object
		        // Serialization machinery, with the exception of
		        // references to Externalizable targets.
		        // References to Externalizable objects are not handled
		        // by Object Serialization and must be manually
		        // reconstructed.     
		}
		
		if (wei[j] instanceof WrapperPropertyEventInfo) {
		    // This is a (bound) property hookup
		    WrapperPropertyEventInfo wpei = (WrapperPropertyEventInfo) wei[j];		    foundPropertyAdaptor = true;
		    
		    p.pp("hookup"+count+" = addPropertyTarget(");
		    p.ip(uniqueName(bean)+", ");
		    p.pp("\""+wpei.getPropertyName()+"\",");

		    p.pp(uniqueName(wpei.getTargetBean())+",");
		    p.pp("\""+wpei.getSetterName()+"\", "+
			 wpei.getSetterTypes()+");");
		    p.o();
		} else {
		    // This is a method adaptor -- generate in-line
		    foundMethodAdaptor = true;

		    p.pp("hookup"+count+" = new "+
			 wei[j].getAdaptorClassName()+"();");
		    p.pp("hookup"+count+".setTarget("+
			 uniqueName(wei[j].getTargetBean())+");");
		    p.pp(uniqueName(bean)+"."+w.getAdderName(wei[j].getEventSetName())+"("+
			 "hookup"+count+");");
		}
		count += 1;
	    }	    
	    p.pp("//");
	}
	if (! foundPropertyAdaptor) {
	    p.pp("//   No property adaptors.  A typical example is:");
	    p.pp("//     hookup0 = addPropertyTarget(");
	    p.pp("//       ourButton1,");
	    p.pp("//       \"foreground\",");
	    p.pp("//       \"background\",");
	    p.pp("//       \"setBackground\", new String[] {\"java.awt.Font\"});");
	}
	if (! foundMethodAdaptor) {
	    p.pp("//   No method adaptors.  A typical example is:");
	    p.pp("//     hookup0 = new MyActionAdaptor();");
	    p.pp("//     ourButton1.addActionListener(hookup0);");
	}

	p.op("} catch (Exception ex) {");
	p.ip("System.err.println(\"Problems adding a target: \"+ex);");
	p.pp("ex.printStackTrace();");
	p.op("}");
	p.op("}");
	
    }
    
    private void generateSupportCode() {
	p.pp("// Acquire a bean");
	p.pp("private void acquire(Object bean, Rectangle boundsData) {");
	p.ip("if (!(bean instanceof Component)) {");
	p.ip("return;");
	p.op("}");
	p.pp("if (bean instanceof Applet) {");
        p.ip("AppletSupport.assignStub((Applet) bean,");
	p.ip("myLoader,");
	p.op("bean.getClass());");
	p.op("}");
	p.pp();
	p.pp("add((Component) bean);");
	p.pp("if (boundsData != null) {");
	p.ip("((Component) bean).setBounds(boundsData);");
	p.op("}");
	p.pp("((Component)bean).invalidate();	// not needed?");
	p.pp();
	p.pp("if (bean instanceof Applet) {");
	p.ip("// Start the Applet");
	p.pp("((Applet)bean).start();");
	p.op("}");
	p.op("}");

	p.pp();
	p.pp("// Add a property bound via an adaptor");
	p.pp("Hashtable propInstances = new Hashtable();");
	p.pp();
	p.pp("private PropertyHookup addPropertyTarget(Object source,");
	p.ip("String propertyName,");
	p.pp("Object targetObject,");
	p.pp("String setterName, String[] setterTypeNames) throws Exception");
	p.op("{");
	p.ip("Object args[] = new Object[1]; // arguments");
	p.pp("Class types[] = new Class[1]; // types");
	p.pp("Class pClass = java.beans.PropertyChangeListener.class;");
	p.pp();
	p.pp("PropertyHookup hook = (PropertyHookup) propInstances.get(source);");
	p.pp("if (hook == null) {");
	p.ip("// This is the first property hookup on this source object");
	p.pp("// Push a PropertyHookup adaptor onto the source");
	p.pp("hook = new PropertyHookup(source);");
	p.pp("propInstances.put(source, hook);");
	p.pp();
	p.pp("// find the adder");
	p.pp("types[0] = pClass;");
	p.pp("Method adder = source.getClass().getMethod(\"addPropertyChangeListener\", types);");
	p.pp("// invoke the adder");
	p.pp("args[0] = hook;");
	p.pp("adder.invoke(source, args);");
	p.op("}");
	p.pp();
	p.pp("// get setter");
	p.pp("Method setter = targetObject.getClass().getMethod(setterName,");
	p.ip("getClassFromTypes(setterTypeNames));");
	p.op();
	p.pp("hook.attach(propertyName, targetObject, setter);");
	p.pp("return hook;");
	p.op("}");

	p.pp();
	p.pp("private Class[] getClassFromTypes(String types[]) throws Exception {");
	p.ip("Class[] back = new Class[types.length];");
	p.pp("for (int i=0; i<back.length; i++) {");
	p.ip("Class c = unwrapPrimitiveStringToClass(types[i]);");
	p.pp();
	p.pp("if (c == null)");
	p.ip("back[i] = myLoader.loadClass(types[i]);");
	p.op("else");
	p.ip("back[i] = c;");
	p.o();
	p.op("}");
	p.pp("return back;");
	p.op("}");
	
	p.pp("private Class unwrapPrimitiveStringToClass(String s) {");
	p.ip("if (s.equals(Byte.TYPE.getName()))        return byte.class;");
	p.pp("if (s.equals(Short.TYPE.getName()))       return short.class;");
	p.pp("if (s.equals(Integer.TYPE.getName()))     return int.class;");
	p.pp("if (s.equals(Long.TYPE.getName()))        return long.class;");
	p.pp("if (s.equals(Double.TYPE.getName()))      return double.class;");
	p.pp("if (s.equals(Float.TYPE.getName()))       return float.class;");
	p.pp("if (s.equals(Character.TYPE.getName()))   return char.class;");
	p.pp("if (s.equals(Boolean.TYPE.getName()))     return boolean.class;");
	p.pp("if (s.equals(Void.TYPE.getName()))        return void.class;");
	p.pp("return null;");
	p.op("}");
    }
   
    private void generateInitContentsFromStream(String appletName) {
 	p.pp("// Read hidden-state beans from stream");
	p.pp("private void initContentsFromStream(java.io.ObjectInputStream ois)");
        p.ip("throws java.lang.ClassNotFoundException,");
        p.pp("java.io.IOException");
	p.op("{");
	p.ip("Object[] data = (Object[]) ois.readObject();");
	p.pp();
	p.pp("String id = (String) data[0];");;
	p.pp("if (! id.equals(\""+appletName+"\")) {");
	p.ip("throw new Error(\"Wrong data!\");");
	p.op("}");
	
	// generate based on this BeanBox...
	p.pp();
	p.pp("// Get references to hidden-state beans");
	int serializedBeanIndex = 1; // index zero is reserved for applet name
	for (int i=0; i<beanCount; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    Object bean = w.getBean();
	    if (this.beanHasHiddenState(bean)) 		
	        readIntoField(bean, serializedBeanIndex++);
	} 
	p.pp();
	p.pp("// Initialize the remainder of the applets contents");
	p.pp("// including acquiring its nested beans and reconnecting hookups.");
	p.pp("initContents();");
	p.op("}");
	p.pp();
    }
   
    private void generateInitCode(String appletName) {
	p.pp("// Initialize nested beans");
	p.pp("private void initContents()");
	p.i();
        p.ip("throws java.lang.ClassNotFoundException,");
        p.pp("java.io.IOException");
        p.o();
	p.op("{");
	p.ip("myLoader = this.getClass().getClassLoader();");
	p.pp("propInstances = new Hashtable();");

	p.pp();
	generateBeanInitialization(beanBox.getTopWrapper(), p);

	// generate based on this BeanBox...
	p.pp();
	p.pp("// Create nested beans");
	for (int i=0; i<beanCount; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    Object bean = w.getBean();
	    // only instantiate non-hidden-state beans
	    if (this.beanHasHiddenState(bean) == false) 		
	    {
	        p.pp(   uniqueName(bean)
			+ " = (" 
			+ bean.getClass().getName() 
			+ ") Beans.instantiate(myLoader,\""
			+ bean.getClass().getName()
			+ "\");"
		);
		
	    generateBeanInitialization(w, p);
	    p.pp();
	    }
	} 
	p.pp("// position all nested beans - we don't have it initially");
	for (int i=0; i<beanCount; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    Object bean = w.getBean();
	    positionWithinField(bean, getBeanBounds(w));
	}

	p.pp();
	p.pp("// Add their connections");
	p.pp("addConnections();");
	p.op("}");
    }

    private void generateSerializationCode(String appletName) {
	p.pp("// Serialization code - readObject");
        p.pp("private void readObject(java.io.ObjectInputStream ois)");
        p.ip("throws java.lang.ClassNotFoundException,");
        p.pp("java.io.IOException");
	p.op("{");
	p.i();

	p.pp("// Initialize object from stream");
	p.pp("Object[] data = (Object[]) ois.readObject();");

	p.pp();
	p.pp("myLoader = this.getClass().getClassLoader();");
	p.pp("propInstances = new Hashtable();");

	p.pp();
	p.pp("String id = (String) data[0];");;
	p.pp("if (! id.equals(\""+appletName+"\")) {");
	p.ip("throw new Error(\"Wrong data!\");");
	p.op("}");
	
	// generate based on this BeanBox...
	p.pp();
	p.pp("// Get references to nested beans");
	for (int i=0; i<beanCount; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    Object bean = w.getBean();
	    readIntoField(bean, i+1);
	} 
	p.pp("");
	p.pp("// Don't position nested beans");
	for (int i=0; i<beanCount; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    Object bean = w.getBean();
	    positionWithinField(bean, null);
	}

	p.pp();
	p.pp("// Reconnect their connections");
	p.pp("addReconnections();");
	p.op("}");
	p.pp();

	p.pp("// Serialization code - writeObject");
        p.pp("private void writeObject(java.io.ObjectOutputStream oos)");
	p.ip("throws java.io.IOException");
        p.op("{");
	p.i();

	p.pp("Object data[] = new Object["+(beanCount+1)+"];");
	p.pp("data[0] = \""+appletName+"\";");
	// generate based on this BeanBox...
	for (int i=0; i<beanCount; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    Object bean = w.getBean();
	    String name = uniqueName(bean);
            p.pp("data["+(i+1)+"] = "+name+";");
	} 

	p.pp();
	p.pp("// Write the object out");
	p.pp("oos.writeObject(data);");
	p.op("}");
	p.pp();
	}

    /**
     *  manage unique names
     */
    // Made public static so it could be directly accessed by
    // BeanProperty.getJavaInitializationStatement()
    public static String uniqueName(Object bean) {
	String back = (String) names.get(bean);
	if (back != null) {
	    return back; // the name we last assigned it
	}
	String klassName = bean.getClass().getName();
	int ix = klassName.lastIndexOf('.');
	if (ix >= 0) {
	    klassName = klassName.substring(ix+1);
	}
	// decapitalize
	klassName = Introspector.decapitalize(klassName);
	// note that different classes may have the same short name
	Integer klassCount = (Integer) names.get(klassName);

	int count = 0; // last count for this class name
	if (klassCount != null) {
	    count = klassCount.intValue();
	}
	count += 1;
	back = klassName+count;
	names.put(klassName, new Integer(count));
	names.put(bean, back);
        return back;
    }
    
    /**
     * Returns true if bean has hidden state.
     */
    protected boolean beanHasHiddenState(Object bean)
    {
	return hiddenStateBeans.contains(bean);
    }
     
    /**
     * Returns true if beans containing hidden-state are nested in
     * this beanbox.
     */
    protected boolean shouldSerializeHiddenStateBeans()
    {
	return (hiddenStateBeans != null && hiddenStateBeans.size() > 0)
			? true : false;
    }
    
    /**
     * Initializes a private collection of beans that contain hidden-state,
     * if any.
     */ 
     protected void findBeansWithHiddenState() {
	for (int i=0; i<beanCount; i++) {
	    Wrapper w = (Wrapper) beanBox.getComponent(i);
	    Object bean = w.getBean();
	    	
	    Class beanClass = bean.getClass();
	    try {
		BeanInfo bi = Introspector.getBeanInfo(beanClass);
		BeanDescriptor bd = bi.getBeanDescriptor();

		boolean beanMustBeSerialized = true;
		Boolean hiddenStateProperty =
                        (Boolean)bd.getValue("hidden-state");
		
		if (hiddenStateProperty != null && 
		    (hiddenStateProperty.booleanValue() == true)) {
		    beanMustBeSerialized = true;
		} else {
		    beanMustBeSerialized = false;
		}

		if (beanMustBeSerialized) {
		    hiddenStateBeans.addElement(bean);
		}
	    } catch (Exception e) {
		System.err.println(e);
	    }
	}
    }
    
    // Private fields for Class generation
    
    // Vector of beans with hidden state.
    private Vector hiddenStateBeans;
    private static Hashtable names = new Hashtable(); // unique names
    private BeanBox beanBox;	// source for what we want to pack
    private int hookupCount = 0;	// how many hookups
    private int beanCount = 0;	// how many beans
    private static File dir;	// where to generate before packing it up
    private PropertyEditor colorEditor;	// the PropertyEditor for color
    private PropertyEditor stringEditor;	// the PropertyEditor for string
    private PropertyEditor fontEditor;	// the PropertyEditor for font

    private IndentedStream p = null; // the stream for the code
}

// Abstracts a bean property
class BeanProperty {

    BeanProperty(Object bean, PropertyDescriptor pd) {
	this.pd = pd;
	this.bean = bean;
    
	readMethod = pd.getReadMethod();	// getter
	writeMethod = pd.getWriteMethod();	// setter
	propertyType = pd.getPropertyType();

	if (propertyType != null) {
	    propertyEditor = PropertyEditorManager.findEditor(propertyType);
	} else {
	    // property type may be an indexed type w/out indexed accessors.
	}
	this.propertyHasChanged = false;
    }
	
    boolean isAccessibleToSourceCodeGeneration()
    {
	return (readMethod != null
	        && writeMethod != null
	        && propertyEditor != null
		&& propertyHasChanged == true);
    }
	
    private boolean writeMethodThrowsCheckedExceptions()
    {
	if (writeMethod == null) {
	    return false;
	}
		
	Class[] exceptionTypes = writeMethod.getExceptionTypes();
	return (exceptionTypes.length == 0) ? false : true;
    }
	
    /*
     * returns a java source statement capable of initializing
     * this property's value
     */
    String getJavaInitializationStatement(boolean use_this_identifier)
    {
	if (isAccessibleToSourceCodeGeneration() == false) {
	    return null;
	}

	StringBuffer s = new StringBuffer();

	try {
	    propertyValue = readMethod.invoke(bean,null);
	    
	    if ((propertyValue == null) |
		((propertyValue instanceof String) && propertyValue == "null")) {
		return null; // don't set null values in sources...
	    }

	    propertyEditor.setValue(propertyValue);

	    if (use_this_identifier == true) {
		s.append("this");
	    } else {
		s.append(AppletClassGenerator.uniqueName(bean));
	    }

	    s.append(".");
	    s.append(writeMethod.getName());
	    s.append("(");
	    s.append(propertyEditor.getJavaInitializationString());	
	    s.append(");");
			
	    if (writeMethodThrowsCheckedExceptions()) {
		s.insert(0, "try { ");
		
		String exceptionId =
		    AppletClassGenerator.uniqueName(new Exception());
		s.append(" } catch (Exception " + exceptionId
			 + ") { System.err.println(" + exceptionId
			 + "); }");
	    }
	} catch (Exception e) {
	    System.err.println(e);
	}
	
	return s.toString();
    }
	
    public void setChanged()
    {
	propertyHasChanged = true;
	// System.err.println(pd.getName() + " FOR " + bean + " IS MODIFIED");
    }
	
    private boolean             propertyHasChanged;  //true if property has changed
    private PropertyDescriptor  pd;
    private Method              readMethod;     // getter
    private Method              writeMethod;    // setter
    private Class               propertyType;
    private PropertyEditor      propertyEditor; // editor for property type
    private Object              propertyValue;
    private Object              bean;   // bean that owns this property
}


// A list of bean properties
class BeanPropertyList {
    BeanPropertyList(Wrapper beanWrapper) {
	Object  bean = beanWrapper.getBean();
	Vector  changedProperties = beanWrapper.getChangedProperties();
	boolean isFromPrototype = beanWrapper.isFromPrototype();		
	beanProps = new Vector();
		
	Class beanClass = bean.getClass();
	try {
	    BeanInfo bi = Introspector.getBeanInfo(beanClass);
	    PropertyDescriptor props[] = bi.getPropertyDescriptors();
	    for (int i = 0; i < props.length; i++) {
		BeanProperty bp = new BeanProperty(bean, props[i]);
		beanProps.addElement(bp);
		
		if (isFromPrototype
		    || (changedProperties != null
		    	&& changedProperties.contains(props[i]))) {
		    bp.setChanged();
		}
	    }
	} catch (Exception e) {
	    System.err.println(e);
	}
    }
	
    void generateInitializationCodeForNestedBean(IndentedStream o) {
	genInitCodeForBean(o,false);
    }
	
    void generateInitializationCodeForTopBean(IndentedStream o) {
	genInitCodeForBean(o,true);
    }
	
    private void genInitCodeForBean(IndentedStream o,
				    boolean useThisIdentifier)
    {
	// This method writes all the code needed to reinitialize a bean.
	// It only calls getJavaInitializationStatement() on properties that 
	// are accessible to source code generation.
		
	for (Enumeration e = beanProps.elements(); e.hasMoreElements(); ) {
	    BeanProperty bp = (BeanProperty)e.nextElement();
	    if (bp.isAccessibleToSourceCodeGeneration()) {
		String s = bp.getJavaInitializationStatement(useThisIdentifier);
		if (s != null) {
		    o.pp(s);
		}
	    }
	}
    }

    // List of BeanProperty instances
    private Vector beanProps;
}

