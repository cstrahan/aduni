
/**
 * This class manages events hookups between event source beans and 
 * target methods on target beans.
 * It does this for each hookup by generating a .java file for an adaptor
 * class and then compiling and loading the adaptor.
 */

package sun.beanbox;

import java.util.*;
import java.beans.*;
import java.lang.reflect.*;

public class HookupManager {

    static void hookup(EventSetDescriptor esd,
		Method listenerMethod,
		Wrapper sourceWrapper,
		Wrapper targetWrapper,
		Method targetMethod) {

	try {
	    // If we were smarter we might cache and reuse hookups.

	    Object source = sourceWrapper.getBean();
	    Object target = targetWrapper.getBean();
	    String hookupName = generateHookup(esd, listenerMethod, source, target, targetMethod);

	    if (hookupName == null) {
		// The compile failed.
	        System.err.println("Could not create event adaptor.");
		return;
	    }

	    SimpleClassLoader loader = SimpleClassLoader.ourLoader;

	    javaFiles.addElement(hookupName);
 	    Class hookupClass = loader.loadClassFromFile(hookupName);
	    Object hookup = hookupClass.newInstance();

	    Method setTargetMethod = findMethod(hookupClass, "setTarget");   
	    Object args[] = new Object[1];
	    args[0] = target;
	    setTargetMethod.invoke(hookup, args);

	    sourceWrapper.addEventTarget(esd.getName(), targetWrapper, hookup);

	} catch (InvocationTargetException ex) {
	    System.err.println("Hookup caught exception on target: " + ex.getTargetException());
	    ex.getTargetException().printStackTrace();
	} catch (Exception ex) {
	    System.err.println("Hookup caught: " + ex);
	    ex.printStackTrace();
	}

    }

    static String[] getHookupFiles() {
	String back[] = new String[javaFiles.size()];
	Enumeration e = javaFiles.elements();
	for (int i=0; e.hasMoreElements(); i++) {
	    back[i] = (String) e.nextElement();
	}
	return back;
    }

    static String generateHookup(EventSetDescriptor esd, Method listenerMethod, Object source,
		Object target, Method targetMethod) {

	String id = getId();
	String className = "___Hookup_" + id;
	String fileName = tmpDir + java.io.File.separator + className + ".java";
	String targetType = target.getClass().getName();
	Method methods[] = esd.getListenerMethods();

	// Create an appropriate subdirectory.
	java.io.File tmp = new java.io.File(tmpDir);
	tmp.mkdirs();

	// Open the new java source file,
	java.io.PrintWriter out = null;
	try {
	    java.io.FileWriter fout = new java.io.FileWriter(fileName);
	    out = new java.io.PrintWriter(fout);
	} catch (Exception ex) {
	    System.err.println("Couldn't open hookup file " + fileName);
	    System.err.println("   " + ex);
	    return null;
	}

	out.println("// Automatically generated event hookup file.");
	out.println("");
	out.println("package " + packageName + ";");

	// These imports are in case the target or listener types are
	// in the default package.
	out.println("import " + targetType + ";");
	out.println("import " + esd.getListenerType().getName() + ";");

	// Now do any imports on argument types.
	Hashtable argt = new Hashtable();
	argt.put(targetType, argt);
	argt.put(esd.getListenerType().getName(), argt);
	for (int k = 0; k < methods.length; k++) {
	    Class argTypes[] = methods[k].getParameterTypes();
	    for (int i = 0; i < argTypes.length; i++) {
		Class type = argTypes[i];
		while (type.isArray()) {
		    type = type.getComponentType();
		}
		if (type.isPrimitive()) {
		    continue;
		}
		String typeName = type.getName();
		if (argt.get(typeName) == null) {	
		    out.println("import " + typeName + ";");
		    argt.put(typeName, argt);
		}
	    }
	}
	
	
	out.println("");
	out.println("public class " + className + " implements " + 
			esd.getListenerType().getName() + ", java.io.Serializable {");
	out.println("");
	out.println("    public void setTarget(" + targetType + " t) {");
	out.println("        target = t;");
	out.println("    }");

	for (int k = 0; k < methods.length; k++) {
	    out.println("");
	    out.print("    public void " + methods[k].getName() + "(");
	    Class argTypes[] = methods[k].getParameterTypes();
	    for (int i = 0; i < argTypes.length; i++) {
	        if (i > 0) {
		    out.print(", ");
	        }
		// Figure out the string for the argument type.  We have
		// have to treat array types specially.
		Class type = argTypes[i];
		String typeName = "";
		while (type.isArray()) {
		    typeName = "[]" + typeName;
		    type = type.getComponentType();
		}
		typeName = type.getName() + typeName;

	        out.print(typeName + " arg" + i);
	    }
	    out.print(")");

	    Class[] exceptionTypes = methods[k].getExceptionTypes();
	    if (exceptionTypes.length > 0) {
	    out.print("\n         throws");
	    for (int i = 0; i < exceptionTypes.length; i++)
	        out.print(((i != 0) ? ", " : " ") + exceptionTypes[i].getName());
	         out.print(" ");
	    }

	    out.println(" {");
	    if (listenerMethod.getName() == methods[k].getName()) {
	        out.print("        target." + targetMethod.getName() + "(");
		// Either the targetMethod must take zero args, or the
		// same args as the listenerMethod.
		if (targetMethod.getParameterTypes().length != 0) {
	            for (int i = 0; i < argTypes.length; i++) {
	                if (i > 0) {
		            out.print(", ");
	                }
	                out.print("arg" + i);
		    }
	        }
	        out.println(");");
    	    }
	    out.println("    }");
	}
	out.println("");
	out.println("    private " + targetType + " target;");
	out.println("}");
	out.close();

	boolean ok = ClassCompiler.compile(fileName, pathFromLoadedJarFiles());	
	if (!ok) {
	    return null;
	}

	String fullClassName = packageName+"."+className;
	String pathToFile = tmpDir+java.io.File.separatorChar+className+".class";
	return pathToFile;
    }

    /**
     * A classpath corresponding to all the JAR files that have been
     * loaded so far.
     */

    static String pathFromLoadedJarFiles() {
	String path = System.getProperty("java.class.path");
	String sep = java.io.File.pathSeparator;
	Vector allJI = BeanBoxFrame.getToolBox().getLoadedJarInfo();

	StringBuffer allJars = new StringBuffer(path);
	Enumeration e = allJI.elements();
	JarInfo prev = null;	// REMIND!! clean this up
	while (e.hasMoreElements()) {
	    JarInfo ji = (JarInfo) e.nextElement();
	    if (ji == null) {
		// the built-in BeanBox container
		continue;
	    }
	    if (ji == prev) {
		// we already dealt with this one
		continue;
	    }
	    prev = ji;
	    allJars.append(sep);
	    allJars.append(ji.getJarName());
	}
	return allJars.toString();
    }

    static String getId() {
	java.util.Date now = new java.util.Date();
	long id = now.getTime()/10;
	return Long.toHexString(id);
    }

    private static Method findMethod(Class cls, String methodName) {
	try {
            Method methods[] = cls.getMethods();
	    for (int i = 0; i < methods.length; i++) {
	        Method method = methods[i];
	        if (method.getName().equals(methodName)) {
		    return method;
		}
	    }
	    throw new Error("method " + methodName + " not found");
        } catch (Exception ex) {
	    throw new Error("findMethod caught : " + ex);
        }
    }

    static String shortPackageName = "sunw.beanbox";
    static String shortTmpDir = BeanBoxFrame.getTmpDir();
    static String packageName;
    static String tmpDir;
    static Vector javaFiles = new Vector();

    static {
	packageName =
	    shortTmpDir.replace(java.io.File.separatorChar, '.')+"."+shortPackageName;
	tmpDir = packageName.replace('.', java.io.File.separatorChar);
    }
}

