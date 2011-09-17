package sun.beanbox;

/**
 * A class that generates .class files
 * 
 * It currently uses the sun.tools.javac.* classes
 */

public class ClassCompiler {

    /**
     * Invoke the javac compiler.
     */
    public static boolean compile(String fileName, String classpath) {
	String args[] = new String[4];
	args[0] = "-classpath";
	args[1] = classpath;
	args[2] = "-nowarn";
	args[3] = fileName;

	//System.err.println("javac -classpath "+classpath+" -nowarn "+fileName);

	Process p = null;	
	try {
	    // The prefered way to invoke javac on JDK1.2 and JDK1.1 platforms.
	    p = Runtime.getRuntime().exec("javac " 
					+ args[0]
					+ " "
					+ args[1]
					+ " "
					+ args[2]
					+ " "
					+ args[3]
					);
						
	    return (p.waitFor() == 0) ? true : false; // status code 0 indicates success.
	} catch (Throwable th) {
	    System.err.println(
				"WARNING: Could not Runtime.exec(String) \"javac\" in"
				+ " the standard way: "
				+ th
				);
	}	
	
	return warningMessage();
    }
    
    static boolean warningMessage() {
	    System.err.println("");
	    System.err.println("Check that the version of \"javac\" that you are running");
	    System.err.println("is the one supplied with Sun's JDK1.x (which includes the");
	    System.err.println("compiler classes) and not some other version of \"java\"");
	    System.err.println("or JRE shipped with some other product.");
	    System.err.println("");
	    return false;
    }
    
}
