
package sunw.demo.buttons;

/**
 * This class is used by the makefiles to create a serialized bean, the
 * OrangeButton, as part of the buttons.jar file.
 */

public class OrangeButtonWriter {

    public static void main(String argv[]) {

	try {
	    String fname = argv[0];

	    // Allocate an ExplicitButton and make it orange.
	    ClassLoader cl = OrangeButtonWriter.class.getClassLoader();
	    ExplicitButton button = (ExplicitButton)
			java.beans.Beans.instantiate(cl, "sunw.demo.buttons.ExplicitButton");
	    button.setBackground(java.awt.Color.orange);

	    // Now write it out as a serialzied object.
	    java.io.FileOutputStream f = new java.io.FileOutputStream(fname);
	    java.io.ObjectOutput s = new java.io.ObjectOutputStream(f);
	    s.writeObject(button);
	    s.close();

	    System.exit(0);

	} catch (Exception ex) {
	    System.err.println("OrangeButton write failed: " + ex);
	    ex.printStackTrace();
	}
	
    }

}
