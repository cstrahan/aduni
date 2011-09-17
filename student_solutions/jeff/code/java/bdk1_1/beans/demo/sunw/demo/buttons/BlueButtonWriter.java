
package sunw.demo.buttons;

/**
 * This class is used by the makefiles to create an externalized bean, the
 * BlueButton, as part of the buttons.jar file.
 */

public class BlueButtonWriter {

    public static void main(String argv[]) {

	try {
	    String fname = argv[0];

	    // Allocate an ExternalizableButton and make it blue.
	    ClassLoader cl = BlueButtonWriter.class.getClassLoader();
	    ExternalizableButton button = (ExternalizableButton)
			java.beans.Beans.instantiate(cl, 
				"sunw.demo.buttons.ExternalizableButton");
	    button.setBackground(java.awt.Color.blue);

	    // Now write it out as a serialzied object.
	    java.io.FileOutputStream f = new java.io.FileOutputStream(fname);
	    java.io.ObjectOutput s = new java.io.ObjectOutputStream(f);
	    s.writeObject(button);
	    s.close();

	    System.exit(0);

	} catch (Exception ex) {
	    System.err.println("BlueButton write failed: " + ex);
	    ex.printStackTrace();
	}
	
    }

}
