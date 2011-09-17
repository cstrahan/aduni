
package sunw.demo.sort;

/**
 * This class is used by the makefiles to create a serialized bean, the
 * SorterBean, as part of the sort.jar file.
 */

public class SorterBeanWriter {

    public static void main(String argv[]) {

	try {
	    String fname = argv[0];

	    // Allocate a SortItem bean.
	
	    SortItem sorter;

	    try {
		ClassLoader cl = SorterBeanWriter.class.getClassLoader();
		sorter = (SortItem)
		    java.beans.Beans.instantiate(cl, "sunw.demo.sort.SortItem");
	    } catch (Exception ex) {
		// 4080478 will raise a security exception; we catch it and
		// do a plain new() instead
		sorter = new SortItem();
	    }

	    // Now write it out as a serialized object.
	    java.io.FileOutputStream f = new java.io.FileOutputStream(fname);
	    java.io.ObjectOutput s = new java.io.ObjectOutputStream(f);
	    s.writeObject(sorter);
	    s.close();

	    System.exit(0);

	} catch (Exception ex) {
	    System.err.println("bean write failed: " + ex);
	    ex.printStackTrace();
	}
	
    }

}
