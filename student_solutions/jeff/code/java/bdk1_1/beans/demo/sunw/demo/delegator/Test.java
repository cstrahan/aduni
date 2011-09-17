
package sunw.demo.delegator;

import java.io.*;
import sunw.demo.classfile.DelegatorClassFile;

class Fred {
    Fred fred;
    public void setFoo(Fred f) {
	fred = f;
    }
    public Fred getFoo() {
	return fred;
    }
}

class Bert {
    Fred fred;
    public void setFoo(Fred f) {
	fred = f;
    }
    public Fred getFoo() {
	return fred;
    }
    public short blek(Fred a, boolean b, char c, short d, Fred e, boolean f, char g, short h) {
	return h;
    }
}



public class Test {

    public static void main(String argv[]) {

	try {
	    // Build the path for the sunw/demo/delegator/XXX.class file
	    File f = new File("sunw");
	    f = new File(f, "demo");
	    f = new File(f, "delegator");
	    f = new File(f, "XXX.class");


	    FileOutputStream fos = new FileOutputStream(f);
            BufferedOutputStream os = new BufferedOutputStream(fos);

	    Class targs[] = { Fred.class, Bert.class };

	    DelegatorClassFile guts = new DelegatorClassFile("sunw.demo.delegator.XXX", targs);
	    guts.write(os);

	    os.close();
	    fos.close();

	} catch (Exception ex) {
	    System.err.println("Caught " + ex);
	    ex.printStackTrace();
	}
    }
}
