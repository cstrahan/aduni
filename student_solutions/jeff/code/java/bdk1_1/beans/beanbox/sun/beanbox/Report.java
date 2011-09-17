
/**
 * Report on the introspection information for a given Bean class.
 */

package sun.beanbox;

import java.beans.*;
import java.lang.reflect.*;

public class Report {

    public static void main(String argv[]) {

	if (argv.length != 1) {
	    throw new Error("Bad args");
	}
	if (argv[0].equals("-version")) {
	    System.out.println("Report version: " + BeanBoxFrame.getVersionID());
	    System.exit(0);
	}

	Class beanClass;
	try {
	    beanClass = Class.forName(argv[0]);
	} catch (Exception ex)  {
	    System.out.println("Could not insantiate class " + argv[0]);
	    ex.printStackTrace();
	    return;
	}

	report(beanClass);
    }

    public static void report(Class beanClass) {

	try {
	    BeanInfo bi = Introspector.getBeanInfo(beanClass);

	    putln("CLASS: " + beanClass.getName());

	    putln("\nAttributes:");
	    BeanDescriptor bd = bi.getBeanDescriptor();
	    put("    ");
	    put("Hidden-state",17);
	    put(" ");
	    put("" + (Boolean)bd.getValue("hidden-state"),27);

	    putln("\n\n");
	    putln("H => Hidden");
	    putln("E => Expert");
	    putln("[ => Indexed Property");
	    putln("* => Default Event or Property");
		
	    PropertyDescriptor props[] = bi.getPropertyDescriptors();
	    int defx = bi.getDefaultPropertyIndex();
   	    putln("\n\nProperties:");
	    for (int i = 0; i < props.length; i++) {
		PropertyDescriptor pd = props[i];
		putFlags(pd);
		if (i == defx) {
		    put("*");
		} else {
		    put(" ");
		}
	        put(pd.getName(), 17);
		put(" ");
	        put("" + pd.getPropertyType(), 27);
		put(" ");
	        putMethod(pd.getReadMethod());
		put("/");
	        putMethod(pd.getWriteMethod());
	        putln("");
		if (pd instanceof IndexedPropertyDescriptor) {
		    IndexedPropertyDescriptor ipd =
					(IndexedPropertyDescriptor) pd;
		    put("    ");
		    put("  (indexed)", 17);
		    put(" ");
	            put("" + ipd.getIndexedPropertyType(), 27);
		    put(" ");
	            putMethod(ipd.getIndexedReadMethod());
		    put("/");
	            putMethod(ipd.getIndexedWriteMethod());
	            putln("");
		}
	    }

	    EventSetDescriptor events[] = bi.getEventSetDescriptors();
	    defx = bi.getDefaultEventIndex();
	    putln("\nEvent sets:");
	    for (int i = 0; i < events.length; i++) {
	        EventSetDescriptor esd = events[i];
		putFlags(esd);
		if (i == defx) {
		    put("*");
		} else {
		    put(" ");
		}
	        put(esd.getName(), 22);
		put(" ");
	        putMethod(esd.getAddListenerMethod());
		put("/");
	        putMethod(esd.getRemoveListenerMethod());
	        putln("");
	        Method ems[] = esd.getListenerMethods();
	        for (int j = 0; j < ems.length; j++) {
		    put("", 8);
		    putMethod(ems[j]);
		    putln("");
	       }
	    }

	    MethodDescriptor methods[] = bi.getMethodDescriptors();
	    putln("\nMethods:");
	    for (int i = 0; i < methods.length; i++) {	
	        MethodDescriptor md = methods[i];
		putFlags(md);
		put(" ");
	        putln(md.getMethod().toString());
	    }
	    putln("");

	} catch (IntrospectionException ex) {
	    System.out.println("Caught introspection exception: " + ex);
	    ex.printStackTrace();
	}
    }

    private static void putMethod(Method m) {
	if (m == null) {
	    put("");
	} else {
	    put(m.getName());
  	}
    }

    private static void putFlags(FeatureDescriptor fd) {
	if (fd.isHidden()) {
	    put("H");
	} else {
	    put(" ");
	}
	if (fd.isExpert()) {
	    put("E");
	} else {
	    put(" ");
	}
	if (fd instanceof IndexedPropertyDescriptor) {
	    put("[");
	} else {
	    put(" ");
	}
    }

    private static void put(String s) {
	System.out.print(s);
    }

    private static void put(String s, int size) {
	System.out.print(s);
	for (int i = s.length(); i < size; i++) {
	    System.out.print(" ");
	}
    }

    private static void putln(String s) {
	put(s, s.length());
	System.out.println("");
    }

}


