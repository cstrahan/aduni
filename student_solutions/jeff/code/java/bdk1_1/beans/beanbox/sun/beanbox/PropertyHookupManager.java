package sun.beanbox;

/**
 * This class manages hookups between properties, so that a
 * bound property change on object X turns into a property
 * set on a related property on object Y.
 * <P>
 * We do this by associating a PropertyHookup adaptor with each
 * source object that we are interested in.  As part of the adaptor
 * we keep track of which target setter methods to call when a given
 * property changes.
 */

import java.lang.reflect.*;
import java.beans.*;
import java.io.*;
import java.util.Hashtable;
import java.util.Vector;
import sunw.beanbox.PropertyHookup;

class PropertyHookupManager {

    /**
     * Create a property hookup, so that a change to the named bound
     * property on the source object turns into a call on the "setter"
     * method of the given target object.
     */

    public synchronized static void attach(Wrapper sourceWrapper,
			String propertyName, Method getter,
			Wrapper targetWrapper, Method setter) {

	Object source = sourceWrapper.getBean();
	Object targetObject = targetWrapper.getBean();

	PropertyHookup hook = (PropertyHookup) instances.get(source);
	if (hook == null) {
	    // This is the first property hookup on this source object.
	    // Push a PropertyHookup adaptor onto the source.
	    hook = new PropertyHookup(source);
	    instances.put(source, hook);
	    // Register our listener object with the source Wrapper.
	    sourceWrapper.addEventTarget("propertyChange", null, hook);
	}

	sourceWrapper.addPropertyTarget(propertyName, targetObject, setter);
	hook.attach(source, propertyName, getter, targetObject, setter);
    }

    // This table maps from event sources to PropertyHookup objects.
    private static Hashtable instances = new Hashtable();

}
