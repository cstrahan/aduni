/*
 *
 * @(#) EncapsulatedEventAdaptor.java 1.5@(#)
 *
 * Copyright (c) 1997 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * SUN MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. SUN SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 * 
 */

/**
 * <p>
 * sunw.demo.encapsulatedEvents.EncapsulatedEventAdaptor
 * </p>
 *
 * @version 1.0
 * @author Laurence P. G. Cable.
 */

package sunw.demo.encapsulatedEvents;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

import java.util.Vector;

import java.util.EventObject;
import java.util.EventListener;


import java.beans.BeanInfo;
import java.beans.EventSetDescriptor;
import java.beans.Introspector;
import java.beans.IntrospectionException;

import sunw.demo.encapsulatedEvents.EncapsulatedEvent;
import sunw.demo.encapsulatedEvents.EncapsulatedEventListener;
import sunw.demo.encapsulatedEvents.EncapsulatedEventException;
import sunw.demo.encapsulatedEvents.EncapsulatedEventAdaptorGenerator;

/**
 * <p>
 * sunw.demo.encapsulatedEvents.EncapsulatedEventAdaptor is an abstract base
 * class designed to support the dynamic generation of java.util.EventListener
 * sub-interface adaptor classes.
 * </p>
 * <p>
 * These dynamically generated adaptor classes can be used to take arbitrary
 * events and "wrap" them in sunw.demo.encapsulatedEvent.EncapsulatedEvent
 * objects. These "encapsulated" events are then fired on the
 * sunw.demo.encapsulatedEventListener interface.
 * </p>
 * <p>
 * Using this dynamic adaptor class, objects that emits events on arbitrary`
 * sub-interfaces of java.util.EventListener can be encapsulated and delivered
 * onto a single EventListener interface, this allowing late binding of event
 * behaviors and filtering applications.
 * </p>
 *
 * @seealso sunw.demo.encapsulatedEvents.EncapsulatedEvent
 * @seealso sunw.demo.encapsulatedEvents.EncapsulatedEventListener
 * @seealso sunw.demo.encapsulatedEvents.EncapsulatedEventException
 * @seealso sunw.demo.encapsulatedEvents.EncapsulatedEventAdaptorGenerator
 */

public abstract class EncapsulatedEventAdaptor {

    /**
     * The Event Source
     */

    protected Object source;

    /**
     * The Event Source's add<T>Listener method
     */

    protected Method addAdaptorMethod;

    /**
     * The Event Source's remove<T>Listener method
     */

    protected Method removeAdaptorMethod; 

    /*
     * This Adaptor's list of listeners ...
     */

    protected Vector listeners = new Vector(0, 1);

    /**
     * Generate an Adaptor instance for the Listener Interface and Event Source
     *
     * @param  lc	The java.lang.Class object for the listener to adapt.
     * @param  s	The source object that the adaptor will listen to.
     *
     * @return The newly instantiated dynamic adaptor
     * 
     * @throws ClassNotFoundException
     * @throws IllegalArgumentException
     */

    public static EncapsulatedEventAdaptor getEncapsulatedEventAdaptor(Class lc, Object s) throws ClassNotFoundException, InstantiationException, IllegalAccessException, IntrospectionException {
        Class		       	 eeac  = null;
    	EncapsulatedEventAdaptor eea   = null;
	BeanInfo		 sbi   = Introspector.getBeanInfo(s.getClass());
	EventSetDescriptor[]     sesd  = sbi.getEventSetDescriptors();

	/*
	 * first validate that the event source emits event on the interface
	 * specified.
	 */
 
	if (validateEventSource(sesd, lc) == -1) { 
	    throw new IllegalArgumentException("Object: "	    +
					       s	  	    +
					       " does not source: " +
					       lc.getName()
		      );
	}

	// generate the adaptor class

        eeac = EncapsulatedEventAdaptor.getEncapsulatedEventAdaptorClass(lc);

	// instantiate an instance of it ...

        eea  = (EncapsulatedEventAdaptor)eeac.newInstance();

	// and register the adaptor with the event source

	try {
	    eea.setSource(s);
	} catch (Exception e) {
	    throw new RuntimeException("Failed to register with source");
	}

	return eea;
    }


    /**
     *
     * @param   lc       The java.lang.Class object for the listener to adapt.
     *
     * @return The Class object for the newly generated dynamic adaptor class.
     */

    public static Class getEncapsulatedEventAdaptorClass(Class lc) throws ClassNotFoundException {
	if (!java.util.EventListener.class.isAssignableFrom(lc)) {
	    throw new IllegalArgumentException("Class is not a subinterface of java.util.EventListenerEventListener");
	}

    	return EncapsulatedEventAdaptorGenerator.getAdaptorClassForListenerClass(lc);
    }

    /**
     * <p> default constructor.
     */

    protected EncapsulatedEventAdaptor() { super(); }

    /**
     * @return the java.lang.Class object for the java.util.EventListener subinterface this instance adapts.
     */

    abstract public Class getListenerClass();

    /**
     * @return the name of the java.util.EventListener subinterface this instance adapts.
     */

    public String getListenerClassName() {
	return getListenerClass().getName();
    }

    /**
     * @return the event source object that this instance is adapting.
     */

    public synchronized Object getSource() { return source; }

    /**
     * @param  sesd	the EventSetDescriptor[] from the prospective event source.
     * @param  lc	the java.lang.Class for the EventListener we are adapting.
     * @return the index of the matching EventSetDescriptor or -1.
     */

    private static int validateEventSource(EventSetDescriptor[] sesd, Class lc) {
	for (int i = 0; i < sesd.length; i++)
	    if (lc.equals(sesd[i].getListenerType())) return i;

	return -1;
    }

    /**
     * <p>
     * setSource sets the adaptor instance to listen to the specified
     * source. This operation will fail if the source does not emit events
     * on the EventListener subinterface this adaptor implements.
     * </p>
     *
     * @param s 	the prospective event source.
     *
     * @throws IntrospectionException
     */

    public synchronized void setSource(Object s)
    throws IntrospectionException, Exception {

	if (source != null && s != null && source.equals(s)) return;

    	if (source != null) removeAdaptorFromSource(); // unregister current

	if (s == null) {
	    source	        = null;
	    addAdaptorMethod    = null;
	    removeAdaptorMethod = null;
	    return;
	}

	BeanInfo	     sbi   = Introspector.getBeanInfo(s.getClass());
	EventSetDescriptor[] sesd  = sbi.getEventSetDescriptors();

	int		     i;

	if ((i = validateEventSource(sesd, getListenerClass())) == -1) {
	    throw new IllegalArgumentException("Object: "	    +
					       s	  	    +
					       " does not source: " +
					       getListenerClassName()
		      );
	}


	// update state

    	Object	 	     olds  = source;
	Method   	     oldam = addAdaptorMethod;
	Method   	     oldrm = removeAdaptorMethod;

    	source 		    = s;
	addAdaptorMethod    = sesd[i].getAddListenerMethod();
	removeAdaptorMethod = sesd[i].getRemoveListenerMethod();

    	try {
    	    addAdaptorToSource(); // register with new source
    	} catch (Exception e) {

	    // something went wrong ... restore previous state.

    	    source		= olds;
	    addAdaptorMethod    = oldam;
	    removeAdaptorMethod = oldrm;

    	    if (source != null) addAdaptorToSource();

    	    throw e; // reraise problem
    	}
    }

    /**
     * @param el Adds the EncapsulatedEventListener to the listeners for this adaptor
     */

    public void addEncapsulatedEventListener(EncapsulatedEventListener el) {
    	synchronized(listeners) {
    	    if (!listeners.contains((Object)el))
		listeners.addElement((Object)el);
    	}
    }

    /**
     * @param el Removes the EncapsulatedEventListener to the listeners for this adaptor
     */

    public void removeEncapsulatedEventListener(EncapsulatedEventListener el) {
    	synchronized(listeners) {
	    if (listeners.contains((Object)el))
    		listeners.removeElement((Object)el);
    	}
    }

    /**
     * @returns number of listeners currently registered
     */

    public int getEncapsulatedListenerCount() {
	int n; 

	synchronized(listeners) { n = listeners.size(); }

	return n;
    }

    /**
     * @returns copy of listeners currently registered
     */

    public Vector getEncapsulatedListeners() {
	Vector copy;

	synchronized(listeners) { copy = (Vector)listeners.clone(); }

	return copy;
    }

    /**
     *
     * <p>
     * This method is called from simple EventListener method stubs of
     * dynamic subclasses in order to create and EncapsulatedEvent and
     * deliver it to the registered listeners.
     * </p>
     *
     * @param e  The EventObject being encapsulated
     * @param lm The jav.lang.reflect.Method describing the listener Method.
     *
     * @throws Exception
     * @throws RuntimeException
     */

    protected void fire(EventObject e, Method lm) throws Exception {
    	Vector		  	  copy = null;
    	EncapsulatedEvent	  ee   = new EncapsulatedEvent(e, lm);
	EncapsulatedEventListener eel  = null;

    	synchronized(listeners) { copy = (Vector)listeners.clone(); }

    	for (int i = 0; i < copy.size(); i++) try {
	    eel = (EncapsulatedEventListener)copy.elementAt(i);
	    eel.encapsulatedEvent(ee);
    	} catch (EncapsulatedEventException eee) {
		handleEncapsulatedEventException(eee, eel, lm);
	}
    }

    /**
     * <p>
     * This method is called from cracked EventListener method stubs of
     * dynamic subclasses in order to create and EncapsulatedEvent and
     * deliver it to the registered listeners.
     * </p>
     *
     * @param a  The cracked Event arguments being encapsulated
     * @param lm The jav.lang.reflect.Method describing the listener Method.
     *
     * @throws Exception
     * @throws RuntimeException
     */

    protected void fire(Object[] a, Method lm) throws Exception {
    	Vector		  	  copy = null;
    	EncapsulatedEvent	  ee   = new EncapsulatedEvent(source, lm, a);
	EncapsulatedEventListener eel  = null;

    	synchronized(listeners) { copy = (Vector)listeners.clone(); }

    	for (int i = 0; i < copy.size(); i++) try {
	    eel = (EncapsulatedEventListener)copy.elementAt(i);
	    eel.encapsulatedEvent(ee);
    	} catch (EncapsulatedEventException eee) {
		handleEncapsulatedEventException(eee, eel, lm);
	}
    }

    
    /**
     * <p>
     * EncapsulatedEventListener's may raise exceptions to the originating
     * event source through an adaptor by throwing the actual exception within
     * an instance of EncapsulatedEventException.
     * </p>
     * <p>
     * This method is called to verify that it is allowable to re-raise the
     * exception from the adaptor to the source, by checking that the listener
     * method on the adaptor that the source delivered the original event to
     * is permitted to throw such an exception, otherwise a RuntimeException
     * is raised.
     * </p>
     *
     * @param ex	the exception thrown by the listener
     * @param eel 	the listener instance that threw it
     * @param lm	the adaptors listener method that must re-raise it
     *
     */

    protected final void handleEncapsulatedEventException(EncapsulatedEventException ex, EncapsulatedEventListener eel, Method lm) throws Exception {
	Class   ec  = ex.getExceptionClass();
    	Class[]	ext = lm.getExceptionTypes();

	// let's see if the Exception encapsulated is one the source is expecting
	// if it is then throw it.

    	for (int i = 0; i < ext.length; i++) {
	    if (ext[i].isAssignableFrom(ec)) throw ex.getException();
    	}

	// if we get here then the Exception the listener is trying
	// to throw isnt one the source is expecting ... so throw it as a RTE

	throw new RuntimeException("Event Source ["		   + 
				   source			   +
				    "] is not prepared to catch [" +
				   ex.getException()           	   +
				   "] from listener ["		   +
				   eel
	);
    }

    /**
     * Adds this Adaptor to the Event Source
     */

    protected void addAdaptorToSource() {
	try {
	    Object[] args = new Object[1];

	    args[0] = this;

	    addAdaptorMethod.invoke(source, args);
	} catch (InvocationTargetException ite) {
	    System.err.println("cannot add adaptor [" 	       +
			       this		      	       +
			       "] to source ["	      	       +
			       source		      	       +
			       "] InvocationTargetException: " +
			       ite.getMessage()
	    );
	} catch (IllegalAccessException iae) {
	    System.err.println("cannot add adaptor [" 	    +
			       this		      	    +
			       "] to source ["	      	    +
			       source		      	    +
			       "] IllegalAccessException: " +
			       iae.getMessage()
	    );
	}
    }

    /**
     * Removes this Adaptor instance from the Event Source
     */

    protected void removeAdaptorFromSource() {
	try {
	    Object[] args = new Object[1];

	    args[0] = this;

	    removeAdaptorMethod.invoke(source, args);
	} catch (InvocationTargetException ite) {
	    System.err.println("cannot remove adaptor ["       +
			       this		      	       +
			       "] from source ["      	       +
			       source		      	       +
			       "] InvocationTargetException: " +
			       ite.getMessage()
	    );
	} catch (IllegalAccessException iae) {
	    System.err.println("cannot remove adaptor ["    +
			       this		      	    +
			       "] from source ["	    +
			       source		      	    +
			       "] IllegalAccessException: " +
			       iae.getMessage()
	    );
	}
    }
}
