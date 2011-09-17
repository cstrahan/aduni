/*
 *
 * @(#) EncapsulatedEvent.java 1.3@(#)
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
 * sunw.demo.encapsulatedEvents.EncapsulatedEvent
 * </p>
 *
 * @version 1.0
 * @author Laurence P. G. Cable.
 */

package sunw.demo.encapsulatedEvents;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

import java.util.EventObject;
import java.util.EventListener;

/**
 *
 * <p>
 * The EncapsulatedEvent class is a subclass of java.util.EventObject and
 * is designed as part of a package sun.demo.encapsulatedEvents to demonstrate
 * the dynamic generation of JavaBeans Event Adaptor classes, and also a
 * technique for creating a polymorphic event processing model as an extension
 * of the existing JavaBeans Event Model.
 * </p>
 * @see sunw.demo.encapsulatedEvents.EncapsulatedEventAdaptor
 * @see sunw.demo.encapsulatedEvents.EncapsulatedEventListener
 * @see sunw.demo.encapsulatedEvents.EncapsulatedEventAdaptorGenerator
 * @see sunw.demo.encapsulatedEvents.EncapsulatedEventException
 */

public class EncapsulatedEvent extends EventObject {

    protected EventObject	       event;	
    
    protected Method      	       listenerMethod;

    protected Class	  	       listenerInterface;

    protected Object[]    	       eventArgs;

    /**
     * <p>
     * Construct an EncapsulatedEvent object. An EncapsulatedEvent contains
     * a reference to the "actual" event occurring, and the Method/Class
     * from which this "actual" event was fired.
     * </p>
     * @param s	The "source" of the "actual" event (may not be null).
     * @param e The "actual" event itself (or null if this is a "cracked" event.
     * @param m The java.lang.reflect.Method describing the Listener method that this event was emitted from.
     *
     * @throw NullPointerException
     */

    protected EncapsulatedEvent(Object s, EventObject e, Method m, Object[] a) {
    	super(s);

	if (m == null || (e == null && a == null))
		throw new NullPointerException("Encapsulated Event constructor args");

    	if (e == null && a != null) {
	    for (int i = 0; i < a.length; i++) {
    		if (a[i] instanceof EventObject) {
    		    e = (EventObject)a[i];
    		    break;
    		}
	    }
    	}

    	/*
    	 * if we are forwarding an EncapsulatedEvent then unwrap it!
    	 */

    	if (e != null && e instanceof EncapsulatedEvent) {
    	    EncapsulatedEvent ee = (EncapsulatedEvent)e;

    	    event = ee.getEvent();
    	    m     = ee.getListenerMethod();
    	} else event = e;

    	listenerMethod    = m;
    	listenerInterface = m.getDeclaringClass();

	// if the event object is not included in the event args then insert
	// it at index 0.

	if (e != null) {
	    if (a == null) {
		a    = new Object[1];
		a[0] = event;
	    } else {
		int i;

		for (i = 0; i < a.length && !e.equals(a[i]); i++) 
		;

		if (i == a.length) { // not found
		    Object[] tmp = new Object[a.length + 1];

		    tmp[0] = event; // put it at the start

		    for (i = 0; i < a.length; i++) tmp[i+1] = a[i];
		}
	    }
	}

    	eventArgs = a;
    }
    
    /**
     * <p>
     * Construct an Event Object from an intermediate.
     * </p>
     */

    public EncapsulatedEvent(Object s, EventObject e, Method m) {
    	this(s, e, m, null);
    }

    /**
     * <p>
     * Construct an Event Object from a simple event listener method.
     * </p>
     */

    public EncapsulatedEvent(EventObject e, Method m) {
    	this(e.getSource(), e, m);
    }

    /**
     * <p>
     * Construct an Event Object from a cracked event listener method.
     * </p>
     */

    public EncapsulatedEvent(Object s, Method m, Object[] a) {
    	this(s, null, m, a);
    }

    /**
     * @return The EventObject instance encapsulated or null if from a cracked event.
     */

    public EventObject getEvent() {
    	return event;
    }

    /**
     * @return The java.lang.Class of the EventObject instance encapsulated or null if from a cracked event.
     */

    public Class getEventClass() {
    	return (event != null ? event.getClass() : null);
    }

    /**
     *  @return The String name of the class of the EventObject instance encapsulated or null if from a cracked event.
     */

    public String getEventClassName() {
    	return (event != null ? event.getClass().getName() : null);
    }

    /**
     * @return The source object of the encapsulated EventObject instance.
     */

    public Object getEventSource() {
    	return (event != null ? event.getSource() : this.getSource());
    }

    /**
     * @return the java.lang.reflect.Method of the EventListener Method.
     */

    public Method getListenerMethod() { return listenerMethod; }

    /**
     * @return the name of the java.lang.reflect.Method of the EventListener Method.
     */

    public String getListenerMethodName() {
    	return listenerMethod.getName();
    }


    /**
     * @return the Class for the orginating EventListener subinterface
     */

    public Class getListenerInterface() {
    	return listenerInterface;
    }
 
    /**
     * @return the name of the Class for the orginating EventListener subinterface
     */

    public String getListenerInterfaceName() {
    	return listenerInterface.getName();
    }

    /**
     * @return an Object[] of the encapsulated event, if the EventObject is not already in the args, it is inserted by this class at index 0.
     */

    public Object[] getEventArguments() {
	return eventArgs;
    }

    /**
     * <p>
     * This method can be used to deliver the encapsulated event to an object
     * that conforms to the EventListener sub-interface that the event originated from.
     </p>
     *
     * @param el The EventListener object to deliver the unencapsulated event to.
     *
     * @throws InvocationTargetException
     * @throws IllegalAccessException
     */

    public void deliverEvent(EventListener el) throws InvocationTargetException, IllegalAccessException {
    
    	/*
    	 * a likely error here is the caller passes an EventListener
    	 * reference that does not match the EventListener instance
    	 * this particular EncapsulatedEvent encapsulates, fortunately
    	 * the JVM RT checks will catch this and throw an exception.
    	 *
    	 * In any case the caller of this method should get the
    	 * exception (if any) so we should not catch it here.
    	 */

    	listenerMethod.invoke((Object)el, eventArgs);
    }
}
