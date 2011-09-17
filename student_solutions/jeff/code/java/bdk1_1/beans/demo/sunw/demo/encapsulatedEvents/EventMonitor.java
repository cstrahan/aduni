/*
 *
 * @(#) EventMonitor.java 1.13@(#)
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
 * sunw.demo.encapsulatedEvents.EventMonitor.java
 * </p>
 *
 * @version 1.0
 * @author Laurence P. G. Cable.
 */

package sunw.demo.encapsulatedEvents;

import java.awt.*;
import java.beans.*;
import java.beans.beancontext.*;
import java.util.*;

/**
 *
 * <p>
 * The EventMonitor is a simple bean that demonstrates the power of the
 * EncapsulatedEventManager and EncapsulatedEventAdaptor classes.
 * </p>
 * <p>
 * The EventMonitor and EventManager classes provide a simple event debugging
 * interface that allows the BeanBox user to display a flow of all events from
 * selected Beans.
 * </p>
 *
 * @see sunw.demo.encapsulatedEvents.EncapsulatedEventAdaptor
 * @see sunw.demo.encapsulatedEvents.EncapsulatedEventListener
 * @see sunw.demo.encapsulatedEvents.EncapsulatedEventAdaptorGenerator
 * @see sunw.demo.encapsulatedEvents.EncapsulatedEventException
 */

public final class EventMonitor extends TextArea implements BeanContextProxy { 

    private static String msg = "Event Monitor\n";

    private static Dimension zero = new Dimension(0,0);

    private transient EventManager em; 
    private Vector beans = new Vector();

    private transient BeanContextChildSupport bccs = new BeanContextChildSupport() {
	protected void initializeBeanContextResources() {
	    if (beanContext != null) 
		beanContext.addBeanContextMembershipListener(EventMonitor.this.bcml);
	}

	protected void releaseBeanContextResources() {
	    if (beanContext != null)
		beanContext.removeBeanContextMembershipListener(EventMonitor.this.bcml);
	}

    };

    private transient BeanContextMembershipListener bcml = new BeanContextMembershipListener() {
	public void childrenAdded(BeanContextMembershipEvent bcme) {
	    Iterator i = bcme.iterator();

	    while (i.hasNext()) {
		Object o = i.next();

		if (!(o.equals(EventMonitor.this) || o.equals(EventMonitor.this.bccs))) {
		    EventMonitor.this.addEventSource(o);
		}
	    }
	}

	public void childrenRemoved(BeanContextMembershipEvent bcme) {
	    Iterator i = bcme.iterator();

	    while (i.hasNext()) {
		Object o = i.next();

		if (!(o.equals(EventMonitor.this) || o.equals(EventMonitor.this.bccs))) {
		    EventMonitor.this.removeEventSource(o);
		}
	    }
	}
    };

    /**
     * <p> Construct an EventMonitor Bean. </p>
     */

    public EventMonitor() {
	super(msg, 4, 32);

	setEditable(false);
	setVisible(true);

	em = new EventManager(this);
    }

    /**
     * <p>
     * Adds the listener eel to the source s to receive ALL events s emits
     * as encpasulated events. Will throw IllegalArgumentException if s is
     * not managed by this EncapsulatedEventManager.
     * </p>
     *
     * @param s		the event source
     * @param eel	the listener
     *
     * @throws IllegalArgumentException
     */

    public void addEventSource(Object s) {
	em.addEncapsulatedEventListener(s, em, em.getSourceEventListenerInterfaces(s));
	beans.addElement(s);
    }

    /**
     * <p>
     * Removes the listener eel from the source s, thus unregistering eel from
     * receiving encapsulated events for all the events that s emits.
     * Will throw IllegalArgumentException if s is not managed by this
     * EncapsulatedEventManager.
     * </p>
     *
     * @param s		the event source
     *
     * @throws IllegalArgumentException
     * @throws NullPointerException
     */

    void removeEventSource(Object s) {
	em.removeEncapsulatedEventListener(s, em, em.getSourceEventListenerInterfaces(s));
	beans.removeElement(s);
    }

    /**
     *  @return the object's preferred size
     */

    public Dimension getPreferredSize() {
	Dimension current = getSize();

	/*
 	 * This method exists as a hack to ensure that when the TextArea is
	 * resized by the user that it in fact does resize properly.
 	 *
 	 * The bug exhibited is that the object gets a reshape() to the new
 	 * Dimension followed by a reshape() to the previosu Dimension prior
 	 * to the user resize gesture.
 	 * 
 	 * The cause of this is that the Wrapper object first resizes the
 	 * Bean calling reshape with the new size, then it calls layout
	 * which requests the Beans preferredSize().
	 *
	 * As best as I can tell the Component.getPreferredSize() replies
	 * with the preferredSize() of its peer which at this stage of the
	 * proceedings is still the old Dimension since the actual resize
	 * of the peer was deferred in Component.reshape().
	 *
	 * cheating by replying my current Dimension if non-zero seems to
	 * work since by th etime layout occurs Component.reshape() has
	 * updated this object's Dimension properly.
	 *
	 * having said all that I am not confident that this does not 
	 * introduce less than desirable behaviors under different 
	 * circumstances.
	 */

	return (current.equals(zero) ? super.getPreferredSize() : current);
	
    }

    /**
     * <p> register the event source for monitoring </p>
     *
     * @param eo the event object
     */

    public void initiateEventSourceMonitoring(EventObject eo) {
	addEventSource(eo.getSource());
    }

    /**
     * @return my BCC ...
     */

    public BeanContextChild getBeanContextProxy() { return bccs; }

    // Support for serialization.
    private void writeObject(java.io.ObjectOutputStream s)
        		throws java.io.IOException {
	// Sorry the EventMonitor is not serializable yet,
	// because the various event listeners we create are
	// not serializable.
	throw new java.io.IOException("EventMonitor isn't serializable");
    }

    private void readObject(java.io.ObjectInputStream s)
        		throws java.lang.ClassNotFoundException,
			       java.io.IOException {
	s.defaultReadObject();
	em = new EventManager(this);
	// Reattach all the previously registered beans.
	for (int i = 0; i < beans.size(); i++) {
	    Object bean = beans.elementAt(i);
	    if (bean == null) {
		continue;
	    }
	    addEventSource(bean);
	}
    }

}

/*
 * Package private class to manage beans being monitored.
 */

final class EventManager implements EncapsulatedEventManager, EncapsulatedEventListener {

    private int		 	eventCnt = 1;

    private EventMonitor	em;

    private transient Hashtable	sources = new Hashtable();


    /*
     * 
     */

    EventManager(EventMonitor owner) {
	super();

	em = owner;
    }


    /**
     * <p>
     * Called to determine the events that a particular source emits. Will throw
     * IllegalArgumentException if s is not managed by this
     * EncapsulatedEventManager.
     * </p>
     *
     * @param s the event source
     *
     * @return list of java.util.EventListener sub-interfaces that s sources or null.
     *
     * @throws IllegalArgumentException
     * @throws NullPointerException
     */

    public Class[] getSourceEventListenerInterfaces(Object s) {
	EventSetDescriptor[] esd = null;

	try {
	    esd = Introspector.getBeanInfo(s.getClass()).getEventSetDescriptors();
	} catch (IntrospectionException ie) {
	    System.err.println("Failed to introspect");
	}

	Class[] ifs = (esd != null && esd.length >  0
		       ? new Class[esd.length]
		       : new Class[0]
		     );

	for (int i = 0; i < ifs.length; i++) {
	    ifs[i] = esd[i].getListenerType();
	}

	return ifs;
    }

    /**
     * <p>
     * Adds the listener eel to the source s to receive ALL events s emits
     * as encpasulated events. Will throw IllegalArgumentException if s is
     * not managed by this EncapsulatedEventManager.
     * </p>
     *
     * @param s		the event source
     * @param eel	the listener
     *
     * @throws IllegalArgumentException
     * @throws NullPointerException
     */

    public void addEncapsulatedEventListener(Object s, EncapsulatedEventListener eel) {
	addEncapsulatedEventListener(s, eel, getSourceEventListenerInterfaces(s));
    }

    /**
     * <p>
     * Removes the listener eel from the source s, thus unregistering eel from
     * receiving encapsulated events for all the events that s emits.
     * Will throw IllegalArgumentException if s is not managed by this
     * EncapsulatedEventManager.
     * </p>
     *
     * @param s		the event source
     * @param eel	the listener
     *
     * @throws IllegalArgumentException
     * @throws NullPointerException
     */

    public void removeEncapsulatedEventListener(Object s, EncapsulatedEventListener eel) {
	removeEncapsulatedEventListener(s, eel, getSourceEventListenerInterfaces(s));
    }

    /**
     * <p>
     * Adds the listener eel to the source s to receive the events enumerated
     * by lc that s emits. Will throw IllegalArgumentException if s is not
     * managed by this EncapsulatedEventManager or if lc contains a reference
     * to a Class that s does not emit events on.
     * </p>
     *
     * @param s		the event source
     * @param eel	the listener
     * @param lc	the list of events to register the eel to receive.
     *
     * @throws IllegalArgumentException
     * @throws NullPointerException
     */

    public synchronized void addEncapsulatedEventListener(
				Object       		  s,
				EncapsulatedEventListener eel,
				Class[]		          lc
    ) {
	if (s   == null) throw new NullPointerException("Source");
	if (eel == null) throw new NullPointerException("Listener");
	if (lc  == null) throw new NullPointerException("Interfaces");

	Hashtable interfaces = (Hashtable)sources.get(s);

	if (interfaces == null) {
	    interfaces = new Hashtable();
	    sources.put(s, interfaces);
	}

	for (int i = 0; i < lc.length; i++) {
	    EncapsulatedEventAdaptor eea = (EncapsulatedEventAdaptor)interfaces.get(lc[i]);

	    if (eea == null) { // its new
		try {
		    eea = EncapsulatedEventAdaptor.getEncapsulatedEventAdaptor(lc[i], s);
		} catch (ClassNotFoundException  cnfe)  {
		    System.err.println("Cant create adaptor class");
		} catch (InstantiationException  inste) {
		    System.err.println("Cant instantiate adaptor");
		} catch (IllegalAccessException iae)   {
		    System.err.println("illegal access");
		} catch (IntrospectionException  intre) {
		    System.err.println("introspection error");
		}

		if (eea != null) {
		    synchronized(eea) {
			eea.addEncapsulatedEventListener(eel);
		    }
    		    interfaces.put(lc[i], eea);

		    em.append("Listening to: " + lc[i].getName() + " on: " + s + "\n");
		} else {
    		    interfaces.put(lc[i], null);
		}
	    }
	}
    }

    /**
     * <p>
     * Removes the listener eel from the source s, thus unregistering for 
     * encapsulated events from s enumerated by lc. Will throw
     * IllegalArgumentException if s is not managed by this
     * EncapsulatedEventManager or if lc contains a reference
     * to a Class that s does not emit events on. 
     * </p>
     *
     * @param s		the event source
     * @param eel	the listener
     * @param lc	the list of events to unregister the eel from receiving.
     *
     * @throws IllegalArgumentException
     * @throws NullPointerException
     */

    public synchronized void removeEncapsulatedEventListener(
				Object	     	  	  s,
				EncapsulatedEventListener eel,
    			        Class[]			  lc
    ) {

	if (s   == null) throw new NullPointerException("Source");
	if (eel == null) throw new NullPointerException("Listener");
	if (lc  == null) throw new NullPointerException("Interfaces");

	Hashtable interfaces = (Hashtable)sources.get(s);

	
	if (interfaces == null) throw new IllegalArgumentException("Source");

	for (int i = 0; i < lc.length; i++) {
	    EncapsulatedEventAdaptor eea = (EncapsulatedEventAdaptor)interfaces.get(lc[i]);

	    if (eea != null) {
		eea.removeEncapsulatedEventListener(eel);

		synchronized(eea) {
		    if (eea.getEncapsulatedListenerCount() == 0) try {
			eea.setSource(null);
		    } catch (Exception e) {
			System.err.println("failed to unregister source");
		    }

		    em.append("No longer listening to: " + lc[i].getName() + " on: " + s + "\n");
		    interfaces.put(lc[i], null); 
		} 
	    }
	}
    }

    /**
     * <p>
     * The event handler logs the encapsulated Event to the text area.
     * </p>
     *
     * @param ee The Encapsulated Event
     */

    public void encapsulatedEvent(EncapsulatedEvent ee) {
	Object	    s  = ee.getEventSource();
	EventObject eo = ee.getEvent();

	// log the event

	em.append(eventCnt++  + " Source: [" + s + "] Event: [" + eo + "]" + "\n");
    }
}
