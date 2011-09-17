/*
 *
 * @(#) EncapsulatedEventManager.java 1.4@(#)
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
 * sunw.demo.encapsulatedEvents.EncapsulatedEventManager
 * </p>
 *
 * @version 1.0
 * @author Laurence P. G. Cable.
 * 
 */

package sunw.demo.encapsulatedEvents;

import sunw.demo.encapsulatedEvents.EncapsulatedEvent;
import sunw.demo.encapsulatedEvents.EncapsulatedEventListener;

/**
 * <p>
 * Containers or other "manager"-like objects wishing to offer an encapsulated
 * event stream from its "set" of containees or "managed" objects, should 
 * implement this interface to expose this facility.
 * </p>
 * <p>
 * Arbitrary objects wishing to observe the event stream from a particular
 * event source, should locate that event sources EncapsulatedEventManager
 * and use this interface on that instance to obtain such an event stream.
 * </p>
 * @seealso sunw.demo..encapsulatedEvents.EncapsulatedEvent
 * @seealso sunw.demo..encapsulatedEvents.EncapsulatedEventListener
 * @seealso sunw.demo..encapsulatedEvents.EncapsulatedEventAdaptor
 */

public interface EncapsulatedEventManager {

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

    Class[] getSourceEventListenerInterfaces(Object s);

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

    void addEncapsulatedEventListener(Object s, EncapsulatedEventListener eel);

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

    void removeEncapsulatedEventListener(Object s, EncapsulatedEventListener eel);

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

    void addEncapsulatedEventListener(Object		        s,
    			     	      EncapsulatedEventListener eel,
    			     	      Class[]		  	lc
    );

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

    void removeEncapsulatedEventListener(Object		     	   s,
					 EncapsulatedEventListener eel,
    			        	 Class[]		   lc
    );
}
