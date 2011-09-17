/*
 *
 * @(#) EncapsulatedEventListener.java 1.3@(#)
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
 * sunw.demo.encapsulatedEvents.EncapsulatedEventListener
 * </p>
 * 
 * @version 1.0
 * @author Laurence P. G. Cable
 *
 * Copyright Sun Microsystems Inc., 1997.
 */

package sunw.demo.encapsulatedEvents;

import java.util.EventListener;

import sunw.demo.encapsulatedEvents.EncapsulatedEvent;
import sunw.demo.encapsulatedEvents.EncapsulatedEventException;

/**
 * <p>
 * This is the polymorphic/generic EventListener interface that arbitrary
 * objects wishing to receive "encapsulated" events, from dynamically
 * generated adaptors interposed on specific event sources, should implement.
 * </p>
 *
 * @seealso sunw.demo.encapsulatedEvents.EncapsulatedEvent
 * @seealso sunw.demo.encapsulatedEvents.EncapsulatedEventAdaptor
 * @seealso sunw.demo.encapsulatedEvents.EncapsulatedEventException
 */

public interface EncapsulatedEventListener extends EventListener {

    /**
     * @param ee the encapsulated event from the dynamic adaptor.
     *
     * @throws EncapsulatedEventException.
     *
     */

    void encapsulatedEvent(EncapsulatedEvent ee) throws EncapsulatedEventException;
}
