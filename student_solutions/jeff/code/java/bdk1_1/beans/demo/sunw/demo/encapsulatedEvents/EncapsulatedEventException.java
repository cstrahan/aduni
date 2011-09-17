/*
 *
 * @(#) EncapsulatedEventException.java 1.3@(#)
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
 * sunw.demo.encapsulatedEvents.EncapsulatedEventException
 * </p>
 *
 * @version 1.0
 * @author  Laurence P. G. Cable
 */

package sunw.demo.encapsulatedEvents;

/**
 * Implementors of EncapsulatedListener use this Exception class to throw
 * Encapsulated Event specific exceptions back to the Event Source.
 *
 * For instance if a Listener is processing an EncapsulatedEvent that is
 * encapsulating a FooEvent, delivered to the FooListener interface method:
 * <code> void fooHappened(FooEvent fe) throws BadFooException; </code>
 * then in order for the EncapsulatedEventListener to throw the
 * BadFooException it should invoke:
 * <code>  throw new EncapsulatedEventException(new BadFooException()); </code>
 */

public class EncapsulatedEventException extends RuntimeException {
	
    protected Exception exception;

    /**
     * public constructor
     */

    public EncapsulatedEventException(Exception e) {
    	this(e, null);
    }

    /**
     * public constructor
     */

    public EncapsulatedEventException(Exception e, String s) {
	super(s);

    	if (e == null)
    		throw new IllegalArgumentException("null exception param");

    	exception = e;
    }

    /**
     * @returns the Exception Object itself
     */

    public Exception getException() { return exception; }

    /**
     * @returns the Class of the Exception Object itself.
     */

    public Class getExceptionClass() { return exception.getClass(); }

    /** 
     * @returns the name of the Class of the Exception Object itself.
     */

    public String getExceptionClassName() {
	return exception.getClass().getName(); 
    }
}
