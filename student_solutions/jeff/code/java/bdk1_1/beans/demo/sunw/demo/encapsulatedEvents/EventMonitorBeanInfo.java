/*
 *
 * @(#) EventMonitorBeanInfo.java 1.5@(#)
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
 * sunw.demo.encapsulatedEvents.EventMonitorBeanInfo
 * </p>
 *
 * @version 1.0
 * @author Laurence P. G. Cable.
 */

package sunw.demo.encapsulatedEvents;

import java.beans.MethodDescriptor;
import java.beans.SimpleBeanInfo;

import java.lang.reflect.Method;

/**
 *
 * <p>
 * BeanInfo for the EventMonitor exposes Listeners for registration.
 * </p>
 */

public final class EventMonitorBeanInfo extends SimpleBeanInfo { 

    /**
     * @return the public methods for the EventMonitor Bean
     */

    public MethodDescriptor[] getMethodDescriptors() {

	// First find the "method" object.
	Class args[] = { java.util.EventObject.class };
	Method m;
	try {
	    m = EventMonitor.class.getMethod("initiateEventSourceMonitoring", args);
	} catch (Exception ex) {
	    // "should never happen"
	    throw new Error("Missing method: " + ex);
	}

	// Now create the MethodDescriptor array:
	MethodDescriptor result[] = new MethodDescriptor[1];
	result[0] = new MethodDescriptor(m); 
	return result;
    }
}
