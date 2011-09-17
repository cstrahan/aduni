/*
 *
 * @(#) MethodTracer.java 1.3@(#)
 *
 * Copyright (c) 1999 Sun Microsystems, Inc. All Rights Reserved.
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
 * sunw.demo.methodtracer.MethodTracer.java
 * </p>
 *
 */

package sunw.demo.methodtracer;

import java.awt.*;
import java.io.*;
import java.beans.*;
import java.beans.DesignMode.*;

/**
 *
 * <p>
 * The MethodTracer performs method tracing or text message display 
 * for beans requesting this service.  The text is displayed to the
 * MethodTracer's TextArea component.
 * Note:  Since there is no specification for stack trace output, 
 * this may not work on all virtual machines.  
 * </p>
 *
 */

public final class MethodTracer extends Frame 
		   implements PropertyChangeListener, DesignMode { 

    private TextArea ta;
    protected boolean useGui;

    /** 
     * MethodTracer constructor
     */

    public MethodTracer() {
        super( "Method Tracer");
        setBackground(Color.lightGray);
        ta = new TextArea( "Method tracing service started.\n", 7, 50,
                           TextArea.SCROLLBARS_BOTH);
        add(ta);
        ta.setBackground(Color.lightGray);
        ta.setEditable(false);
        pack();
    }

    /**
     * Output a string indicating what method of what class this was 
     * invoked from.  
     */

    synchronized public void traceMethod() {
        try {
            StringWriter sw = new StringWriter();
            (new Exception()).printStackTrace(new PrintWriter(sw));
            StringReader sr = new
                StringReader(sw.getBuffer().toString());
            BufferedReader br = new BufferedReader(sr);
 
            br.readLine();
            br.readLine(); // discard first two lines
            String line = br.readLine();
            line = line.substring("  at".length());
            line = line.substring(0, line.indexOf('('));
            ta.append( line);
            ta.append( "\n");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Output the specified string to the MethodTracer window.
     */

    synchronized public void logText( String line) {
        ta.append( line);
        ta.append( "\n");
    }

    /**
     * Turn off visibility of MethodTracer window if execution 
     * environment is switched to runtime and turn on visibility 
     * if switched to design mode.
     */

    public void propertyChange( PropertyChangeEvent evt) {
        if (evt.getPropertyName().equals("designMode")) {
	    boolean dmode = ((Boolean)evt.getNewValue()).booleanValue();
	    setDesignTime(dmode);
	}
    }

    /**
     * Turn off visibility of MethodTracer window if false and turn on 
     * visibility if true.
     */

    public void setDesignTime( boolean dmode) {
	if (this.isVisible() != dmode) setVisible(dmode);
    }

    /**
     * Return true if MethodTracer window is visible and false if it
     * is not visible.
     */

    public boolean isDesignTime() {
	return isVisible();
    }

}
