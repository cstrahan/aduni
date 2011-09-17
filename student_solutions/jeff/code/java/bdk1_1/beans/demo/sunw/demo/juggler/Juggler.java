
package sunw.demo.juggler;

/** 
 * A simple JavaBean demonstration class that displays an animation 
 * of Duke juggling a couple of coffee beans.    The Juggler class 
 * is a good simple example of how to write readObject/writeObject 
 * serialization methods that restore transient state.    In this case 
 * the transient state is an array of images and a Thread.
 */

import java.applet.Applet;
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.net.URL;
import java.beans.*;
import java.beans.beancontext.*;
import java.beans.DesignMode.*;
import sunw.demo.methodtracer.*;

public class Juggler extends Applet implements Runnable, BeanContextProxy,
                                    BeanContextServicesListener, 
                                    PropertyChangeListener, DesignMode {
    private transient Image[] images;
    private transient Thread animationThread;
    private int rate = 125;
    private transient int loop;
    private boolean stopped = true;
    private boolean debug = false;
    private boolean dmode = false;
    private transient MethodTracer mtService;
    private transient MethodTracer mt;

    private BeanContextChildSupport bccs = new BeanContextChildSupport() {

	protected void initializeBeanContextResources() {

	    try {

		// Get method tracing service if it's available.
		BeanContextServices bcs = (BeanContextServices)bccs.getBeanContext();
	    	if (bcs.hasService(MethodTracer.class)) {
		    mtService = (MethodTracer)bcs.getService( 
					getBeanContextProxy(), Juggler.this, 
					MethodTracer.class, null, Juggler.this);
	        } else {
		    bcs.addBeanContextServicesListener(Juggler.this);
		}

	    	// Allow nesting BeanContext control design/runtime mode.
	    	bcs.addPropertyChangeListener("designMode", Juggler.this);

	    } catch (ClassCastException ex) {
		// Nesting BeanContext is not a BeanContextServices
		// so do nothing.
	    } catch (Exception e) {
		System.err.println("Error initializing BeanContext resources.");
		System.err.println(e);
	    } 
	}

	protected void releaseBeanContextResources() {
	    if (mtService != null) { mtService = mt = null; }
            try {
	    	BeanContextServices bcs = (BeanContextServices)getBeanContext();
		bccs.removePropertyChangeListener("designMode", Juggler.this);
		bcs.removeBeanContextServicesListener(Juggler.this);
	    } catch (Exception ex) {
	    } 
	}
    };

    public BeanContextChild getBeanContextProxy() {
	return bccs;
    }

    /** 
     * Applet method: start the Juggler applet.
     */

    public synchronized void start() {
	startJuggling();
    }

    /** 
     * Applet method: stop the Juggler applet.
     */

    public synchronized void stop() {
	stopJuggling();
    }

    /** 
     * Initialize the Juggler applet.
     */

    private void initialize() {

        // Load the image resources:
	images = new Image[5];
        for (int i = 0; i < 5; i++) {
            String imageName = "Juggler" + i + ".gif";
            images[i] = loadImage(imageName);
            if (images[i] == null) {
		System.err.println("Couldn't load image " + imageName);
		return;
            }
        }
    }


    /**
     * This is an internal utility method to load GIF icons.
     * It takes the name of a resource file associated with the
     * current object's class-loader and loads a GIF image
     * from that file.
     * <p>
     * @param resourceName    A pathname relative to the DocumentBase
     *		of this applet, e.g. "wombat.gif".
     * @return    a GIF image object.    May be null if the load failed.
     */
    private java.awt.Image loadImage(String name) {
        if (mt != null) mt.traceMethod();
	try {
	    java.net.URL url = getClass().getResource(name);
	    return createImage((java.awt.image.ImageProducer) url.getContent());
	} catch (Exception ex) {
	    return null;
	}
    }


    /** 
     * Draw the current frame.
     */
    public void paint(Graphics g) {
        if (mt != null) mt.traceMethod();
	int index = (loop%4) + 1;
        // If the animation is stopped, show the startup image.
        if (stopped) {
	    index = 0;
	}
	if (images == null || index >= images.length) {
	    return;
	}
	Image img = images[index];
	if (img != null) {
	    g.drawImage(img, 0, 0, this);
	}
    }


    /** 
     * If false, suspend the animation thread.
     */
    public synchronized void setEnabled(boolean x) {
        if (mt != null) mt.traceMethod();
        super.setEnabled(x);
        notify();
    }


    /** 
     * Resume the animation thread if we're enabled.
     * @see #stopJuggling
     * @see #setEnabled
     */
    public synchronized void startJuggling() {
        if (mt != null) mt.traceMethod(); 
        if (images == null) {
	    initialize();
	}
	if (animationThread == null) {
            animationThread = new Thread(this);
            animationThread.start();
	}
        stopped = false;
        notify();
    }

    /** 
     * Suspend the animation thread if neccessary.
     * @see #startJuggling
     * @see #setEnabled
     */
    public synchronized void stopJuggling() {
        if (mt != null) mt.traceMethod(); 
        stopped = true;
	loop = 0;
	// Draw the stopped frame.	
        Graphics g = getGraphics();
	if (g == null || images == null) {
	    return;
	}
	Image img = images[0];
	if (img != null) {
	    g.drawImage(img, 0, 0, this);
	}
    }


    /** 
     * An event handling method that calls startJuggling.    This method
     * can be used to connect a Button or a MenuItem to the Juggler.
     *
     */
    public void startJuggling(ActionEvent x) {
        startJuggling();
    }

    /** 
     * This method can be used to connect a Button or a MenuItem 
     * to the Juggler.stopJuggling method.
     */
    public void stopJuggling(ActionEvent x) {
        stopJuggling();
    }


    /** 
     * Returns false if the Juggler is stopped, true otherwise.
     */
    public boolean isJuggling() {
	return stopped;
    }

    public int getAnimationRate() {
        return rate;
    }
    
    public void setAnimationRate(int x) {
        rate = x;
    }


    public Dimension getMinimumSize() {
        return new Dimension(144, 125);
    }

    /**
     * @deprecated provided for backward compatibility with old layout managers.
     */
    public Dimension minimumSize() {
	return getMinimumSize();
    }

    public Dimension getPreferredSize() {
        return minimumSize();
    }

    /**
     * @deprecated provided for backward compatibility with old layout managers.
     */
    public Dimension preferredSize() {
	return getPreferredSize();
    }

    /** 
     * Returns true if debugging is enabled, false if it's not.
     */
    public boolean isDebug() {
        return debug;
    }

    /** 
     * Turns debugging on, only if a MethodTracer service is available
     * and we are in design mode.
     */
    public void setDebug( boolean debug) {
        if (debug) {
            if (isDesignTime() && (mtService != null)) {
                mt = mtService;
		this.debug = true;
            } else if (mtService == null) {
                System.err.println("MethodTracer service not available.");
                this.debug = false;
            } else if (!isDesignTime()) {
		System.err.println("Debugging not available during runtime.");
                this.debug = false;
	    }
        } else {
            mt = null;
	    this.debug = false;
        }
    }

    /* 
     * PropertyChangeListener method.  Currently only listen for designMode.
     */
    public void propertyChange( PropertyChangeEvent evt) {
    	if (evt.getPropertyName().equals("designMode")) {
	    boolean dmode = (boolean)((Boolean)evt.getNewValue()).booleanValue();
	    setDesignTime(dmode);
	}
    }

    /*
     * If switching to runtime, turn off method tracing if it was enabled.
     * If switching to design time and debugging is true, then enable
     * method tracing if the service is available.
     */
    public void setDesignTime(boolean dmode) {
	this.dmode = dmode;
	if (dmode) {
	    if (isDebug() && (mtService != null)) {
		mt = mtService;
	    } 
	} else if (!dmode && (mt != null)) {
	    mt = null;
	}
    }
        
   /*
    * Returns true if we're in design mode, false if in runtime mode.
    */
    public boolean isDesignTime() {
	return dmode;
    }
        
    /* 
     * BeanContextServicesListener methods.
     */
    public void serviceRevoked( BeanContextServiceRevokedEvent bcsre) {
        System.err.println("Method Tracing service revoked.");
        setDebug( false);
        mtService = null;
    }

    public void serviceAvailable( BeanContextServiceAvailableEvent bcsae) {
    	if (bcsae.getServiceClass() == MethodTracer.class) {
    	    // MethodTracer service has just become available.
	    try {
		mtService = (MethodTracer)bcsae.getSourceAsBeanContextServices().getService( getBeanContextProxy(), this, MethodTracer.class, null, this);
	    } catch ( Exception ex) {
		System.err.println(ex);
	    } 
	}
    }

    public void run() {
        if (mt != null) mt.traceMethod(); 
        try {
            while(true) {
		// First wait until the animation is not stopped.
		synchronized (this) {
	 	   while (stopped || !isEnabled()) {
	                wait();
	            }
		}
		loop++;
	        // Now draw the current frame.
	        Graphics g = getGraphics();
		Image img = images[(loop % 4) + 1];
	     	if (g != null && img != null) {
		    g.drawImage(img, 0, 0, this);
		}
		Thread.sleep(rate);
            }
        } catch (InterruptedException e) {
        }
    }

}
