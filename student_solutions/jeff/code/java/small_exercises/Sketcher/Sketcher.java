// Sketcher application as an Applet
import java.awt.*;
// import javax.swing.*;
import java.awt.event.*;

public class Sketcher 
{
    public static void main(String[] args)
    {
	theApp = new Sketcher();
	theApp.init();
    }

    public void init()
    {
	window = new SketchFrame("Sketcher");
	Toolkit theKit = window.getToolkit();
	Dimension wndSize = theKit.getScreenSize();

	// Set the position to screen center & size to half screen size
	window.setBounds(wndSize.width/6, wndSize.height/6,  // Position
			 2*wndSize.width/3, 2*wndSize.height/3); // Size

	window.addWindowListener(new WindowHandler());
	window.setVisible(true);
    }
    // Handler for window closing event
    class WindowHandler extends WindowAdapter
    {
	// Handler for window closing events
	public void windowClosing(WindowEvent e)
	
	{
	    window.dispose();
	    System.exit(0);
	}
    }

    private static SketchFrame window;
    private static Sketcher theApp;
}
	
