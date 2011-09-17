// Sketcher application as an Applet
import java.awt.*;
import javax.swing.*;
import java.awt.event.*;

public class Sketcher extends JApplet implements WindowListener
{
    public static void main(String[] args)
    {
	theApp = new Sketcher2();
	theApp.init();
    }

    public void init()
    {
	window = new SketchFrame("Sketcher");
	Toolkit theKit = window.getToolkit();
	Dimension wndSize = theKit.getScreenSize();

	// Set the position to screen center & size to half screen size
	window.setBounds(wndSize.width/4, wndSize.height/4,  // Position
			 wndSize.width/2, wndSize.height/2); // Size

	window.setVisible(true);
    }

    private static SketchFrame window;
    private static Sketcher2 theApp;
}
	
