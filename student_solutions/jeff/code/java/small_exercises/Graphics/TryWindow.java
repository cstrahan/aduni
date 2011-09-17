import javax.swing.*;
import java.awt.*;

public class TryWindow
{
    // The window object
    static JFrame aWindow = new JFrame("Arma virumque cano");

    public static void main(String[] args)
    {
	Toolkit theKit = aWindow.getToolkit(); // Get the window Toolkit
	Dimension wndSize = theKit.getScreenSize(); // Get the screen size

	// Set the position to screen center & size to half screen size
	aWindow.setBounds(wndSize.width/4, wndSize.height/4,        // Position
			  wndSize.width/2, wndSize.height/2);       // Size
	aWindow.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
	aWindow.setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));
	aWindow.getContentPane().setBackground(new Color(238,233,233));
	aWindow.setVisible(true);
    }
}
