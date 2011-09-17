import javax.swing.*;
import java.awt.*;
import javax.swing.border.*;

public class TryGridLayout
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

	GridLayout grid = new GridLayout(5,2,30,20); // Create a layout manager
	Container content = aWindow.getContentPane();
	content.setLayout(grid);

	EtchedBorder edge = new EtchedBorder(EtchedBorder.RAISED); // Button border

	// Now add ten Button compenents
	JButton button;
	for(int i = 1; i <= 10; i++)
	    {
		content.add(button = new JButton("Press " + i));
		button.setBorder(edge);
	    }

	aWindow.getContentPane().setBackground(new Color(238,233,233));
	aWindow.setVisible(true);
    }
}
