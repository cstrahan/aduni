import javax.swing.*;
import java.awt.*;
import javax.swing.border.*;

public class TryBorderLayout
{
    // The window object
    static JFrame aWindow = new JFrame("This is a Border Layout");

    public static void main(String[] args)
    {
	Toolkit theKit = aWindow.getToolkit();
	Dimension wndSize = theKit.getScreenSize();

	// Set the position to screen center & size to half screen size
	aWindow.setBounds(wndSize.width/4, wndSize.height/4, // position
			  wndSize.width/2, wndSize.height/2); // size
	aWindow.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);


	BorderLayout border = new BorderLayout(); // Create a layout manager
	Container content = aWindow.getContentPane();
	content.setLayout(border);                           // Set the container layout mgr

	EtchedBorder edge = new EtchedBorder(EtchedBorder.RAISED); // Button border
	// Now add five Jbutton components and set their borders
	JButton button;
	content.add(button = new JButton("EAST"), BorderLayout.EAST);
	button.setBorder(edge);
	content.add(button = new JButton("WEST"), BorderLayout.WEST);
	button.setBorder(edge);
	content.add(button = new JButton("NORTH"), BorderLayout.NORTH);
	button.setBorder(edge);
	content.add(button = new JButton("SOUTH"), BorderLayout.SOUTH);
	button.setBorder(edge);
	content.add(button = new JButton("CENTER"), BorderLayout.CENTER);
	button.setBorder(edge);

	aWindow.setVisible(true);
    }
}
			  
