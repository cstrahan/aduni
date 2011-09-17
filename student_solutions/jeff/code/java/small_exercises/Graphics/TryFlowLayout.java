import javax.swing.*;
import java.awt.*;
import javax.swing.border.*;

public class TryGridBagLayout
{
    // The window object
    static JFrame aWindow = new JFrame("This is a Gridbag Layout");

    public static void main(String[] args)
    {
	Toolkit theKit = aWindow.getToolkit();
	Dimension wndSize = theKit.getScreenSize();

	// Set the position to screen center & size to half screen size
	aWindow.setBounds(wndSize.width/4, wndSize.height/4, // position
			  wndSize.width/2, wndSize.height/2); // size
	aWindow.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

	GridBagLayout gridbag = new GridBagLayout();
	GridBagContstraints constraints = new GridBagConstraints();
	aWindow.getContentPane().setLayout(gridbag);

	// Set constaints and add first button
	constraints.weightx = constraints.weighty = 10.0;
	constraints.fill = constraints.BOTH;
	addButton("Press", constraints, gridbag);

	// Set constraints and add second button
	constraints.gridwidth = constraints.REMAINDER; // Rest of the row
	addButton("GO", constraints, gridbag);

	aWindow.setVisible(true);	
    }

    static void addButton(String label,
			  GridBagConstraints constraints, GridBagLayout layout)
    {
	// Create a Border object using a BorderFactory method
	Border edge = BorderFactory.createRaisedBevelBorder();

	JButton button = new JButton(label);
	button.setBorder(edge);
	layout.setConstraints(button, constraints);
	aWindow.getContentPane().add(button);
    }
}
			  
