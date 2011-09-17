import javax.swing.*;
import java.awt.*;
import javax.swing.border.*;

public class TryBoxLayout
{
    // The window object
    static JFrame aWindow = new JFrame("This is a Box Layout");

    public static void main(String[] args)
    {
	Toolkit theKit = aWindow.getToolkit(); // Get the window Toolkit
	Dimension wndSize = theKit.getScreenSize(); // Get the screen size

	// Set the position to screen center & size to half screen size
	aWindow.setBounds(wndSize.width/4, wndSize.height/4,        // Position
			  wndSize.width/2, wndSize.height/2);       // Size
	aWindow.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

	// Create left column of radio buttons
	Box left = Box.createVerticalBox();
	left.add(Box.createVerticalStrut(30));
	ButtonGroup radioGroup = new ButtonGroup(); // Create button group
	JRadioButton rbutton; // stores a button
	radioGroup.add(rbutton = new JRadioButton("Red"));
	left.add(rbutton);  // adds a button
	left.add(Box.createVerticalStrut(30));
	radioGroup.add(rbutton = new JRadioButton("Green"));
	left.add(rbutton);  // adds a button
	left.add(Box.createVerticalStrut(30));
	radioGroup.add(rbutton = new JRadioButton("Blue"));
	left.add(rbutton);  // adds a button
	left.add(Box.createVerticalStrut(30));
	radioGroup.add(rbutton = new JRadioButton("Yellow"));
	left.add(rbutton);  // adds a button
	left.add(Box.createGlue());
	
	// Create a panel with a titled border to hold the let Box container
	JPanel leftPanel = new JPanel(new BorderLayout());
	leftPanel.setBorder(new TitledBorder
	    (new EtchedBorder(), "Line Color")); // Border to use, name
	leftPanel.add(left, BorderLayout.CENTER);

	// Create right columns of checkboxes
	Box right = Box.createVerticalBox();
	right.add(Box.createVerticalStrut(30));
	right.add(new JCheckBox("Dashed"));
	right.add(Box.createVerticalStrut(30));
	right.add(new JCheckBox("Thick"));
	right.add(Box.createVerticalStrut(30));
	right.add(new JCheckBox("Rounded"));
	right.add(Box.createGlue());

	// Create a panel with a titled border to hold the right Box container
	JPanel rightPanel = new JPanel(new BorderLayout());
	rightPanel.setBorder(new TitledBorder
	    (new EtchedBorder(), "Line Properties"));
	rightPanel.add(right, BorderLayout.CENTER);

	// Create top row to hold left and right
	Box top = Box.createHorizontalBox();
	top.add(leftPanel);
	top.add(Box.createHorizontalStrut(5));
	top.add(rightPanel);

	// Create bottom row of buttons
	JPanel bottomPanel = new JPanel();
	bottomPanel.setBorder
	    (new CompoundBorder
		(BorderFactory.createLineBorder(Color.black, 1), // Outer
		 BorderFactory.createBevelBorder(BevelBorder.RAISED))); // Inner
	Border edge = BorderFactory.createRaisedBevelBorder(); // Button border
	JButton button;
	Dimension size = new Dimension(80,20);
	bottomPanel.add(button = new JButton("Defaults"));
	button.setBorder(edge);
	button.setPreferredSize(size);
	bottomPanel.add(button = new JButton("OK"));
	button.setBorder(edge);
	button.setPreferredSize(size);
	bottomPanel.add(button = new JButton("Cancel"));
	button.setBorder(edge);
	button.setPreferredSize(size);

	// Add top and bottom panel to content pane
	Container content = aWindow.getContentPane(); // Get content pane
	content.setLayout(new BorderLayout()); // Set border layout mgr
	content.add(top, BorderLayout.CENTER);
	content.add(bottomPanel, BorderLayout.SOUTH);
	
	aWindow.setVisible(true);
    }
}


