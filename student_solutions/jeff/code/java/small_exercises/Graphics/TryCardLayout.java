import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class TryCardLayout extends JApplet
    implements ActionListener // for event handing
{
    CardLayout card = new CardLayout(50,50);

    public void init()
    {
	Container content = getContentPane();
	content.setLayout(card);
	JButton button;

	for(int i = 1; i <= 6; i++)
	    {
		content.add(button = new JButton("Press " + i), "Card" + i); // Add a button
		button.addActionListener(this);
	    }
    }

    // Handle button events
    public void actionPerformed(ActionEvent e)
    {
	card.next(getContentPane()); // Switch to the next card
    }
}

	
			    
	    
	       
