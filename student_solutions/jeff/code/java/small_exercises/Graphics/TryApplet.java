import javax.swing.*;
import java.awt.*;
import javax.swing.border.*;

public class TryApplet extends JApplet
{
    public void init()
    {
	Container content = getContentPane();
	content.setLayout(new FlowLayout());

	JButton button; // Stores a button
	Font[] fonts = { new Font("Arial", Font.ITALIC, 10),
			 new Font("Playbill", Font.PLAIN, 14)};

	BevelBorder edge = new BevelBorder(BevelBorder.RAISED); // bevelled bordeer

	// Add the buttons using alternate fonts
	for(int i = 1; i <= 6; i++)
	    {
		content.add(button = new JButton("Press " + i));
		button.setFont(fonts[i%2]);
		button.setBorder(edge);
	    }
    }
}
	
