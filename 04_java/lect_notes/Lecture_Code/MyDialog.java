import javax.swing.*;
import java.awt.*;
import java.awt.geom.*;
import java.awt.event.*;

/**
 * Main class for game. Does window stuff and display
 */

public class MyDialog extends JDialog{

    // must make instance var since we need it to pass data
    DialogPanel dialogPanel;

    public MyDialog(JFrame parent){
	// must call JDialog constructor, 
	//  final arg true makes show() blocking.
	super(parent,"Color Dialog",true);
	setSize(200,200);        // size in pixels
	// new default close action -- equiv to setVisible(false)
	setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	// Add panel
	dialogPanel= new DialogPanel(this);
	getContentPane().add(dialogPanel);
    }

    public Color getCurrent(){
	// just forward request
	return(dialogPanel.getCurrent());
    }

}



class DialogPanel extends JPanel{
    // the dialog that contains me, needed to call isVisible(false)
    JDialog dialog;

    // This color is not used to draw, it is just the 'model' that
    //  this dialog alters. This data is retrieved by the main app
    //  through the getCurrent9) method
    Color current = Color.black;

    // Handler for Mouse presses and clicks
    //  Extends MouseAdapator 
    //      (which implements MouseListener with empty methods)
    // We use an inner class so we can access to data on JPanel

    JButton toggle = new JButton("Toggle");
    JButton green = new JButton("Green");
    JButton ok = new JButton("OK");


    // Button listener. Now an inner class so we
    //  can access mypanel data.
    class ButtonHandler implements ActionListener{
	public void actionPerformed(ActionEvent e){
	    // check source, if green button, set color
	    if(e.getSource() == green){
		current = Color.green;
	    }
	    else if(e.getSource() == toggle){ // do toggle stuff
		if(current.equals(Color.red))
		    current = Color.blue;
		else
		    current = Color.red;
	    }
	    else{ // OK
		// setVisible(false) close dialog and causes show() to return!
		dialog.hide();
	    }
	}
	
    }

    DialogPanel(JDialog dialog){
	// make one instance of listener and add to all
	this.dialog = dialog;
	ButtonHandler b = new ButtonHandler();
	toggle.addActionListener(b);
	green.addActionListener(b);
	ok.addActionListener(b);
	// add buttons (and text field)
	add(toggle);
	add(green);
	add(ok);
    }

    public Color getCurrent(){
	return(current);
    }

}

