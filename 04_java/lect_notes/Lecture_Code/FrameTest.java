import javax.swing.*;
import java.awt.*;
import java.awt.geom.*;
import java.awt.event.*;

/**
 * Main class for game. Does window stuff and display
 */

class MyFrame extends JFrame{

    public MyFrame(){
	setTitle("MyFrame");
	setSize(200,200);        // size in pixels
	setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }

}



class MyPanel extends JPanel{

    Color current = Color.red;

    // Handler for Mouse presses and clicks
    //  Extends MouseAdapator 
    //      (which implements MouseListener with empty methods)
    // We use an inner class so we can access to data on JPanel
    class MouseHandler extends MouseAdapter{
	
	// call on  mouse button down
	public void mousePressed(MouseEvent ev){
	    int x = ev.getX();
	    int y = ev.getY();
	    System.out.println("Pressed at " + x + "," + y);
	}

	public void mouseClicked(MouseEvent ev){
	    int x = ev.getX();
	    int y = ev.getY();
	    System.out.println("Clicked at " + x + "," + y);
	    if(current.equals(Color.red))
		current = Color.blue;
	    else
		current = Color.red;
	    repaint();
	}

    }

    MyPanel(){
	addMouseListener(new MouseHandler());
    }

    public void paintComponent(Graphics g){
	super.paintComponent(g);

	//	    System.out.println("paintComponent");
	g.setColor(current);
	g.drawString("Hello World",50,50);
    }

}


public class FrameTest{

    // OK let's have our main() create a frame.
    public static void main(String[] args){
	MyFrame myframe = new MyFrame();
	MyPanel mypanel = new MyPanel();
	// Random stuff we just have to do (the book explains, kind of)
	Container contentPane = myframe.getContentPane();
	// add panel
	contentPane.add(mypanel);

	myframe.show();
    }
}

/*


 */

