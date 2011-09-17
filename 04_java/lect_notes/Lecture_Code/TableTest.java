import javax.swing.*;
import javax.swing.table.*;
import java.awt.*;
import java.awt.geom.*;
import java.awt.event.*;

/**
 * Main class for game. Does window stuff and display
 */

class MyFrame extends JFrame{

    public MyFrame(){
	// set up UI
	setTitle("MyFrame");
	setSize(200,200);        // size in pixels
	setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	// make model
	MyTableModel model = new MyTableModel(31);
	// make table
	JTable table = new JTable(model);
	// Random stuff we just have to do (the book explains, kind of)
	Container contentPane = getContentPane();
	// add panel with scroller
	JScrollPane scroll = new JScrollPane(table);
	contentPane.add(scroll);

    }

}

class MyTableModel extends AbstractTableModel{
    int rows = 2;
    int cols = 2;
    String[] colnames = {"Day","Hours"};
    Integer[] hours;
    MyTableModel(int rows){
	this.rows = rows;
	hours = new Integer[rows];
	for(int i=0;i<rows;i++){
	    hours[i] = new Integer((int)(Math.random() * 8.0));
	}
    }
    
    public int getColumnCount(){
	return(cols);
    }
    public String getColumnName(int c){
	return(colnames[c]);
    }
    public int getRowCount(){
	return(rows);
    }
    public Object getValueAt(int r, int c){
	if(c == 0) return(new Integer(r));
	else return(hours[r]);
    }
    public void setValueAt(Object value, int r, int c){
	hours[r] = new Integer((String)value);
    }
    // make second column editable
    public boolean isCellEditable(int r, int c){
	if(c > 0) return(true);
	return(false);
    }
}

public class TableTest{

    // OK let's have our main() create a frame.
    public static void main(String[] args){
	MyFrame myframe = new MyFrame();
	myframe.show();
    }
}

/*


 */

