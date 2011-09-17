// Frame for the Sketcher application
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
                                
public class SketchFrame extends JFrame implements Constants
{
  // Constructor
  public SketchFrame(String title)
    {
      
      setTitle(title);          
      setJMenuBar(menuBar);     
      
      JMenu fileMenu = new JMenu("File");
      JMenu elementMenu = new JMenu("Elements");
      fileMenu.setMnemonic('F');
      elementMenu.setMnemonic('E');

      // We will construct the file pull down menu here using actions...

      // We will add the types menu items here using actions...

      elementMenu.addSeparator();

      JMenu colorMenu = new JMenu("Color");
      elementMenu.add(colorMenu);

      // We will add the color menu items here using actions...

      menuBar.add(fileMenu);
      menuBar.add(elementMenu);
    }

  // We will add inner classes defining action objects here...
  class FileAction extends AbstractAction
  {
    // Contructor
    FileAction(String name)
    {
      super(name);
    }

    public void actionPerformed(ActionEvent e)
    {
      // We will add action code here eventually
    }
  }

zz  class TypeAction extends AbstractAction
  {
    TypeAction(String name, int typeID)
    {
      super(name);
      this.typeID = typeID;
    }

    public void actionPerformed(ActionEvent e)
    {
      elementType = typeID;
    }

    private int typeID;
  }
      
  // We will add action objects as members here...
  private FileAction newAction, openAction, closeAction,
    saveAction, saveAsAction, printAction;
  private JMenuBar menuBar = new JMenuBar();
  private Color elementColor = DEFAULT_ELEMENT_COLOR;
  private int elementType = DEFAULT_ELEMENT_TYPE;
}


  
  
  
