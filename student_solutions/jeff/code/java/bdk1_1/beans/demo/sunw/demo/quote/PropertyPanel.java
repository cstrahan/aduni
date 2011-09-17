package sunw.demo.quote;

import java.awt.*;
import java.awt.event.*;
import java.util.Hashtable;


public class PropertyPanel extends Panel
{
  GridBagLayout g = new GridBagLayout();
  GridBagConstraints c = new GridBagConstraints();
  Hashtable propertyTable = new Hashtable();
  Panel buttonPanel = new Panel();

  private Font font = null;
  private Color backgroundColor = null;
  private Color foregroundColor = null;


  PropertyPanel()
  {
    super();
    setLayout(g);

    c.weighty = 0;
    c.gridheight = 1;
    c.insets = new Insets(1, 1, 1, 1);  // top left bottom right

    buttonPanel.setLayout(new FlowLayout());

    c.weightx = 1;
    c.weighty = 1;
    c.gridwidth = GridBagConstraints.REMAINDER;
    c.anchor = GridBagConstraints.CENTER;
    c.fill = GridBagConstraints.HORIZONTAL;
    g.setConstraints(buttonPanel, c);
    add(buttonPanel);

    c.weighty = 0;
  }


  public void appendProperty(String name, String value)
  {
    Label label = new Label(name + ":");
    label.setText(name + ":");
    c.anchor = GridBagConstraints.EAST;
    c.fill = GridBagConstraints.NONE;
    c.weightx = 0;
    c.gridwidth = 1;
    g.setConstraints(label, c);
    add(label, getComponentCount() - 1);

    TextField text = new TextField(value);
    text.setEditable(false);
    c.weightx = 1;
    c.gridwidth = GridBagConstraints.REMAINDER;
    c.anchor = GridBagConstraints.WEST;
    c.fill = GridBagConstraints.HORIZONTAL;
    g.setConstraints(text, c);
    add(text, getComponentCount() - 1);

    propertyTable.put(name, text);
  }


  public void setFont(Font x)
  {
    super.setFont(x);
    Component[] v = getComponents();
    for(int i = 0; i < v.length; i++)
      v[i].setFont(x);
  }


  public void setForeground(Color x)
  {
    super.setForeground(x);
    Component[] v = getComponents();
    for(int i = 0; i < v.length; i++)
      v[i].setForeground(x);
  }


  public void setBackground(Color x)
  {
    Component[] v = getComponents();
    for(int i = 0; i < v.length; i++)
      v[i].setBackground(x);
  }


  public void appendProperty(String name)
  {
    appendProperty(name, "");
  }


  public void appendButton(Button x)
  {
    buttonPanel.add(x);
  }


  public void removeButton(Button x)
  {
    buttonPanel.remove(x);
  }


  public void setPropertyValue(String name, String value)
  {
    TextField text = (TextField)propertyTable.get(name);
    text.setText(value);
  }


  public String getPropertyValue(String name)
  { 
    TextField text = (TextField)propertyTable.get(name);
    return text.getText();
  }

  
  public boolean isPropertyEditable(String name)
  {
    TextField text = (TextField)propertyTable.get(name);
    return text.isEditable();
  }


  public void setPropertyEditable(String name, boolean x)
  {
    TextField text = (TextField)propertyTable.get(name);
    text.setEditable(x);
  }

  
  public void addPropertyActionListener(String name, ActionListener l) 
  {
    TextField text = (TextField)propertyTable.get(name);
    text.addActionListener(l);
  }

  public void removePropertyActionListener(String name, ActionListener l)
  {
    TextField text = (TextField)propertyTable.get(name);
    text.removeActionListener(l);
  }
}

