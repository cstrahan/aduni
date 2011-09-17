/**
 * This is the bridge tester bean.
 *
 * Simply define all property types, fire evemts and define a customizer
 */

package sunw.demo.test;

import java.awt.*;
import java.beans.*;
import java.awt.event.*;

public class BridgeTester extends Panel
                implements ActionListener, KeyListener, MouseListener {

    public BridgeTester() {

	setLayout(new GridLayout(6,4,10,10));
	event1Button = new Button("Fire Event 1 ");
	event1Button.addActionListener(this);
	add(event1Button);
	event2Button = new Button("Fire Event 2 ");
	event2Button.addActionListener(this);
	add(event2Button);
	event3Button = new Button("Fire Event 3 ");
	event3Button.addActionListener(this);
	add(event3Button);
	event4Button = new Button("Fire Event 4 ");
	event4Button.addActionListener(this);
	add(event4Button);

	add(new Label("Int Value : "));
	intField = new TextField();
	intField.addKeyListener(this);
	add(intField);

	doubleField = new TextField();
	add(new Label("Double Value : "));
	doubleField.addKeyListener(this);
	add(doubleField);

	add(new Label("Short Value :"));
	shortField = new TextField();
	shortField.addKeyListener(this);
	add(shortField);

	add(new Label("Float Value :"));
	floatField = new TextField();
	floatField.addKeyListener(this);
	add(floatField);

	add(new Label("Char Value :"));
	charField = new TextField();
	charField.addKeyListener(this);
	add(charField);

	add(new Label("Byte Value :"));
	byteField = new TextField();
	byteField.addKeyListener(this);
	add(byteField);

	add(new Label("String value :"));
	stringField = new TextField();
	stringField.addKeyListener(this);
	add(stringField);

	add(new Label("Boolean Value :"));
	booleanField = new Checkbox();
        booleanField.addMouseListener(this);
	add(booleanField);

	array1 = new TextField();
	add(array1);
	array2 = new TextField();
	add(array2);
	array3 = new TextField();
	add(array3);
	array4 = new TextField();
	add(array4);	

	int[] myIntArray = { 1 , 2, 3, 4, 5 };
	setIntArray(myIntArray);
	double[] myDoubleArray = {1.1, 2.2, 3.3, 4.4, 5.5};
	setDoubleArray(myDoubleArray);
       	String[] myStringArray = {"TOTO", "s'en", "va", "en", "guerre"};
        setStringArray(myStringArray);
        float[] myFloatArray = { (float) 1.01,(float)  2.02,(float)  3.03,(float)  4.04,(float)  5.05 };
	setFloatArray(myFloatArray);
	char[] myCharArray = { 'A', 'B', 'c', 'd', 'E' };
	setCharArray(myCharArray);
	byte[] myByteArray = {1, 2, 3, 4, 5};
	setByteArray(myByteArray);
	short[] myShortArray = {5, 4, 3, 2, 1};
	setShortArray(myShortArray);
	boolean[] myBooleanArray = {true, false, false, true, false};
	setBooleanArray(myBooleanArray);
    }

    public java.awt.Dimension getPreferredSize() {
	return new java.awt.Dimension(400,200);
    }

    /**
     * @deprecated provided for backward compatibility with old layout managers.
     */
    public Dimension preferredSize() {
	return getPreferredSize();
    }

    public void setIntValue(int newValue)  throws PropertyVetoException {

	// Fires first the Vetoable event, if it's vetoed the following code
	// won't happen
	int oldValue = intValue;
	vetos.fireVetoableChange("intValue", new Integer(oldValue), new Integer(newValue));
	intValue = newValue;
	intField.setText(String.valueOf(intValue));
	changes.firePropertyChange("intValue", new Integer(oldValue), new Integer(newValue));
    }

    public int getIntValue() {
	return intValue;
    }

    public void setDoubleValue(double newValue) {    
	double oldValue = doubleValue;
	doubleValue = newValue;
	doubleField.setText(String.valueOf(doubleValue));
	changes.firePropertyChange("doubleValue", new Double(oldValue), new Double(newValue));
    }

    public double getDoubleValue() {
	return doubleValue;
    }
  
    public void setShortValue(short newValue) {
	short oldValue = shortValue;    
	shortValue = newValue;
	shortField.setText(String.valueOf(shortValue));
	changes.firePropertyChange("shortValue", new Short(oldValue), new Short(newValue));
    }

    public short getShortValue() {
	return shortValue;
    }

    public void setFloatValue(float newValue) {
	float oldValue = floatValue;
	floatValue = newValue;
	floatField.setText(String.valueOf(floatValue));
	changes.firePropertyChange("floatValue", new Float(oldValue), new Float(newValue));
    }

    public float getFloatValue() {
	return floatValue;
    }

    public void setCharValue(char newValue) {
	char oldValue = charValue;
	charValue = newValue;
	charField.setText(String.valueOf(charValue));
	changes.firePropertyChange("charValue", new Character(oldValue), new Character(newValue));
    }

    public char getCharValue() {
	return charValue;
    }

    public void setByteValue(byte  newValue) {
	byte oldValue = byteValue;
	byteValue = newValue;
	byteField.setText(String.valueOf(byteValue));
	changes.firePropertyChange("byteValue", new Byte(oldValue), new Byte(newValue));
    }

    public void setByteValue(int  newValue) {
	setByteValue((byte) newValue);
    }

    public byte getByteValue() {
	return byteValue;
    }

    public void setLongValue(long  newValue) {
	longValue = newValue;
    }

    public long getLongValue() {
	return longValue;
    }
    public boolean isBooleanValue() {
	return booleanValue;
    } 

    public void setBooleanValue(boolean newValue) {
	boolean oldValue = booleanValue;
	booleanValue = newValue;
	booleanField.setState(booleanValue);
	changes.firePropertyChange("booleanValue", new Boolean(oldValue), new Boolean(newValue));
    }

    public String getStringValue() {
	if (stringValue == null) {
	    return "";
	} else
	    return stringValue;
    }
  
    public void setStringValue(String newValue) {
	String oldValue = stringValue;
	stringValue = newValue;
	stringField.setText(stringValue);
	changes.firePropertyChange("stringValue", oldValue, newValue);
    }



    public int[] getIntArray() {
        return intArray;
    }

    public void setIntArray(int[] newIntArray) {
         intArray = newIntArray;
	 array1.setText(String.valueOf(intArray[0]));
	 array2.setText(String.valueOf(intArray[1]));
	 array3.setText(String.valueOf(intArray[2]));
	 array4.setText(String.valueOf(intArray[3]));
    }

    public void setIntArrayWithIndex(int index, int value) {
	 intArray[index] = value;
    }

    public int getIntArrayWithIndex(int index) {       
	return intArray[index];
    }

    public double[] getDoubleArray() {
        return doubleArray;
    }

    public void setDoubleArray(double[] newDoubleArray) {
         doubleArray = newDoubleArray;
	 array1.setText(String.valueOf(doubleArray[0]));
	 array2.setText(String.valueOf(doubleArray[1]));
	 array3.setText(String.valueOf(doubleArray[2]));
	 array4.setText(String.valueOf(doubleArray[3]));
    }

    public void setDoubleArrayWithIndex(int index, double value) {
	 doubleArray[index] = value;
    }

    public double getDoubleArrayWithIndex(int index) {       
	return doubleArray[index];
    }

    public String[] getStringArray() {
        return stringArray;
    }

    public void setStringArray(String[] newStringArray) {
         stringArray = newStringArray;
	 array1.setText(stringArray[0]);
	 array2.setText(stringArray[1]);
	 array3.setText(stringArray[2]);
	 array4.setText(stringArray[3]);
    }

    public void setStringArrayWithIndex(int index, String value) {
	 stringArray[index] = value;
    }

    public String getStringArrayWithIndex(int index) {       
	return stringArray[index];
    }

    public byte[] getByteArray() {
        return byteArray;
    }

    public void setByteArray(byte[] newByteArray) {
         byteArray = newByteArray;
	 array1.setText(String.valueOf(byteArray[0]));
	 array2.setText(String.valueOf(byteArray[1]));
	 array3.setText(String.valueOf(byteArray[2]));
	 array4.setText(String.valueOf(byteArray[3]));
    }

    public void setByteArrayWithIndex(int index, byte value) {
	 byteArray[index] = value;
    }

    public byte getByteArrayWithIndex(int index) {       
	return byteArray[index];
    }

    public float[] getFloatArray() {
        return floatArray;
    }

    public void setFloatArray(float[] newFloatArray) {
         floatArray = newFloatArray;
	 array1.setText(String.valueOf(floatArray[0]));
	 array2.setText(String.valueOf(floatArray[1]));
	 array3.setText(String.valueOf(floatArray[2]));
	 array4.setText(String.valueOf(floatArray[3]));
    }

    public void setFloatArrayWithIndex(int index, float value) {
	 floatArray[index] = value;
    }

    public float getFloatArrayWithIndex(int index) {       
	return floatArray[index];
    }

    public char[] getCharArray() {
        return charArray;
    }

    public void setCharArray(char[] newCharArray) {
         charArray = newCharArray;
	 array1.setText(String.valueOf(charArray[0]));
	 array2.setText(String.valueOf(charArray[1]));
	 array3.setText(String.valueOf(charArray[2]));
	 array4.setText(String.valueOf(charArray[3]));
    }

    public void setCharArrayWithIndex(int index, char value) {
	 charArray[index] = value;
    }

    public char getCharArrayWithIndex(int index) {       
	return charArray[index];
    }

    public boolean[] getBooleanArray() {
        return booleanArray;
    }

    public void setBooleanArray(boolean[] newBooleanArray) {
         booleanArray = newBooleanArray;
	 array1.setText(String.valueOf(booleanArray[0]));
	 array2.setText(String.valueOf(booleanArray[1]));
	 array3.setText(String.valueOf(booleanArray[2]));
	 array4.setText(String.valueOf(booleanArray[3]));
    }

    public void setBooleanArrayWithIndex(int index, boolean value) {
	 booleanArray[index] = value;
    }

    public boolean getBooleanArrayWithIndex(int index) {       
	return booleanArray[index];
    }

    public short[] getShortArray() {
        return shortArray;
    }

    public void setShortArray(short[] newShortArray) {
         shortArray = newShortArray;
	 array1.setText(String.valueOf(shortArray[0]));
	 array2.setText(String.valueOf(shortArray[1]));
	 array3.setText(String.valueOf(shortArray[2]));
	 array4.setText(String.valueOf(shortArray[3]));
    }

    public void setShortArrayWithIndex(int index, short value) {
	 shortArray[index] = value;
    }

    public short getShortArrayWithIndex(int index) {       
	return shortArray[index];
    }

  public void printObjectInField(Object o) {
    if (o==null)
      stringField.setText("NULL REF");
    else
      stringField.setText(o.toString());
  }

    /**
     * The specified PropertyChangeListeners <b>propertyChange</b> method will
     * be called each time the value of any bound property is changed.
     * The PropertyListener object is addded to a list of PropertyChangeListeners
     * managed by this button, it can be removed with removePropertyChangeListener.
     * Note: the JavaBeans specification does not require PropertyChangeListeners
     * to run in any particular order.
     *
     * @see #removePropertyChangeListener
     * @param l the PropertyChangeListener
     */      
    public void addPropertyChangeListener(PropertyChangeListener l) {
	changes.addPropertyChangeListener(l);
    }

    /** 
     * Remove this PropertyChangeListener from the buttons internal list.  
     * If the PropertyChangeListener isn't on the list, silently do nothing.
     * 
     * @see #addPropertyChangeListener
     * @param l the PropertyChangeListener
     */      
    public void removePropertyChangeListener(PropertyChangeListener l) {
	changes.removePropertyChangeListener(l);
    }

    public void addVetoableChangeListener(VetoableChangeListener l) {
	vetos.addVetoableChangeListener(l);
    }

    public void removeVetoableChangeListener(VetoableChangeListener l) {
	vetos.removeVetoableChangeListener(l);
    }

    // Event Listeners handling
    public synchronized void addBridgeTesterListener(BridgeTesterListener l) {
	listeners.addElement(l);
    }

    public synchronized void removeBridgeTesterListener(BridgeTesterListener l) {
	listeners.removeElement(l);
    }

    /*
     * Small adaptor class that implement the java.awt.event.ActionListener
     * @return ActionListener interface implementor
     */
    public java.awt.event.ActionListener getActionListener() {
      return new java.awt.event.ActionListener() {
	public void actionPerformed(java.awt.event.ActionEvent e) {
	  Dialog myDialog = new Dialog(new Frame(), "Event Received");
	  myDialog.setBounds(200,200,400,400);
	  myDialog.show();
	}
      };
    }
	  

    private void fireEvent(int eventNumber) {

	java.util.Vector listenerObjects;
	synchronized(this) {
	    listenerObjects = (java.util.Vector) listeners.clone();
	}
    
	for (int i=0; i<listenerObjects.size();i++) {
	    BridgeTesterListener l = (BridgeTesterListener) listenerObjects.elementAt(i);
	    switch(eventNumber) {
	        case 1 : 
		  BridgeTesterEvent evt = new BridgeTesterEvent(this, stringValue, intValue);		          l.eventNumber1(evt);
		  break;
  	        case 2 :
		  l.eventNumber2(stringValue);
		  break;
    	        case 3:
		  l.eventNumber3(shortValue);
		  break;
		case 4:
		  l.eventNumber4(intArray);
		  break;
	    }
	}
    }

    // ActionListener implementation		      
    public void actionPerformed(ActionEvent e) {

         Object source = e.getSource();
         if (source==event1Button) {
                fireEvent(1);
		return;
	 }
         if (source==event2Button) {
                fireEvent(2);
		return;
	 }
         if (source==event3Button) {
                fireEvent(3);
		return;
	 }
         if (source==event4Button) {
                fireEvent(4);
		return;
	 }

    }
    
    
    // KeyListener implementation
    public void keyPressed(KeyEvent e) {}
    public void keyTyped(KeyEvent e) {}
    public void keyReleased(KeyEvent e) {
	
         Object source = e.getSource();
	 if (source==stringField) {
	        setStringValue(stringField.getText());
		return;
	 }
	 if (source==doubleField) {
	    try {
	      double oldValue = doubleValue;
	      doubleValue = new Double(doubleField.getText()).doubleValue();
	      changes.firePropertyChange("doubleValue", new Double(oldValue), new Double(doubleValue));

	    } catch (java.lang.NumberFormatException ex) {
	      doubleField.setText(String.valueOf(getDoubleValue()));
	    }
	    return;
	 } 
	 if (source==intField) {
	    int oldValue = intValue;
	    try {	 
	      int newValue = new Integer(intField.getText()).intValue();
	      vetos.fireVetoableChange("intValue", new Integer(oldValue), new Integer(newValue));
	      intValue = newValue;
	      changes.firePropertyChange("intValue", new Integer(oldValue), new Integer(newValue));	 
	    } catch (java.lang.NumberFormatException ex) {
	         intField.setText(String.valueOf(getIntValue()));
	    } catch (PropertyVetoException ex) {
	         intField.setText(String.valueOf(getIntValue()));
	    }  
	    return;
	 } 
	 if (source==shortField) {
	    try {
	      short oldValue = shortValue;
	      shortValue = new Short(shortField.getText()).shortValue();
	      changes.firePropertyChange("shortValue", new Short(oldValue), new Short(shortValue));
	    } catch (java.lang.NumberFormatException ex) {
	         shortField.setText(String.valueOf(getShortValue()));
	    }
	    return;
	 } 
	 if (source==floatField) {
	    try {
	      float oldValue = floatValue;
	      floatValue = new Float(floatField.getText()).floatValue();
	      changes.firePropertyChange("floatValue", new Float(oldValue), new Float(floatValue));
	    } catch (java.lang.NumberFormatException ex) {
	      floatField.setText(String.valueOf(getFloatValue()));
	    }
	    return;
	 } 
	 if (source==byteField) {
	    try {
	      byte oldValue = byteValue;
	      byteValue = new Byte(byteField.getText()).byteValue();
	      changes.firePropertyChange("byteValue", new Byte(oldValue), new Byte(byteValue));
	    } catch (java.lang.NumberFormatException ex) {
	         byteField.setText(String.valueOf(getByteValue()));
	    }
	    return;
	 } 
	 if (source==charField) {
	    try {
	      setCharValue(charField.getText().charAt(0));
	    } catch (java.lang.NumberFormatException ex) {
	      char[] value = {getCharValue()};
	         charField.setText(new String(value));
	    }
	    return;
	 } 
    }

    // Mouse Listener implementation
    public void mousePressed(MouseEvent e) {}
    public void mouseClicked(MouseEvent e) {}
    public void mouseEntered(MouseEvent e) {}
    public void mouseExited(MouseEvent e) {}
    public void mouseReleased(MouseEvent e) {
      if (e.getSource()==booleanField) {
	boolean oldValue = booleanValue;
	boolean newValue;
	if (booleanValue) 
	  newValue=false;
	else 
	  newValue = true;
	booleanValue = newValue;
	changes.firePropertyChange("booleanValue", new Boolean(oldValue), new Boolean(newValue));
      }

    }
    
    // All the properties we publish
    private int intValue;
    private short shortValue;
    private double doubleValue;
    private float floatValue;
    private char charValue;
    private byte byteValue;
    private boolean booleanValue;
    private String stringValue;
    private long longValue;

    // Indexed Properties
    private int[] intArray;
    private double[] doubleArray;
    private String[] stringArray;
    private float[] floatArray;
    private char[] charArray;
    private short[] shortArray;
    private byte[] byteArray;
    private boolean[] booleanArray;

    // All the widgets to display the properties
    private TextField intField;
    private TextField doubleField;
    private TextField shortField;
    private TextField floatField;
    private TextField charField;
    private TextField byteField;
    private Checkbox  booleanField;
    private TextField stringField;
    private Button event1Button, event2Button, event3Button, event4Button;
    private TextField array1, array2, array3, array4;

    // Some utility objects
    private PropertyChangeSupport changes = new PropertyChangeSupport(this);
    private VetoableChangeSupport vetos = new VetoableChangeSupport(this);
    private java.util.Vector listeners = new java.util.Vector();
    private java.util.Vector actionListeners = new java.util.Vector();
}
