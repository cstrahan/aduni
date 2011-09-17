
// BeanInfo for an BridgeTester.

package sunw.demo.test;

import java.beans.*;


public class BridgeTesterBeanInfo extends SimpleBeanInfo 
{
    Class btClass = BridgeTester.class; // around a bug in JDK1.1 javac

    public BeanDescriptor getBeanDescriptor() {
	return new BeanDescriptor(btClass,
					BridgeTesterCustomizer.class);
    }

    public PropertyDescriptor[] getPropertyDescriptors() {       
        try {
            PropertyDescriptor background =
			new PropertyDescriptor("background", btClass);
	    PropertyDescriptor foreground =
			new PropertyDescriptor("foreground", btClass);
	    PropertyDescriptor font =
			new PropertyDescriptor("font", btClass);

	    PropertyDescriptor intValue = new PropertyDescriptor("intValue",btClass);
	    intValue.setConstrained(false);
	    intValue.setBound(true);

	    PropertyDescriptor shortValue = new PropertyDescriptor("shortValue", btClass);
	    shortValue.setConstrained(false);
	    shortValue.setBound(false);

	    PropertyDescriptor longValue = new PropertyDescriptor("longValue", btClass);
	    longValue.setConstrained(false);
	    longValue.setBound(false);

	    PropertyDescriptor byteValue = new PropertyDescriptor("byteValue", btClass);
	    byteValue.setConstrained(false);
	    byteValue.setBound(false);

	    PropertyDescriptor charValue = new PropertyDescriptor("charValue", btClass);
	    charValue.setConstrained(false);
	    charValue.setBound(false);

	    PropertyDescriptor floatValue = new PropertyDescriptor("floatValue", btClass);
	    floatValue.setConstrained(false);
	    floatValue.setBound(false);

	    PropertyDescriptor doubleValue = new PropertyDescriptor("doubleValue", btClass);	
	    floatValue.setConstrained(false);
	    floatValue.setBound(false);

	    PropertyDescriptor booleanValue = new PropertyDescriptor("booleanValue",btClass);
	    booleanValue.setConstrained(false);
	    booleanValue.setBound(false);

	    PropertyDescriptor stringValue = new PropertyDescriptor("stringValue", btClass);
	    stringValue.setConstrained(false);
	    stringValue.setBound(false);

	    IndexedPropertyDescriptor intArray = new IndexedPropertyDescriptor("intArray", 
		   btClass, "getIntArray", "setIntArray", "getIntArrayWithIndex", 
		   "setIntArrayWithIndex");
	    intArray.setBound(false);
	    intArray.setConstrained(false);

	    IndexedPropertyDescriptor doubleArray = new IndexedPropertyDescriptor("doubleArray", 
		   btClass, "getDoubleArray", "setDoubleArray", "getDoubleArrayWithIndex", 
		   "setDoubleArrayWithIndex");
	    doubleArray.setConstrained(false);
	    doubleArray.setBound(false);

	    IndexedPropertyDescriptor stringArray = new IndexedPropertyDescriptor("stringArray", 
		   btClass, "getStringArray", "setStringArray", "getStringArrayWithIndex", 
		   "setStringArrayWithIndex");
	    stringArray.setConstrained(false);
	    stringArray.setBound(false);

	    IndexedPropertyDescriptor floatArray = new IndexedPropertyDescriptor("floatArray", 
		   btClass, "getFloatArray", "setFloatArray", "getFloatArrayWithIndex", 
		   "setFloatArrayWithIndex");
	    floatArray.setConstrained(false);
	    floatArray.setBound(false);

	    IndexedPropertyDescriptor charArray = new IndexedPropertyDescriptor("charArray", 
		   btClass, "getCharArray", "setCharArray", "getCharArrayWithIndex", 
		   "setCharArrayWithIndex");
	    charArray.setConstrained(false);
	    charArray.setBound(false);

	    IndexedPropertyDescriptor byteArray = new IndexedPropertyDescriptor("byteArray", 
		   btClass, "getByteArray", "setByteArray", "getByteArrayWithIndex", 
		   "setByteArrayWithIndex");
	    byteArray.setConstrained(false);
	    byteArray.setBound(false);

	    IndexedPropertyDescriptor booleanArray = new IndexedPropertyDescriptor("booleanArray", 
		   btClass, "getBooleanArray", "setBooleanArray", "getBooleanArrayWithIndex", 
		   "setBooleanArrayWithIndex");
	    booleanArray.setConstrained(false);
	    booleanArray.setBound(false);

	    IndexedPropertyDescriptor shortArray = new IndexedPropertyDescriptor("shortArray", 
		   btClass, "getShortArray", "setShortArray", "getShortArrayWithIndex", 
		   "setShortArrayWithIndex");
	    shortArray.setConstrained(false);
	    shortArray.setBound(false);
	     
	    PropertyDescriptor rv[]={intValue, shortValue, longValue, floatValue, doubleValue, charValue,
				     booleanValue, byteValue,stringValue,
				     intArray, doubleArray, stringArray, charArray, byteArray,
				     booleanArray, floatArray, shortArray,
				     background, foreground, font};
	    return rv;

	} catch (IntrospectionException e) {
	     throw new Error(e.toString());
	}
    }

    public int getDefaultPropertyIndex() {
        return 8;
    }

    public EventSetDescriptor[] getEventSetDescriptors() {
      String[] listenerMethods = { "eventNumber1", "eventNumber2", "eventNumber3", "eventNumber4" };
      EventSetDescriptor event, changes, vetos;
      try {
         event = new EventSetDescriptor(btClass, 
		     "Event", BridgeTesterListener.class, listenerMethods, 
		     "addBridgeTesterListener", "removeBridgeTesterListener");
	 changes = new EventSetDescriptor(btClass, 
			"propertyChange",
			java.beans.PropertyChangeListener.class,
			"propertyChange");
         vetos = new EventSetDescriptor(btClass, 
			"vetoableChange",
			java.beans.VetoableChangeListener.class,
		        "vetoableChange");
      } catch (IntrospectionException e) {
	   throw new Error(e.toString());
      }
      EventSetDescriptor[] events = {event, changes, vetos};
      return events;
    }

    public int getDefaultEventIndex() {
      return 2;
    }

    public MethodDescriptor[] getMethodDescriptors() {
       return null;
    }
     
    public java.awt.Image getIcon(int iconKind) {
	if (iconKind == BeanInfo.ICON_COLOR_16x16) {
	    java.awt.Image img = loadImage("BridgeTesterIconColor16.gif");
	    return img;
	}
	if (iconKind == BeanInfo.ICON_COLOR_32x32) {
	    java.awt.Image img = loadImage("BridgeTesterIconColor32.gif");
	    return img;
	}
	if (iconKind == BeanInfo.ICON_MONO_16x16) {
	    java.awt.Image img = loadImage("BridgeTesterIconMono16.gif");
	    return img;
	}
	if (iconKind == BeanInfo.ICON_MONO_32x32) {
	    java.awt.Image img = loadImage("BridgeTesterIconMono32.gif");
	    return img;
	}
	return null;
    }
}

