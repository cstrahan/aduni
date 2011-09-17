package sunw.demo.buttons;

import java.beans.*;

/**
 * BeanInfo for an ExplicitButton.
 * 
 * @see sunw.demo.buttons.ExplicitButton
 */
public class ExplicitButtonBeanInfo extends SimpleBeanInfo {

    public PropertyDescriptor[] getPropertyDescriptors() {
        try {
            PropertyDescriptor background =
			new PropertyDescriptor("background", beanClass);
	    PropertyDescriptor foreground =
			new PropertyDescriptor("foreground", beanClass);
	    PropertyDescriptor font =
			new PropertyDescriptor("font", beanClass);
            PropertyDescriptor label =
			new PropertyDescriptor("label", beanClass);

            background.setBound(true);
            foreground.setBound(true);
            font.setBound(true);
            label.setBound(true);

            PropertyDescriptor rv[] = {background, foreground, font, label};
            return rv;
        } catch (IntrospectionException e) {
            throw new Error(e.toString());
        }
    }


    public int getDefaultPropertyIndex() {
	// the index for the "label" property
        return 3; 
    }


    public EventSetDescriptor[] getEventSetDescriptors() {
        try {
            EventSetDescriptor push = new EventSetDescriptor(beanClass, 
	                "actionPerformed",
			java.awt.event.ActionListener.class,
			"actionPerformed");

            EventSetDescriptor changed = new EventSetDescriptor(beanClass,
			"propertyChange",
			java.beans.PropertyChangeListener.class,
			"propertyChange");

            push.setDisplayName("button push");
            changed.setDisplayName("bound property change");
	
            EventSetDescriptor[] rv = { push, changed};
            return rv;
        } catch (IntrospectionException e) {
            throw new Error(e.toString());
        }
    }

    public BeanDescriptor getBeanDescriptor() {
	BeanDescriptor back = new BeanDescriptor(beanClass, customizerClass);
        back.setValue("hidden-state", Boolean.TRUE);
	return back;
    }

    public java.awt.Image getIcon(int iconKind) {
	if (iconKind == BeanInfo.ICON_MONO_16x16 ||
	    iconKind == BeanInfo.ICON_COLOR_16x16 ) {
	    java.awt.Image img = loadImage("ExplicitButtonIcon16.gif");
	    return img;
	}
	if (iconKind == BeanInfo.ICON_MONO_32x32 ||
	    iconKind == BeanInfo.ICON_COLOR_32x32 ) {
	    java.awt.Image img = loadImage("ExplicitButtonIcon32.gif");
	    return img;
	}
	return null;
    }

    private final static Class beanClass = ExplicitButton.class;
    private final static Class customizerClass = ExplicitButtonCustomizer.class;
}

