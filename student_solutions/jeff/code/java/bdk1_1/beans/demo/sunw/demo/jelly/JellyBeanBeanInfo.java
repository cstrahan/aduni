
/** 
 * The only thing we define in the Juggler BeanInfo is a GIF icon.
 */

package sunw.demo.jelly;

import java.beans.*;

public class JellyBeanBeanInfo extends SimpleBeanInfo {

    public java.awt.Image getIcon(int iconKind) {
	if (iconKind == BeanInfo.ICON_COLOR_16x16) {
	    java.awt.Image img = loadImage("JellyBeanIconColor16.gif");
	    return img;
	}
	if (iconKind == BeanInfo.ICON_COLOR_32x32) {
	    java.awt.Image img = loadImage("JellyBeanIconColor32.gif");
	    return img;
	}
	if (iconKind == BeanInfo.ICON_MONO_16x16) {
	    java.awt.Image img = loadImage("JellyBeanIconMono16.gif");
	    return img;
	}
	if (iconKind == BeanInfo.ICON_MONO_32x32) {
	    java.awt.Image img = loadImage("JellyBeanIconMono32.gif");
	    return img;
	}
	return null;
    }

}
