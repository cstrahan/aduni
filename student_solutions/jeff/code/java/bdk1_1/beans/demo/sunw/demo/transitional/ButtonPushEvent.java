package sunw.demo.transitional;

/**
 * JDK1.1
 * This class describes the event that gets generated when
 * OurButton gets pushed.
 */

public class ButtonPushEvent extends sunw.util.EventObject {
    public ButtonPushEvent(java.awt.Component source) {
        super(source);
    }
}

