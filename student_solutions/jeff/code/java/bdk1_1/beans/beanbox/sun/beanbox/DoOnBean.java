package sun.beanbox;

/**
 * An action to perform on created beans
 */

import java.beans.*;

public interface DoOnBean {
    void action(JarInfo ji, BeanInfo bi, String beanName);
    void error(String msg);	// display an error message
    void error(String msg, Exception ex); // ditto, with an exception
}
