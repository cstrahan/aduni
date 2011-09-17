
// BeanInfo for an DBViewer.

package sunw.demo.select;

import java.beans.*;

public class SelectBeanInfo extends SimpleBeanInfo {

    public BeanDescriptor getBeanDescriptor() {
	BeanDescriptor bd = new BeanDescriptor(Select.class,
					       SelectCustomizer.class);
	bd.setDisplayName("JDBC SELECT");
	return bd;
    }

}
