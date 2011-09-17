
/**
 * Take a generic component Editor and wrap it in a Dialog box.
 * this includes adding the Frame and the "ok" and "cancel" buttons.
 */

package sun.beanbox;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;


public class CustomizerDialog extends Dialog implements ActionListener {

    private Component body;
    private Button doneButton;
    private static int vPad = 5;
    private static int hPad = 4;

    public CustomizerDialog(Frame frame, Customizer customizer, Object target) {
	super(frame, customizer.getClass().getName(), true);
	new WindowCloser(this);
        setLayout(null);

        body = (Component)customizer;
	add(body);

	doneButton = new Button("Done");
	doneButton.addActionListener(this);
	add(doneButton);

        int x = frame.getLocation().x + 30;
        int y = frame.getLocation().y + 100;
	setLocation(x,y);

	show();
    }

    public void doLayout() {
        Insets ins = getInsets();
	Dimension bodySize = body.getPreferredSize();
	Dimension buttonSize = doneButton.getPreferredSize();

	int width = ins.left + 2*hPad + ins.right + bodySize.width;
	int height = ins.top + 3*vPad + ins.bottom + bodySize.height +
							buttonSize.height;

        body.setBounds(ins.left+hPad, ins.top+vPad,
				bodySize.width, bodySize.height);

	doneButton.setBounds((width-buttonSize.width)/2,
				ins.top+(2*hPad) + bodySize.height,
				buttonSize.width, buttonSize.height);

	setSize(width, height);
    }

    public void actionPerformed(ActionEvent evt) {
	// Our "done" button got pushed.
	dispose();
    }

}
