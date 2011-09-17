

/**
 * A subclass of ExplicitButton that uses Externalization to
 * gain bit0by-bit control of its Serialized state.
 * 
 * @see sunw.demo.buttons.ExplicitButton
 * @see jav.io.Externalizable
 */

package sunw.demo.buttons;

import java.awt.*;
import java.beans.*;
import java.io.*;

public class ExternalizableButton extends ExplicitButton
			implements Externalizable {

    // We take complete control of our own persistence state with externalization.

    public void writeExternal(ObjectOutput out) throws IOException {
	out.writeInt(currentMagic);

	Rectangle bounds = getBounds();
	out.writeInt(bounds.x);
	out.writeInt(bounds.y);
        out.writeInt(bounds.width);
	out.writeInt(bounds.height);

	out.writeUTF(getLabel());

	Color bg = getBackground();
	if (bg == null) {
	    out.writeInt(0);
	} else {
	    out.writeInt(bg.getRGB());
	}

	Color fg = getForeground();
	if (fg == null) {
	    out.writeInt(0);
	} else {
	    out.writeInt(fg.getRGB());
	}

	Font f = getFont();
	if (f == null) {
	    out.writeInt(-1);
	} else {
	    out.writeInt(f.getStyle());
	    out.writeInt(f.getSize());
	    out.writeUTF(f.getName());
	}
    }

    public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {
	int magic = in.readInt();
	if (magic != currentMagic) {
	    throw new IOException("magic number mismatch for ExternalizableButton");
	}

	int x = in.readInt();
	int y = in.readInt();
	int width = in.readInt();
	int height = in.readInt();
	setBounds(x, y, width, height);

	setLabel(in.readUTF());

	int bg = in.readInt();
	if (bg != 0) {
	    setBackground(new Color(bg));
	} 

	int fg = in.readInt();
	if (fg != 0) {
	    setForeground(new Color(fg));
	} 
	
	int style = in.readInt();
	if (style >= 0) {
	    int size = in.readInt();
	    String name = in.readUTF();
	    Font f = new Font(name, style, size);
	    setFont(f);
	}

    }

    private final static int currentMagic = 0xAAAAAAAA;
}
