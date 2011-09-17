/*
 *
 * @(#) DoubleConstant.java 1.4@(#)
 *
 * Copyright (c) 1997 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * SUN MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. SUN SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 * 
 */

/**
 * <p>
 * sunw.demo.classfile.DoubleConstant
 * </p> 
 *
 * @version 1.0
 * @author Laurence P. G. Cable
 */


package sunw.demo.classfile;

import java.io.DataOutputStream;
import java.io.IOException;

import sunw.demo.classfile.ClassFile;
import sunw.demo.classfile.ConstantPoolEntry;

/**
 * <p> implements a DOUBLE_CONSTANT CPE </p>
 */

class DoubleConstant extends ConstantPoolEntry {

    private double	doubler;

    /**
     * <p> construct a DOUBLE_CONSTANT CPE </p>
     *
     * @param d 	the double constant
     * @param cf	the class file
     */

    DoubleConstant(double d, ClassFile cf) {
    	super(CONSTANT_DOUBLE, cf);
    
    	doubler = d;

    	addToConstantPool();
    }

    /**
     * <p> write the constant CPE to the stream </p>
     *
     * @param dos the stream
     *
     * @throws IOException
     */

    void write(DataOutputStream dos) throws IOException {
    	dos.writeByte(getTag());
    	dos.writeDouble(doubler);
    }

    /**
     * @return the double constant value.
     */

    double getValue() { return doubler; }

    /**
     * @return the object's equality.
     */

    public boolean equals(Object o) {
    	if (o instanceof Double) {
    	    return doubler == ((Double)o).doubleValue();
    	} else if (o instanceof DoubleConstant) {
    	    DoubleConstant dc = (DoubleConstant)o;

    	    return doubler == dc.getValue();
    	}

    	return false;
    }

    /**
     * @return a hashcode for the object.
     */
    public int hashCode() {
	return (int)doubler;
    }
}
