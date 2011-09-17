/*
 *
 * @(#) LongConstant.java 1.4@(#)
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
 * sunw.demo.classfile.LongConstant
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
 * <p> this class provides minimal support for CONSTANT_LONG CPE's </p>
 */

class LongConstant extends ConstantPoolEntry {

    private long	longish;

    /**
     * <p> construct a CONSTANT_LONG </p>
     *
     * @param l		the long constant
     * @param cf	the class file
     */

    LongConstant(long l, ClassFile cf) {
    	super(CONSTANT_LONG, cf);
    
    	longish = l;

    	addToConstantPool();
    }

    /**
     * <p> write the CONSTANT_LONG to the stream </p>
     *
     * @param dos 	the output stream
     *
     * @throws IOException
     */

    void write(DataOutputStream dos) throws IOException {
    	dos.writeByte(getTag());
    	dos.writeLong(longish);
    }

    /**
     * @return the long constant value
     */

    long getValue() { return longish; }

    /**
     * @return object equality
     */

    public boolean equals(Object o) {
    	if (o instanceof Long) {
    	    return longish == ((Long)o).longValue();
    	} else if (o instanceof LongConstant) {
    	    LongConstant lc = (LongConstant)o;

    	    return longish == lc.getValue();
    	}

    	return false;
    }

    /**
     * @return a hashcode for the object.
     */
    public int hashCode() {
	return (int)longish;	
    }
}
