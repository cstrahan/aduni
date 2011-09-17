/*
 *
 * @(#) UTF8Constant.java 1.4@(#)
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
 * sunw.demo.classfile.UTF8Constant
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
 * <p> this class provides minimal support for CONSTANT_UTF8 CPE's </p>
 */

class UTF8Constant extends ConstantPoolEntry {

    private String string;

    /**
     * <p> construct a CONSTANT_UTF8 CPE </p>
     *
     * @param s 	the string
     * @param cf	the class file
     */

    UTF8Constant(String s, ClassFile cf) {
    	super(CONSTANT_UTF8, cf);
    
    	string = s;

    	addToConstantPool();
    }

    /**
     * <p> construct a CONSTANT_UTF8 CPE </p>
     * This variant is used when the ClassFile is doing the "new"
     * and already knows the intended index.
     *
     * @param s 	the string
     * @param cf	the class file
     * @param index	the constant pool index.
     */
    UTF8Constant(String s, ClassFile cf, short index) {
    	super(CONSTANT_UTF8, cf, index);
    	string = s;
    }

    /**
     * <p> write the CPE to the output stream </p>
     *
     * @param dos the output stream
     *
     * @throws IOException
     */

    void write(DataOutputStream dos) throws IOException {
    	dos.writeByte(getTag());
    	dos.writeUTF(string);
    }

    /**
     * @return the string constant
     */

    String getString() { return string; }

    /**
     * @return object equality
     */

    public boolean equals(Object o) {
    	if (o instanceof UTF8Constant) {
    	    return string.equals(((UTF8Constant)o).getString());
    	}

    	return false;
    }

    /**
     * @return a hashcode for the object.
     */
    public int hashCode() {
	return string.hashCode();	
    }
}
