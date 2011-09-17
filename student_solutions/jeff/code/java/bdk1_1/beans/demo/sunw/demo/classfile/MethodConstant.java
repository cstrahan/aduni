/*
 *
 * @(#) MethodConstant.java 1.4@(#)
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
 * sunw.demo.classfile.MethodConstant
 * </p> 
 *
 * @version 1.0
 * @author Laurence P. G. Cable
 */


package sunw.demo.classfile;

import sunw.demo.classfile.ClassFile;
import sunw.demo.classfile.RefConstant;

/**
 * <p> this class provides minimal support for the CONSTANT_METHODREF CPE </p>
 */

class MethodConstant extends RefConstant {

    /**
     * <p> construct a CONSTANT_METHODREF </p>
     *
     * @param cName the name of the implementing class
     * @param nName the name of the method
     * @param tName the type descriptor of the method
     * @param cf    the class file
     */

    MethodConstant(String cName, String nName, String tName, ClassFile cf) {
	super(CONSTANT_METHODREF, cName, nName, tName, cf);
    }
}
