/*
 *
 * @(#) EncapsulatedEventAdaptorClassFile.java 1.3@(#)
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
 * sunw.demo.classfile.EncapsulatedEventAdaptorClassFile
 * </p> 
 *
 * @version 1.0
 * @author Graham Hamilton
 */

package sunw.demo.classfile;

import java.io.*;
import java.lang.reflect.*;
import sunw.demo.classfile.*;
import java.beans.*;

public final class DelegatorClassFile {

    private Class targets[];  		// classes we're delegating to.
    private FieldConstant fields[];	// one for each target class.
    private static String superclassName;
    private String className;
    private ClassFile classFile;
    private java.util.Hashtable generatedMethods = new java.util.Hashtable();
    private char buff[] = new char[200];
    private int ix;

    public DelegatorClassFile(String className, Class targs[])
				 throws IOException, ClassNotFoundException {

	this.className = className;
	targets = targs;
	superclassName = targets[0].getName();

	classFile = new ClassFile(className, targets[0].getName());

	allocateFields();

	recordInheritedMethods();

	generateDelegatedMethods();

	generateConstructor();
    }

    private void allocateFields() {
	// Create fields to hold delegation objects for each target class.

	fields = new FieldConstant[targets.length];

	for (int i = 1; i < targets.length; i++) {
	    String fieldName = "t" + i;
	    String fieldTypeString = getInternalTypeString(targets[i]);
			 
	    classFile.addFieldDesc(
	         new FieldDesc(
	    		fieldName,
 			fieldTypeString,
			(short)FieldDesc.ACC_PRIVATE,
			classFile,
			null
	           )
	    );

	    FieldConstant field = classFile.addFieldConstant(className,
			  fieldName, fieldTypeString);

	    fields[i] = field;
	}
    }

    private void recordInheritedMethods() {
	// We simply inherit the emthods for target[0].
	Method ms[] = targets[0].getMethods();
	for (int i = 0; i < ms.length; i++) {
	    Method m = ms[i];
	    String typeString = getMethodTypeString(m);
	    String id = m.getName() + typeString;
	    generatedMethods.put(id, id);
	}
    }


    private void generateDelegatedMethods() {

	for (int i = 0; i < targets.length; i++) {
	    Class target = targets[i];
	    BeanInfo bi;
	    try {
	        bi = Introspector.getBeanInfo(target);
	    } catch (IntrospectionException ix) {
	        System.err.println("Introspection failed on " + target);
	        return;
	    }

	    // Generate a delegation method for each method in the class.
	    MethodDescriptor mds[] = bi.getMethodDescriptors();
	    for (int j = 0; j < mds.length; j++) {
	        MethodDescriptor md = mds[j];
	        Method m = md.getMethod();
	        generateDelegateMethod(fields[i], m);
	    }
	}
    }

    private void generateConstructor() {
	Code	code   = new Code(classFile, (short)(targets.length+2), (short)(targets.length+4));

	// Generate the VM type string.  This only happens once.
	String typeString = "(";
	for (int i = 1; i < targets.length; i++) {
	    typeString += getInternalTypeString(targets[i]);
	}
	typeString += ")V";

	// Invoke the superclass constructor
	code.addOp     (Code.OP_ALOAD_0);
	MethodConstant mc = classFile.addMethodConstant(
		ClassFile.fullyQualifiedForm(superclassName),
		"<init>",
		"()V"
	     );
	code.addOpShort(Code.OP_INVOKE_SPECIAL, mc.getConstantPoolIndex());

	// Assign each target object to its corresponding field.
	for (int i = 1; i < fields.length; i++) {
	    code.addOp     (Code.OP_ALOAD_0);
	    pushArg(code, targets[i], i);
	    code.addOpShort(Code.OP_PUTFIELD, fields[i].getConstantPoolIndex());
	}

	code.addOp     (Code.OP_RETURN);

	// now add a method to the class file describing the constructor ...

	classFile.addMethodDesc(
	    new MethodDesc(
		"<init>",
		typeString,
		(short)MethodDesc.ACC_PUBLIC,
		classFile,
		code
	    )
	);
    }

    /**
     * Generate a delegated call.
     * We push all our incoming arguments onto the stack and do
     * a through call.
     */

    private void generateDelegateMethod(FieldConstant targetField, Method m) {
	// System.err.println("Method " + m.getName());

	Class params[] = m.getParameterTypes();

	// Generate the VM type string for the method.
	String typeString = getMethodTypeString(m);

	String id = m.getName() + typeString;
	if (generatedMethods.get(id) != null) {
	    // System.err.println("Skipping duplicate " +id);
	    return;
	}
	generatedMethods.put(id, id);

	Code code = new Code(classFile, (short) (params.length + 2), (short)(2+ (2 * params.length)));

	// Push the target for the method call.
	code.addOp(code.OP_ALOAD_0);
	code.addOpShort(code.OP_GETFIELD, targetField.getConstantPoolIndex());

	// Push each of the arguments for the method.
	for (int i = 0; i < params.length; i++) {
	    pushArg(code, params[i], i+1);
	}

	// generate the virtual method call.
	MethodConstant mc = classFile.addMethodConstant(
		m.getDeclaringClass().getName().replace('.', '/'),
		m.getName(),
		typeString
	     );
	code.addOpShort(Code.OP_INVOKE_VIRTUAL, mc.getConstantPoolIndex());

	// generate the return instruction to return any result.
	generateReturn(code, m.getReturnType());


	// Create the VM method descriptor for the generated method.
	MethodDesc md = new MethodDesc(m.getName(),
			    typeString,
			    (short)MethodDesc.ACC_PUBLIC,
			    classFile, code);
	classFile.addMethodDesc(md);

    }

    void pushArg(Code code, Class type, int index) {
	// Figure out both the near and the far forms of
	// argument loading opcodes for the given type.
	byte far;
	byte near;
	if (type.isPrimitive()) {
	    // It's a primitive type
	    if (type == long.class) {
	        far  = Code.OP_LLOAD;
	        near = Code.OP_LLOAD_0;
	    } else if (type == float.class) {
	        far  = Code.OP_FLOAD;
	        near = Code.OP_FLOAD_0;
	    } else if (type == double.class) {
	        far  = Code.OP_DLOAD;
	        near = Code.OP_DLOAD_0;
	    } else {
		// byte, short, char, or int
	        far  = Code.OP_ILOAD;
	        near = Code.OP_ILOAD_0;
	    }
	} else {
	    // It's an object type
	    far  = Code.OP_ALOAD;
	    near = Code.OP_ALOAD_0;
	}
	// Now issue either a near or far load as appropriate.
	if (index < 4) {
	    code.addOp((byte)(near + index));
	} else {
	    code.addOp1(far, (byte)index);
	}
    }

    void generateReturn(Code code, Class returnType) {
	byte opcode;	
	if (returnType.isPrimitive()) {
	    if (returnType == void.class) {
		opcode = Code.OP_RETURN;
	    } else if (returnType == long.class) {
		opcode = Code.OP_LRETURN;
	    } else if (returnType == float.class) {
		opcode = Code.OP_FRETURN;
	    } else if (returnType == double.class) {
		opcode = Code.OP_DRETURN;
	    } else {
		// byte, short, char, or int
		opcode = Code.OP_IRETURN;
	    }
	} else {
	    opcode = Code.OP_ARETURN;
	}
	code.addOp(opcode);
    }

    public void write(OutputStream os) throws IOException {
	classFile.write(os);
    }


    /**
     * Convert a Class type to an internal type string.
     *
     */
    private String getInternalTypeString(Class type) {
	ix = 0;
	push(type);
	return new String(buff, 0, ix);
    }

    /**
     * Convert a Method type to an internal type string.
     *
     * We come through here a lot so we try to optimize it.
     */
    private String getMethodTypeString(Method m) {
	ix = 0;
	Class type;
	try {
	    buff[ix++] = '(';

	    Class params[] = m.getParameterTypes();
	    for (int i = 0; i < params.length; i++) {
		push(params[i]);
	    }
	    buff[ix++] = ')';

	    // Now handle the return type
	    push(m.getReturnType());

	    return new String(buff, 0, ix);

	} catch (ArrayIndexOutOfBoundsException ex) {
	    // Check if we've exceeded the "buff" and try again.
	    if (ix >= buff.length) {
		buff = new char[2*buff.length];
		return getMethodTypeString(m);
	    }
	    throw ex;
	}

    }

    private void push(Class type) {
        while (type.isArray()) {
            type = type.getComponentType();
            buff[ix++] = '[';
        }
        if (type.isPrimitive()) {
            if (type == void.class)   {
                buff[ix++] = 'V';
            } else if (type == int.class) {
                buff[ix++] = 'I';
            } else if (type == boolean.class) {
                buff[ix++] = 'Z';
            } else if (type == long.class) {
                buff[ix++] = 'J';
            } else if (type == float.class) {
                buff[ix++] = 'F';
            } else if (type == double.class) {
                buff[ix++] = 'D';
            } else if (type == char.class) {
                buff[ix++] = 'C';
            } else if (type == byte.class) {
                buff[ix++] = 'B';
            } else if (type == short.class) {
                buff[ix++] = 'S';
            } else {
                throw new Error("Unexpected primitive type " + type.getName());
            }
        } else {
            buff[ix++] = 'L';
            String s = type.getName();
            int len = s.length();
            for (int i = 0; i < len; i++) {
                char ch = s.charAt(i);
                if (ch == '.') {
		    ch = '/';
                }
                buff[ix++] = ch;
            }
            buff[ix++] = ';';
        }
    }

}
