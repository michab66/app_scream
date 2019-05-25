/*
 * Copyright 1999-2004 The Apache Software Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.javaflow.bytecode.transformation.asm;

import java.util.List;

import org.apache.commons.javaflow.bytecode.StackRecorder;

import org.objectweb.asm.Label;
import org.objectweb.asm.MethodAdapter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.MethodInsnNode;
import org.objectweb.asm.tree.analysis.Analyzer;
import org.objectweb.asm.tree.analysis.BasicValue;
import org.objectweb.asm.tree.analysis.Frame;


/**
 * ContinuationMethodAdapter
 * 
 * @author Evgueni Koulechov
 */
public final class ContinuationMethodAdapter extends MethodAdapter implements Opcodes {
    private static final String STACK_RECORDER = Type.getInternalName(StackRecorder.class);
    private static final String POP_METHOD = "pop";
    private static final String PUSH_METHOD = "push";

    private final Analyzer analyzer;
    private Label startLabel = new Label();
    private final List labels;
    private final List nodes;
    private final int stackRecorderVar;
    private final boolean isStatic;
    private final String methodDesc;

    private int currentIndex = 0;
    private Frame currentFrame = null;


    public ContinuationMethodAdapter(ContinuationMethodAnalyzer a) {
        super(a.mv);
        this.analyzer = a.analyzer;
        this.labels = a.labels;
        this.nodes = a.nodes;
        this.stackRecorderVar = a.stackRecorderVar;
        this.isStatic = (a.access & ACC_STATIC) > 0;
        this.methodDesc = a.desc;
    }

    public void visitCode() {
        mv.visitCode();

        int fsize = labels.size();
        Label[] restoreLabels = new Label[fsize];
        for (int i = 0; i < restoreLabels.length; i++) {
            restoreLabels[i] = new Label();
        }

        // verify if restoring
        Label l0 = new Label();

        mv.visitMethodInsn(INVOKESTATIC, STACK_RECORDER, "get", "()L" + STACK_RECORDER + ";");
        mv.visitInsn(DUP);
        mv.visitVarInsn(ASTORE, stackRecorderVar);
        mv.visitLabel(startLabel);

        mv.visitJumpInsn(IFNULL, l0);
        mv.visitVarInsn(ALOAD, stackRecorderVar);
        mv.visitFieldInsn(GETFIELD, STACK_RECORDER, "isRestoring", "Z");
        mv.visitJumpInsn(IFEQ, l0);

        mv.visitVarInsn(ALOAD, stackRecorderVar);
        mv.visitMethodInsn(INVOKEVIRTUAL, STACK_RECORDER, POP_METHOD + "Int", "()I");
        mv.visitTableSwitchInsn(0, fsize - 1, l0, restoreLabels);

        // switch cases
        for (int i = 0; i < fsize; i++) {
            Label frameLabel = (Label) labels.get(i);
            mv.visitLabel(restoreLabels[i]);

            MethodInsnNode mnode = (MethodInsnNode) nodes.get(i);

            Frame frame = analyzer.getFrames()[analyzer.getIndex(mnode)];

            // locals
            int lsize = frame.getLocals();
            for (int j = lsize - 1; j >= 0; j--) {
                BasicValue value = (BasicValue) frame.getLocal(j);
                if (value == null) {
                    mv.visitInsn(ACONST_NULL);
                    mv.visitVarInsn(ASTORE, j);
                } else if (value == BasicValue.UNINITIALIZED_VALUE) {
                    // TODO ??
                } else if (value == BasicValue.RETURNADDRESS_VALUE) {
                    // TODO ??
                } else {
                    mv.visitVarInsn(ALOAD, stackRecorderVar);
                    Type type = value.getType();
                    if (value.isReference()) {
                        mv.visitMethodInsn(INVOKEVIRTUAL, STACK_RECORDER, POP_METHOD + "Object", "()Ljava/lang/Object;");
                        Type t = value.getType();
                        String desc = t.getDescriptor();
                        if (desc.charAt(0) == '[') {
                            mv.visitTypeInsn(CHECKCAST, desc);
                        } else {
                            mv.visitTypeInsn(CHECKCAST, t.getInternalName());
                        }
                        mv.visitVarInsn(ASTORE, j);

                    } else {
                        mv.visitMethodInsn(INVOKEVIRTUAL, STACK_RECORDER, getPopMethod(type), "()" + type.getDescriptor());
                        mv.visitVarInsn(type.getOpcode(ISTORE), j);
                    }
                }
            }

            // stack
            int argSize = Type.getArgumentTypes(mnode.desc).length;
            int ownerSize = mnode.getOpcode() == INVOKESTATIC ? 0 : 1;  // TODO
            int initSize = mnode.name.equals("<init>") ? 2 : 0;
            int ssize = frame.getStackSize();
            for (int j = 0; j < ssize - argSize - ownerSize - initSize; j++) {
                BasicValue value = (BasicValue) frame.getStack(j);
                if (value == null) {
                    mv.visitInsn(ACONST_NULL);
                } else if (value == BasicValue.UNINITIALIZED_VALUE) {
                    // TODO ??
                } else if (value == BasicValue.RETURNADDRESS_VALUE) {
                    // TODO ??
                } else if (value.isReference()) {
                    mv.visitVarInsn(ALOAD, stackRecorderVar);
                    mv.visitMethodInsn(INVOKEVIRTUAL, STACK_RECORDER, POP_METHOD + "Object", "()Ljava/lang/Object;");
                    mv.visitTypeInsn(CHECKCAST, value.getType().getInternalName());
                } else {
                    Type type = value.getType();
                    mv.visitVarInsn(ALOAD, stackRecorderVar);
                    mv.visitMethodInsn(INVOKEVIRTUAL, STACK_RECORDER, getPopMethod(type), "()" + type.getDescriptor());
                }
            }

            if (mnode.getOpcode() != INVOKESTATIC) {
                mv.visitVarInsn(ALOAD, stackRecorderVar);
                mv.visitMethodInsn(INVOKEVIRTUAL, STACK_RECORDER, POP_METHOD + "Reference", "()Ljava/lang/Object;");
                mv.visitTypeInsn(CHECKCAST, ((BasicValue) frame.getStack(ssize - argSize - 1)).getType().getInternalName());
            }

            // Create null types for the parameters of the method invocation
            Type[] paramTypes = Type.getArgumentTypes(mnode.desc);
            for (int j = 0; j < paramTypes.length; j++) {
                pushDefault(paramTypes[j]);
            }

            // continue to the next method
            mv.visitJumpInsn(GOTO, frameLabel);
        }

        // end of start block
        mv.visitLabel(l0);
    }

    public void visitLabel(Label label) {
        if (currentIndex < labels.size() && label == labels.get(currentIndex)) {
            int i = analyzer.getIndex(nodes.get(currentIndex));
            currentFrame = analyzer.getFrames()[i];
        }
        mv.visitLabel(label);
    }

    public void visitMethodInsn(int opcode, String owner, String name, String desc) {
        mv.visitMethodInsn(opcode, owner, name, desc);

        if (currentFrame != null) {
            Label fl = new Label();

            mv.visitVarInsn(ALOAD, stackRecorderVar);
            mv.visitJumpInsn(IFNULL, fl);
            mv.visitVarInsn(ALOAD, stackRecorderVar);
            mv.visitFieldInsn(GETFIELD, STACK_RECORDER, "isCapturing", "Z");
            mv.visitJumpInsn(IFEQ, fl);

            // save stack
            Type returnType = Type.getReturnType(desc);
            boolean hasReturn = returnType != Type.VOID_TYPE;
            if (hasReturn) {
                mv.visitInsn(returnType.getSize() == 1 ? POP : POP2);
            }

            Type[] params = Type.getArgumentTypes(desc);
            int argSize = params.length;
            int ownerSize = opcode == INVOKESTATIC ? 0 : 1;  // TODO
            int ssize = currentFrame.getStackSize() - argSize - ownerSize;
            for (int i = 0; i < ssize; i++) {
                BasicValue value = (BasicValue) currentFrame.getStack(i);
                if (value == null) {
                    mv.visitInsn(POP);
                } else if (value == BasicValue.UNINITIALIZED_VALUE) {
                    // TODO ??
                } else if (value.isReference()) {
                    mv.visitVarInsn(ALOAD, stackRecorderVar);
                    mv.visitInsn(SWAP);
                    mv.visitMethodInsn(INVOKEVIRTUAL, STACK_RECORDER, PUSH_METHOD + "Object", "(Ljava/lang/Object;)V");
                } else {
                    Type type = value.getType();
                    mv.visitVarInsn(ALOAD, stackRecorderVar);
                    if (type.getSize() > 1) {
                        mv.visitInsn(DUP);  // dummy stack entry
                        mv.visitInsn(DUP2_X2);  // swap2 for long/double
                        mv.visitInsn(POP2);
                        mv.visitMethodInsn(INVOKEVIRTUAL, STACK_RECORDER, getPushMethod(type), "(" + type.getDescriptor() + ")V");
                        mv.visitInsn(POP);  // remove dummy stack entry
                    } else {
                        mv.visitInsn(SWAP);
                        mv.visitMethodInsn(INVOKEVIRTUAL, STACK_RECORDER, getPushMethod(type), "(" + type.getDescriptor() + ")V");
                    }
                }
            }

            if (!isStatic) {
                mv.visitVarInsn(ALOAD, stackRecorderVar);
                mv.visitVarInsn(ALOAD, 0);
                mv.visitMethodInsn(INVOKEVIRTUAL, STACK_RECORDER, PUSH_METHOD + "Reference", "(Ljava/lang/Object;)V");
            }

            // save locals
            int fsize = currentFrame.getLocals();
            for (int j = 0; j < fsize; j++) {
                BasicValue value = (BasicValue) currentFrame.getLocal(j);
                if (value == null) {
                    // no need to save null
                } else if (value == BasicValue.UNINITIALIZED_VALUE) {
                    // no need to save uninitialized objects
                } else if (value.isReference()) {
                    mv.visitVarInsn(ALOAD, stackRecorderVar);
                    mv.visitVarInsn(ALOAD, j);
                    mv.visitMethodInsn(INVOKEVIRTUAL, STACK_RECORDER, PUSH_METHOD + "Object", "(Ljava/lang/Object;)V");
                } else {
                    mv.visitVarInsn(ALOAD, stackRecorderVar);
                    Type type = value.getType();
                    mv.visitVarInsn(type.getOpcode(ILOAD), j);
                    mv.visitMethodInsn(INVOKEVIRTUAL, STACK_RECORDER, getPushMethod(type), "(" + type.getDescriptor() + ")V");
                }
            }

            mv.visitVarInsn(ALOAD, stackRecorderVar);
            mv.visitIntInsn(BIPUSH, currentIndex);  // TODO optimize to iconst_0...
            mv.visitMethodInsn(INVOKEVIRTUAL, STACK_RECORDER, "pushInt", "(I)V");

            Type methodReturnType = Type.getReturnType(methodDesc);
            pushDefault(methodReturnType);
            mv.visitInsn(methodReturnType.getOpcode(IRETURN));
            mv.visitLabel(fl);

            currentIndex++;
            currentFrame = null;
        }
    }


    public void visitMaxs(int maxStack, int maxLocals) {
        Label endLabel = new Label();
        mv.visitLabel(endLabel);

        mv.visitLocalVariable("__stackRecorder", "L" + STACK_RECORDER + ";", null, startLabel, endLabel, stackRecorderVar);

        mv.visitMaxs(0, 0);
    }

    void pushDefault(Type type) {
        switch (type.getSort()) {
        case Type.VOID:
            break;
        case Type.DOUBLE:
            mv.visitInsn(DCONST_0);
            break;
        case Type.LONG:
            mv.visitInsn(LCONST_0);
            break;
        case Type.FLOAT:
            mv.visitInsn(FCONST_0);
            break;
        case Type.OBJECT:
        case Type.ARRAY:
            mv.visitInsn(ACONST_NULL);
            break;
        default:
            mv.visitInsn(ICONST_0);
            break;
        }
    }

    private static String[] SUFFIXES = {
        "Object",  // 0 void
        "Int",     // 1 boolean
        "Int",     // 2 char
        "Int",     // 3 byte
        "Int",     // 4 short
        "Int",     // 5 int
        "Float",   // 6 float
        "Long",    // 7 long
        "Double",  // 8 double
        "Object",  // 9 array
        "Object",  // 10 object
    };


    String getPopMethod(Type type) {
        return POP_METHOD + SUFFIXES[type.getSort()];
    }

    String getPushMethod(Type type) {
        return PUSH_METHOD + SUFFIXES[type.getSort()];
    }

}

