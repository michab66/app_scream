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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;


import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnNode;
import org.objectweb.asm.tree.MethodInsnNode;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.VarInsnNode;
import org.objectweb.asm.tree.analysis.Analyzer;
import org.objectweb.asm.tree.analysis.AnalyzerException;
import org.objectweb.asm.tree.analysis.DataflowInterpreter;
import org.objectweb.asm.tree.analysis.DataflowValue;
import org.objectweb.asm.tree.analysis.Frame;
import org.objectweb.asm.tree.analysis.SimpleVerifier;


/**
 * ContinuationMethodAdapter
 *
 * @author Evgueni Koulechov
 */
public class ContinuationMethodAnalyzer extends MethodNode implements Opcodes {
    protected final String className;
    protected final ClassVisitor cv;
    protected final MethodVisitor mv;

    protected final List labels = new ArrayList();
    protected final List nodes = new ArrayList();
    protected final List methods = new ArrayList();

    protected Analyzer analyzer;
    public int stackRecorderVar;


    public ContinuationMethodAnalyzer(String className, ClassVisitor cv,
                                      MethodVisitor mv, int access, String name, String desc, String signature, String[] exceptions) {
        super(access, name, desc, signature, exceptions);
        this.className = className;
        this.cv = cv;
        this.mv = mv;
    }

    public void visitMethodInsn(int opcode, String owner, String name, String desc) {
        MethodInsnNode mnode = new MethodInsnNode(opcode, owner, name, desc);
        if (opcode == INVOKESPECIAL || "<init>".equals(name)) {
            methods.add(mnode);
        }
        if (needsFrameGuard(opcode, owner, name, desc) /* && transformer.inScope(owner, name)*/) {
            Label label = new Label();
            super.visitLabel(label);
            labels.add(label);
            nodes.add(mnode);
        }
        instructions.add(mnode);
    }

    public void visitEnd() {
        if (instructions.size() == 0 || labels.size() == 0) {
            accept(mv);
            return;
        }

        this.stackRecorderVar = maxLocals;
        try {
            moveNew();

//          TraceMethodVisitor mv = new TraceMethodVisitor();
//          System.err.println(name + desc);
//          for (int j = 0; j < instructions.size(); ++j) {
//              ((AbstractInsnNode) instructions.get(j)).accept(mv);
//              System.err.print("   " + mv.text.get(j)); // mv.text.get(j));
//          }
//          System.err.println();

            // analyzer = new Analyzer(new BasicVerifier());
            analyzer = new Analyzer(new SimpleVerifier() {
                protected Class getClass(Type t) {
                    try {
                        if (t.getSort() == Type.ARRAY) {
                            return Class.forName(t.getDescriptor().replace('/', '.'), true, Thread.currentThread().getContextClassLoader());
                        }
                        return Class.forName(t.getClassName(), true, Thread.currentThread().getContextClassLoader());
                    } catch (ClassNotFoundException e) {
                        throw new RuntimeException(e.toString());
                    }
                }
            });
            analyzer.analyze(className, this);
            accept(new ContinuationMethodAdapter(this));

        } catch (AnalyzerException ex) {
            // TODO log the error or fail?
            ex.printStackTrace();
            accept(mv);

        }
    }

    void moveNew() throws AnalyzerException {
        DataflowInterpreter i = new DataflowInterpreter();
        Analyzer a = new Analyzer(i);
        a.analyze(className, this);

        HashMap movable = new HashMap();

        Frame[] frames = a.getFrames();
        for (int j = 0; j < methods.size(); j++) {
            MethodInsnNode mnode = (MethodInsnNode) methods.get(j);
            // require to move NEW instruction
            int n = a.getIndex(mnode);
            Frame f = frames[n];
            Type[] args = Type.getArgumentTypes(mnode.desc);

            DataflowValue v = (DataflowValue) f.getStack(f.getStackSize() - args.length - 1);
            Set insns = v.insns;
            for (Iterator it = insns.iterator(); it.hasNext();) {
                AbstractInsnNode ins = (AbstractInsnNode) it.next();
                if (ins.getOpcode() == NEW) {
                    movable.put(ins, mnode);
                } else {
                    // other known patterns
                    int n1 = a.getIndex(ins);
                    if (ins.getOpcode() == DUP) { // <init> with params
                        AbstractInsnNode ins1 = (AbstractInsnNode) instructions.get(n1 - 1);
                        if (ins1.getOpcode() == NEW) {
                            movable.put(ins1, mnode);
                        }
                    } else if (ins.getOpcode() == SWAP) {  // in exception handler
                        AbstractInsnNode ins1 = (AbstractInsnNode) instructions.get(n1 - 1);
                        AbstractInsnNode ins2 = (AbstractInsnNode) instructions.get(n1 - 2);
                        if (ins1.getOpcode() == DUP_X1 && ins2.getOpcode() == NEW) {
                            movable.put(ins2, mnode);
                        }
                    }
                }
            }
        }

        int updateMaxStack = 0;
        for (Iterator it = movable.entrySet().iterator(); it.hasNext();) {
            Map.Entry e = (Map.Entry) it.next();
            AbstractInsnNode node1 = (AbstractInsnNode) e.getKey();
            int n1 = instructions.indexOf(node1);
            AbstractInsnNode node2 = (AbstractInsnNode) instructions.get(n1 + 1);
            AbstractInsnNode node3 = (AbstractInsnNode) instructions.get(n1 + 2);
            int producer = node2.getOpcode();

            instructions.remove(node1);  // NEW
            boolean requireDup = false;
            if (producer == DUP) {
                instructions.remove(node2);  // DUP
                requireDup = true;
            } else if (producer == DUP_X1) {
                instructions.remove(node2);  // DUP_X1
                instructions.remove(node3);  // SWAP
                requireDup = true;
            }

            MethodInsnNode mnode = (MethodInsnNode) e.getValue();
            int nm = instructions.indexOf(mnode);

            int varOffset = stackRecorderVar + 1;
            Type[] args = Type.getArgumentTypes(mnode.desc);

            // optimizations for some common cases
            if (args.length == 0) {
                instructions.add(nm++, node1);  // NEW
                if (requireDup) {
                    instructions.add(nm++, new InsnNode(DUP));
                }
                continue;
            }

            if (args.length == 1 && args[0].getSize() == 1) {
                instructions.add(nm++, node1);  // NEW
                if (requireDup) {
                    instructions.add(nm++, new InsnNode(DUP));
                    instructions.add(nm++, new InsnNode(DUP2_X1));
                    instructions.add(nm++, new InsnNode(POP2));
                    updateMaxStack = updateMaxStack < 2 ? 2 : updateMaxStack; // a two extra slots for temp values
                } else {
                    instructions.add(nm++, new InsnNode(SWAP));
                }
                continue;
            }

            // TODO this one untested!
            if ((args.length == 1 && args[0].getSize() == 2) ||
                (args.length == 2 && args[0].getSize() == 1 && args[1].getSize() == 1)) {
                instructions.add(nm++, node1);  // NEW
                if (requireDup) {
                    instructions.add(nm++, new InsnNode(DUP));
                    instructions.add(nm++, new InsnNode(DUP2_X2));
                    instructions.add(nm++, new InsnNode(POP2));
                    updateMaxStack = updateMaxStack < 2 ? 2 : updateMaxStack; // a two extra slots for temp values
                } else {
                    instructions.add(nm++, new InsnNode(DUP_X2));
                    instructions.add(nm++, new InsnNode(POP));
                    updateMaxStack = updateMaxStack < 1 ? 1 : updateMaxStack; // an extra slot for temp value
                }
                continue;
            }

            // generic code using temporary locals
            // save stack
            for (int j = args.length - 1; j >= 0; j--) {
                Type type = args[j];
                instructions.add(nm++, new VarInsnNode(type.getOpcode(ISTORE), varOffset));
                varOffset += type.getSize();
            }
            if (varOffset > maxLocals) {
                maxLocals = varOffset;
            }

            instructions.add(nm++, node1);  // NEW
            if (requireDup) {
                instructions.add(nm++, new InsnNode(DUP));
            }

            // restore stack
            for (int j = 0; j < args.length; j++) {
                Type type = args[j];
                varOffset -= type.getSize();
                instructions.add(nm++, new VarInsnNode(type.getOpcode(ILOAD), varOffset));
                // clean up store to avoid memory leak?
                if (type.getSort() == Type.OBJECT || type.getSort() == Type.ARRAY) {
                    updateMaxStack = updateMaxStack < 1 ? 1 : updateMaxStack; // an extra slot for ACONST_NULL
                    instructions.add(nm++, new InsnNode(ACONST_NULL));
                    instructions.add(nm++, new VarInsnNode(type.getOpcode(ISTORE), varOffset));
                }
            }
        }

        maxStack += updateMaxStack;
    }

    // TODO
    boolean needsFrameGuard(int opcode, String owner, String name, String desc) {
        if (opcode == Opcodes.INVOKEINTERFACE ||
            (opcode == Opcodes.INVOKESPECIAL && !"<init>".equals(name)) ||
            opcode == Opcodes.INVOKESTATIC ||
            opcode == Opcodes.INVOKEVIRTUAL) {
            return true;
        }
        return false;
    }

}