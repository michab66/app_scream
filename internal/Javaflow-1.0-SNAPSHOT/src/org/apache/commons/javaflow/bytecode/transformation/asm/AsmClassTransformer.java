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

import java.io.IOException;
import java.io.InputStream;

import org.apache.commons.javaflow.bytecode.transformation.ResourceTransformer;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.util.CheckClassAdapter;


/**
 * AsmClassTransformer
 * 
 * @author Eugene Kuleshov
 */
public final class AsmClassTransformer implements ResourceTransformer {
    private final static Log log = LogFactory.getLog(AsmClassTransformer.class);


    public byte[] transform(InputStream is) throws IOException {
        return transform(new ClassReader(is));
    }

    public byte[] transform(final byte[] original) {
        return transform(new ClassReader(original));
    }

    private byte[] transform(ClassReader cr) {
        // final ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
        final ClassWriter cw = new ClassWriter(true, false);

        // print bytecode before transformation
        // cr.accept(new TraceClassVisitor(new ContinuationClassAdapter(this, cw), new PrintWriter(System.out)), false);

        // prints bytecode after transformation
        // cr.accept(new ContinuationClassAdapter(this, new TraceClassVisitor(cw, new PrintWriter(System.err))), 0);
        // cr.accept(new ContinuationClassAdapter(this, new TraceClassVisitor(cw, new PrintWriter(System.err))), false);

        cr.accept(new ContinuationClassAdapter(new CheckClassAdapter(cw)), false);

        byte[] bytecode = cw.toByteArray();

        // CheckClassAdapter.verify(new ClassReader(bytecode), true);
        // new ClassReader(bytecode).accept(new ASMifierClassVisitor(new PrintWriter(System.err)), false);

        // ClassWriter cww = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
        // new ClassReader(bytecode).accept(cww, ClassReader.SKIP_DEBUG);
        // new ClassReader(cww.toByteArray()).accept(new TraceClassVisitor(new PrintWriter(System.err)), 0);

        return bytecode;
    }

}

