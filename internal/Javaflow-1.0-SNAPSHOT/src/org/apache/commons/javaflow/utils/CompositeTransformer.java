package org.apache.commons.javaflow.utils;

import org.apache.commons.javaflow.bytecode.transformation.ResourceTransformer;

/**
 * {@link ResourceTransformer} whose transformation
 * is defined in terms of multiple {@link ResourceTransformer}s.
 *
 * @author Kohsuke Kawaguchi
 */
public class CompositeTransformer implements ResourceTransformer {
    private final ResourceTransformer[] transformers;

    public CompositeTransformer(ResourceTransformer[] transformers) {
        this.transformers = transformers;
    }

    public byte[] transform(byte[] image) {
        for (int i = 0; i < transformers.length; i++) {
            image = transformers[i].transform(image);
        }
        return image;
    }
}
