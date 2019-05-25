package org.apache.commons.javaflow;

import org.apache.commons.javaflow.bytecode.StackRecorder;

/**
 * This exception is used to signal
 * a control flow change that needs
 * the cooperation inside {@link StackRecorder}.
 *
 * <p>
 * This class is only for javaflow internal code.
 *
 * @author Kohsuke Kawaguchi
 */
public final class ContinuationDeath extends Error {
    final String mode;

    public ContinuationDeath(String mode) {
        this.mode = mode;
    }

    /**
     * Signals that the continuation wants to exit the execution.
     */
    static final String MODE_EXIT = "exit";
    /**
     * Signals that the execution should restart immediately
     * from where it resumed.
     */
    static final String MODE_AGAIN = "again";
    /**
     * Signals that the exeuction should suspend,
     * by using the original continuation.
     */
    static final String MODE_CANCEL = "cancel";
}
