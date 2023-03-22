/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream;

import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;

import org.smack.util.ServiceManager;
import org.smack.util.resource.ResourceManager;
import org.smack.util.resource.ResourceManager.Resource;

import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.LogUtil;

/**
 * Facade to the Scheme interpreter.
 *
 * @author Michael G. Binz
 */
public class Scream implements ScriptEngineFactory
{
    /**
     * Reads the resources.  Terminates the application if any problem occurs.
     * This represents the very first phase of boot up.
     */
    static
    {
        org.smack.util.ServiceManager.getApplicationService(
                org.smack.util.resource.ResourceManager.class )
        .injectResources( Scream.class );

        LogUtil.setLevel(
                Level.WARNING,
                Scream.class.getPackage().getName() );
    }

    @Resource
    private static String engineName;
    @Resource
    private static String engineVersion;
    @Resource
    private static String[] extensions;
    @Resource
    private static String[] names;
    @Resource
    private static String languageName;
    @Resource
    private static String languageVersion;

    /**
     * Creates an instance of an working Scheme interpreter.
     *
     * @param in The reader to use.
     * @param out The writer to use by this interpreter.
     */
    public Scream()
    {
        ServiceManager.getApplicationService( ResourceManager.class )
            .injectResources( Scream.class );
    }

    @FunctionalInterface
    public interface FcoOp {
        Thunk call( Cont<FirstClassObject> c )
            throws RuntimeX;
    }

    /**
     * A Scheme continuation.
     *
     * @param <R> The type accepted.
     */
    @FunctionalInterface
    public static interface Cont<R> {
        Thunk accept(R result) throws RuntimeX;
    }

    /**
     * Entry point. Rarely used.
     */
    public static void main( String[] argv ) throws Exception
    {
        Thread.currentThread().setName( "screamMain" );

        var interpreter = new Scream();

        var engine = interpreter.getScriptEngine();

        engine.eval( new InputStreamReader( System.in ) );
    }

    @Override
    public String getEngineName() {
        return engineName;
    }

    @Override
    public String getEngineVersion() {
        return engineVersion;
    }

    @Override
    public List<String> getExtensions() {
        return Arrays.asList( extensions );
    }

    @Override
    public List<String> getMimeTypes() {
        return Collections.emptyList();
    }

    @Override
    public List<String> getNames() {
        return Arrays.asList( names );
    }

    @Override
    public String getLanguageName() {
        return languageName;
    }

    @Override
    public String getLanguageVersion() {
        return languageVersion;
    }

    @Override
    public Object getParameter(String key) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getMethodCallSyntax(String obj, String m, String... args) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getOutputStatement(String toDisplay) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getProgram(String... statements) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ScriptEngine getScriptEngine()
    {
        try {
            return new ScreamEvaluator(
                    this );
        }
        catch ( RuntimeX rx )
        {
            throw new InternalError( rx );
        }
    }
}
