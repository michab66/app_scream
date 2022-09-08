/* $Id: SchemeEvaluator.java 197 2009-08-03 21:30:27Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */
package de.michab.scream;

import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.util.logging.Logger;

import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptException;

import de.michab.scream.FirstClassObject.Unwind;
import de.michab.scream.binding.SchemeObject;

/**
 * Contains a Scheme top level read-eval-print loop.  A SchemeEvaluator reads
 * scheme expressions from a SchemeReader and writes the results of the
 * evaluation to its sink.
 *
 * @version $Rev: 197 $
 * @author Michael Binz
 */
public class SchemeEvaluator2 implements ScriptEngine
{
    private static Logger _log =
            Logger.getLogger( SchemeEvaluator2.class.getName() );

    /**
     * The symbol being bound to an object reference of the interpreter itself.
     */
    public final static Symbol ANCHOR_SYMBOL =
            Symbol.createObject( "%%interpreter%%" );

    /**
     * Symbol receives an error id when an error occurred.
     */
    private final static Symbol ERROR_ID =
            Symbol.createObject( "%error-id" );

    /**
     * Symbol receives the actual exception when an error occurred.
     */
    private final static Symbol ERROR_OBJ =
            Symbol.createObject( "%error-object" );

    /**
     * This interpreter's top level environment.
     */
    private final Environment _interaction;

    private final SchemeInterpreter2 _factory;

    /**
     * Create a SchemeEvaluator.
     *
     * @param tle An environment used for evaluating the incoming expressions.
     */
    SchemeEvaluator2(
            SchemeInterpreter2 interpreter,
            Environment tle )
    {
        _factory =
                interpreter;
        _interaction =
                tle;
        _interaction.set(
                ANCHOR_SYMBOL,
                new SchemeObject( this ) );
    }

    /**
     * Loads the scheme source file in the port into the passed environment.  The
     * port is closed before the file's contents is evaluated.
     *
     * @param filename The name of the file to load.
     * @throws RuntimeX In case of errors.
     */
    public void load( String filename )
            throws RuntimeX
    {
        _factory.load( filename, _interaction );
    }

    static private FirstClassObject saveEval( FirstClassObject x, Environment e ) throws RuntimeX
    {
        if ( x == Cons.NIL )
            return Cons.NIL;

        try
        {
            return x.evaluate( e );
        }
        catch ( Unwind u )
        {
            return u.result();
        }
    }

    static FirstClassObject evalImpl(
            Environment environment,
            SchemeReader sreader,
            Writer sink )
                    throws ScreamException
    {
        Thread currentThread = Thread.currentThread();

        // Before starting the evaluation we add symbols for error handling to
        // the TLE.
        environment.set( ERROR_ID, Cons.NIL );
        environment.set( ERROR_OBJ, Cons.NIL );

        FirstClassObject result = null;

        // This is the read-eval-print loop.
        while ( ! currentThread .isInterrupted() )
        {
            //                try
            //                {
            FirstClassObject expression =
                    sreader.getExpression();

            if ( expression == Port.EOF )
                break;

            // Evaluate the expression...
            result =
                    saveEval( expression, environment );
            //                    // ...and print the result.
            //                    sink.write( FirstClassObject.stringize( result ) );
            //                }
            //                catch ( RuntimeX e )
            //                {
            //                    environment.assign( ERROR_ID, SchemeInteger.createObject( e.getId() ) );
            //                    environment.assign( ERROR_OBJ, new SchemeObject( e ) );
            //
            //                    // Print the name of the operation that reported the problem.
            //                    Symbol operationName = e.getOperationName();
            //                    if ( operationName == null )
            //                        operationName = Symbol.createObject( "top-level" );
            //                    sink.write( operationName.toString() );
            //                    sink.write( " : " );
            //                    // Write the actual error message.
            //                    sink.write( e.getMessage() );
            //                }
            //                catch ( Error e )
            //                {
            //                    sink.write( e.getMessage() + " " + e );
            //                }
            //                finally
            //                {
            //                    sink.write( '\n' );
            //                    sink.flush();
            //                }
        }

        return result;
    }

    @Override
    public Object eval(String script, ScriptContext context) throws ScriptException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Object eval(Reader reader, ScriptContext context) throws ScriptException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Object eval(String script) throws ScriptException {
        return eval( new StringReader( script ) );
    }

    @Override
    public Object eval(Reader reader) throws ScriptException
    {
        try
        {
            return evalImpl(
                    _interaction,
                    new SchemeReader( reader),
                    _context.getWriter() );
        }
        catch ( ScreamException e )
        {
            throw new ScriptException( e );
        }
    }

    @Override
    public Object eval(String script, Bindings n) throws ScriptException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Object eval(Reader reader, Bindings n) throws ScriptException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void put(String key, Object value) {
        // TODO Auto-generated method stub
    }

    @Override
    public Object get(String key) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Bindings getBindings(int scope) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void setBindings(Bindings bindings, int scope) {
        // TODO Auto-generated method stub

    }

    @Override
    public Bindings createBindings() {
        // TODO Auto-generated method stub
        return null;
    }

    private ScriptContext _context = new SchemeContext();

    @Override
    public ScriptContext getContext()
    {
        return _context;
    }

    @Override
    public void setContext(ScriptContext context)
    {
        _context = context;
    }

    @Override
    public ScriptEngineFactory getFactory()
    {
        return _factory;
    }
}
