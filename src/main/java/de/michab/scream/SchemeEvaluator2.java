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
import java.util.logging.Level;
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
class SchemeEvaluator2 implements ScriptEngine
{
    private static Logger _log =
            Logger.getLogger( SchemeEvaluator2.class.getName() );

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
     * The stream that receives output.
     */
    //private final Writer _sink;

    /**
     * The reader.
     */
    //private final SchemeReader _source;

    /**
     * This interpreter's top level environment.
     */
    private final Environment _tle;

    /**
     * Create a SchemeEvaluator.
     *
     * @param source A reference to a reader that is used as the incoming source
     *        of scheme expressions to evaluate.
     * @param sink A reference to a writer that receives the evaluation results.
     * @param tle An environment used for evaluating the incoming expressions.
     */
    SchemeEvaluator2(
            Environment tle )
    {
        _tle = tle;
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
    {
        Thread currentThread = Thread.currentThread();

        try
        {
            // Before starting the evaluation we add symbols for error handling to
            // the TLE.
            environment.set( ERROR_ID, Cons.NIL );
            environment.set( ERROR_OBJ, Cons.NIL );

            FirstClassObject result = null;

            // This is the read-eval-print loop.
            while ( ! currentThread .isInterrupted() )
            {
                try
                {
                    FirstClassObject expression =
                            sreader.getExpression();

                    // In case we received an EOF the evaluator thread can finish.
                    if ( expression == Port.EOF )
                        break;

                    // Evaluate the expression...
                    result =
                            saveEval( expression, environment );
                    // ...and print the result.
                    sink.write( FirstClassObject.stringize( result ) );
                }
                catch ( RuntimeX e )
                {
                    environment.assign( ERROR_ID, SchemeInteger.createObject( e.getId() ) );
                    environment.assign( ERROR_OBJ, new SchemeObject( e ) );

                    // Print the name of the operation that reported the problem.
                    Symbol operationName = e.getOperationName();
                    if ( operationName == null )
                        operationName = Symbol.createObject( "top-level" );
                    sink.write( operationName.toString() );
                    sink.write( " : " );
                    // Write the actual error message.
                    sink.write( e.getMessage() );
                }
                catch ( Error e )
                {
                    sink.write( e.getMessage() + " " + e );
                }
                finally
                {
                    sink.write( '\n' );
                    sink.flush();
                }
            }

            return result;
        }
        catch ( Exception e )
        {
            // Should be impossible to reach.
            _log.log(
                    Level.SEVERE,
                    "Internal error in " + SchemeEvaluator2.class,
                    e );
            System.exit( 1 );
        }

        return null;
    }

    //    /**
    //     * The thread's worker method.  Finishes either if an EOF token is received
    //     * on the input queue, or if the the thread is interrupted.
    //     */
    //    @Override
    //    public void run()
    //    {
    //        Thread currentThread = Thread.currentThread();
    //
    //        try
    //        {
    //            // Before starting the evaluation we add symbols for error handling to
    //            // the TLE.
    //            _tle.set( ERROR_ID, Cons.NIL );
    //            _tle.set( ERROR_OBJ, Cons.NIL );
    //
    //            // This is the read-eval-print loop.
    //            while ( ! currentThread .isInterrupted() )
    //            {
    //                try
    //                {
    //                    FirstClassObject expression =
    //                            _source.getExpression();
    //
    //                    // In case we received an EOF the evaluator thread can finish.
    //                    if ( expression == Port.EOF )
    //                        break;
    //
    //                    // Evaluate the expression...
    //                    FirstClassObject result =
    //                            saveEval( expression, _tle );
    //                    // ...and print the result.
    //                    _sink.write( FirstClassObject.stringize( result ) );
    //                }
    //                catch ( RuntimeX e )
    //                {
    //                    _tle.assign( ERROR_ID, SchemeInteger.createObject( e.getId() ) );
    //                    _tle.assign( ERROR_OBJ, new SchemeObject( e ) );
    //
    //                    // Print the name of the operation that reported the problem.
    //                    Symbol operationName = e.getOperationName();
    //                    if ( operationName == null )
    //                        operationName = Symbol.createObject( "top-level" );
    //                    _sink.write( operationName.toString() );
    //                    _sink.write( " : " );
    //                    // Write the actual error message.
    //                    _sink.write( e.getMessage() );
    //                }
    //                catch ( Error e )
    //                {
    //                    _sink.write( e.getMessage() + " " + e );
    //                }
    //                finally
    //                {
    //                    _sink.write( '\n' );
    //                    _sink.flush();
    //                }
    //            }
    //        }
    //        catch ( Exception e )
    //        {
    //            // Should be impossible to reach.
    //            _log.log(
    //                    Level.SEVERE,
    //                    "Internal error in " + getClass(),
    //                    e );
    //            System.exit( 1 );
    //        }
    //    }


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
        return evalImpl(
                _tle,
                new SchemeReader( reader),
                new SchemeContext().getWriter() );
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

    @Override
    public ScriptContext getContext() {
        // TODO Auto-generated method stub
        return null;
    }


    @Override
    public void setContext(ScriptContext context) {
        // TODO Auto-generated method stub

    }

    @Override
    public ScriptEngineFactory getFactory() {
        // TODO Auto-generated method stub
        return null;
    }
}
