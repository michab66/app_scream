/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.util.Objects;
import java.util.Stack;
import java.util.logging.Logger;

import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptException;

import org.smack.util.Holder;

import de.michab.scream.Scream.Cont;
import de.michab.scream.Scream.FcoOp;
import de.michab.scream.binding.SchemeObject;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Port;
import de.michab.scream.fcos.PortIn;
import de.michab.scream.fcos.PortOut;
import de.michab.scream.fcos.SchemeString;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.frontend.SchemeParser;
import de.michab.scream.pops.Primitives;
import de.michab.scream.util.Continuation;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Continuation.ToStackOp;
import de.michab.scream.util.Continuation2;
import de.michab.scream.util.LoadContext;
import de.michab.scream.util.SupplierX;

/**
 * Contains a Scheme top level read-eval-print loop.  A SchemeEvaluator reads
 * scheme expressions from a SchemeReader and writes the results of the
 * evaluation to its sink.
 *
 * @author Michael Binz
 */
public final class ScreamEvaluator implements ScriptEngine
{
    @SuppressWarnings("unused")
    private static Logger _log =
            Logger.getLogger( ScreamEvaluator.class.getName() );

    /**
     * The symbol being bound to an object reference of the interpreter itself.
     */
    private final static Symbol ANCHOR_SYMBOL =
            Symbol.createObject( "scream::evaluator" );

    /**
     * The interaction environment.
     * <p>
     * {@code r7rs 6.12 p55}
     */
    private final Environment _interaction;

    /**
     * The scheme-report environment.
     * <p>
     * {@code r7rs 6.12 p55}
     */
    private final Environment _schemeReport;

    private final Stack<ScriptContext> _context = new Stack<>();

    private final Scream _factory;


    /**
     * Create a SchemeEvaluator.
     *
     * @param tle An environment used for evaluating the incoming expressions.
     */
    ScreamEvaluator(
            Scream interpreter,
            Environment tle,
            String extensions)
        throws RuntimeX
    {
        _context.push(  new SchemeContext() );

        _factory =
                interpreter;
        _schemeReport =
                tle.extend( "tle-interpreter" );
        _schemeReport.define(
                Symbol.createObject( "scream:tle-interpreter" ),
                _schemeReport );
        _schemeReport.define(
                ANCHOR_SYMBOL,
                new SchemeObject( this ) );
        Scream.addExtensions(
                _schemeReport,
                extensions );
        FirstClassObject.setConstant(
                _schemeReport );
        _interaction = _schemeReport.extend(
                "interaction" );

        _interaction.define(
                Symbol.createObject( "scream:interaction" ),
                _interaction );
    }

    /**
     * Get the standard input port for this interpreter instance.
     *
     * @return This interpreter's standard in.
     */
    public Port getInPort()
    {
        return new PortIn(
                "stdin",
                _context.peek().getReader(),
                false );
    }

    /**
     * Get the standard output port for this interpreter instance.
     *
     * @return This interpreter's standard out.
     */
    public Port getOutPort()
    {
        return new PortOut(
                "stdout",
                _context.peek().getWriter(),
                false );
    }

    /**
     * Get the standard error port for this interpreter instance.
     *
     * @return This interpreter's standard error port.
     */
    public Port getErrorPort()
    {
        return new PortOut(
                "stderr",
                _context.peek().getErrorWriter(),
                false );
    }

    /**
     * {@code r7rs 6.12 p55}
     *
     * @return the scheme-interaction-environment.
     */
    public Environment getInteraction()
    {
        return _interaction;
    }

    /**
     * {@code r7rs 6.12 p55}
     *
     * @return the scheme-report-environment.
     */
    public Environment getSchemeReport()
    {
        return _interaction;
    }

    /**
     * Loads the scheme source file in the port into the passed environment.  The
     * port is closed before the file's contents is evaluated.
     *
     * @param filename The name of the file to load.
     * @throws RuntimeX In case of errors.
     */
    private void load( SchemeString filename )
            throws RuntimeX
    {
        Scream.load( filename, _interaction );
    }

    @Override
    public Object eval(String script, ScriptContext context) throws ScriptException
    {
        try
        {
            _context.push( context );
            return eval( script );
        }
        finally
        {
            _context.pop();
        }
    }

    @Override
    public Object eval(Reader reader, ScriptContext context) throws ScriptException {
        try
        {
            _context.push( context );
            return eval( reader );
        }
        finally
        {
            _context.pop();
        }
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
            return FirstClassObject.toString( evalFco( reader ) );
//            if ( result == Cons.NIL )
//                return null;
//            return result.toJava();
        }
        catch ( ScreamException e )
        {
            throw new ScriptException( e );
        }
    }

    /**
     * A holder used in continuation processing.  This has to be defined here
     * since in case of top-level continuations it gets captured in a lambda
     * that may be restarted.
     * <p>
     * See the test de.michab.scream.language.R7rs_6_10_Control_features_Test.call_cc_restart_2()
     */
    Holder<FirstClassObject> _result = new Holder<>();

    /**
     * @see #_result
     */
    Holder<Exception> _exception = new Holder<>();

    /**
     *
     */
    Continuation2<FirstClassObject,RuntimeX> continuation = new Continuation2( Runtime.class );

    /**
     * Evaluates the expressions read from the passed Reader in the Scream
     * type system.
     *
     * @param reader Delivers the expressions to be evaluated.
     * @return The evaluation result.
     * @throws RuntimeX In case of an error.
     */
    public FirstClassObject evalFco(Reader reader) throws RuntimeX
    {
            return Scream.evalImpl(
                    _interaction,
                    new SchemeReader( reader )::getExpression,
                    _result,
                    _exception );
    }

    /**
     * Evaluates the passed expressions in the Scream
     * type system.
     *
     * @param script The expressions to be evaluated.
     * @return The evaluation result.
     * @throws RuntimeX In case of an error.
     */
    public FirstClassObject evalFco(String script) throws RuntimeX {
        return evalFco( new StringReader( script ) );
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
    public ScriptContext getContext()
    {
        return _context.peek();
    }

    @Override
    public void setContext( ScriptContext context )
    {
        Objects.requireNonNull( context );

        if ( null == context.getErrorWriter() )
            context.setErrorWriter( Writer.nullWriter() );
        if ( null == context.getReader() )
            context.setReader( Reader.nullReader() );
        if ( null == context.getWriter() )
            context.setErrorWriter( Writer.nullWriter() );

        _context.clear();
        _context.push( context );
    }

    @Override
    public ScriptEngineFactory getFactory()
    {
        return _factory;
    }

///////////////////// Experimental.

    private static Thunk evalImpl_(
            Environment e,
            SupplierX<FirstClassObject,RuntimeX> s,
            FirstClassObject previousResult,
            FirstClassObject newExpression,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        if ( newExpression == Port.EOF )
            return c.accept( previousResult );

        return Primitives._x_eval(
                e,
                newExpression,
                fco -> evalImpl_( e, s, fco, s.get(), c ) );
    }

    private static Cont<FirstClassObject> mapCont( de.michab.scream.util.Continuation.Cont<FirstClassObject> cont )
    {
        return  c -> {
            try
            {
                return cont.accept( c );
            }
            catch ( Exception e )
            {
                throw new InternalError();
            }
        };
    }

    private static ToStackOp<FirstClassObject> mapOp( FcoOp op )
    {
        return c -> op.call( mapCont( c )  );
    }

    @Deprecated
    private static FirstClassObject toStack( FcoOp op )
            throws RuntimeX
    {
        ToStackOp<FirstClassObject> tso2 = mapOp( op );

        try
        {
            return Continuation.toStack( tso2 );
        }
        catch (Exception e) {
            if ( RuntimeX.class.isAssignableFrom( e.getClass() ))
                throw RuntimeX.class.cast( e );

            throw RuntimeX.mInternalError( e );
        }
    }
    public static Thunk evalImpl(
            Environment e,
            SupplierX<FirstClassObject,RuntimeX> s,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        return evalImpl_(
                e,
                s,
                Cons.NIL,
                s.get(),
                c );
    }

    @Deprecated
    private static FirstClassObject evalImpl(
            Environment env,
            SupplierX<FirstClassObject,RuntimeX> spl )
                    throws RuntimeX
    {
        return toStack(
                c -> evalImpl( env, spl, c ) );
    }

    /**
     * Load a Scheme source file.
     *
     * @param file The name of the file to load.
     * @throws RuntimeX In case of errors.
     */
    private static FirstClassObject load( LoadContext file, Environment e )
            throws RuntimeX
    {
        try ( var reader  = LoadContext.getReader( file ) )
        {
            SchemeParser parser =
                    new SchemeParser( reader );

            return evalImpl( e, parser::getExpression );
        }
        catch ( IOException ioe )
        {
            throw RuntimeX.mIoError( ioe );
        }
    }

    /**
     * Loads a Scheme source file into the passed environment.
     *
     * @param filename The name of the file to load.
     * @throws RuntimeX In case of errors.
     */
    public static FirstClassObject load( SchemeString filename, Environment environment )
            throws RuntimeX
    {
        return load( new LoadContext( filename.getValue() ), environment );
    }


}
