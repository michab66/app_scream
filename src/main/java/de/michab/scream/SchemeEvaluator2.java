/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import java.io.Reader;
import java.io.StringReader;
import java.util.logging.Logger;

import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptException;

import de.michab.scream.binding.SchemeObject;

/**
 * Contains a Scheme top level read-eval-print loop.  A SchemeEvaluator reads
 * scheme expressions from a SchemeReader and writes the results of the
 * evaluation to its sink.
 *
 * @author Michael Binz
 */
public class SchemeEvaluator2 implements ScriptEngine
{
    @SuppressWarnings("unused")
    private static Logger _log =
            Logger.getLogger( SchemeEvaluator2.class.getName() );

    /**
     * The symbol being bound to an object reference of the interpreter itself.
     */
    public final static Symbol ANCHOR_SYMBOL =
            Symbol.createObject( "%%interpreter%%" );

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

    public Environment getInteraction()
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
    public void load( String filename )
            throws RuntimeX
    {
        SchemeInterpreter2.load( filename, _interaction );
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

    public Object evalFco(String script) throws ScriptException {
        return evalFco( new StringReader( script ) );
    }

    @Override
    public Object eval(Reader reader) throws ScriptException
    {
        var result = evalFco( reader );
        if ( result == Cons.NIL )
            return null;

        try {
            return result.toJava();
        }
        catch ( ScreamException e )
        {
            throw new ScriptException( e );
        }
    }

    public FirstClassObject evalFco(Reader reader) throws ScriptException
    {
        try
        {
            return SchemeInterpreter2.evalImpl(
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
