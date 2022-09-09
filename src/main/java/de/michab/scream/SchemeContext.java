/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2022 Michael G. Binz
 */
package de.michab.scream;

import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.Collections;
import java.util.List;

import javax.script.Bindings;
import javax.script.ScriptContext;

public class SchemeContext implements ScriptContext
{
    private Writer _stdout;
    private Writer _stderr;
    private Reader _stdin;

    SchemeContext( Writer stdout, Reader stdin, Writer stderr )
    {
        _stdout = stdout;
        _stdin = stdin;
        _stderr = stderr;
    }

    SchemeContext( Writer stdout, Reader stdin )
    {
        this(
                stdout,
                stdin,
                new OutputStreamWriter(
                        System.err ) );
    }
    SchemeContext( Writer stdout )
    {
        this(
                stdout,
                new InputStreamReader(
                        System.in ),
                new OutputStreamWriter(
                        System.err ) );

    }
    SchemeContext()
    {
        this(
                new OutputStreamWriter(
                        System.out ),
                new InputStreamReader(
                        System.in ),
                new OutputStreamWriter(
                        System.err ) );
    }

    @Override
    public void setBindings( Bindings bindings, int scope )
    {
        // TODO Auto-generated method stub

    }

    @Override
    public Bindings getBindings( int scope )
    {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void setAttribute( String name, Object value, int scope )
    {
        // TODO Auto-generated method stub

    }

    @Override
    public Object getAttribute( String name, int scope )
    {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Object removeAttribute( String name, int scope )
    {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Object getAttribute( String name )
    {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public int getAttributesScope( String name )
    {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public Writer getWriter()
    {
        return _stdout;
    }

    @Override
    public Writer getErrorWriter()
    {
        return _stderr;
    }

    @Override
    public void setWriter( Writer writer )
    {
        _stdout = writer;

    }

    @Override
    public void setErrorWriter( Writer writer )
    {
        _stderr = writer;
    }

    @Override
    public Reader getReader()
    {
        return _stdin;
    }

    @Override
    public void setReader( Reader reader )
    {
        _stdin = reader;
    }

    @Override
    public List<Integer> getScopes()
    {
        return Collections.emptyList();
    }
}
