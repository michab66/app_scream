/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2024 Michael G. Binz
 */

package de.michab.scream.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import org.smack.util.JavaUtil;
import org.smack.util.StringUtil;

import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;

/**
 * Supports the transformation of an arbitrary cyclic Cons structure
 * into a machine-readable string.
 */
public class ConsToString
{
    private long _enumId = 0;

    private final StringBuilder _string =
            new StringBuilder();

    private final HashMap<Cons,Long> _nodes = new HashMap<>();
    private final HashMap<Cons,Long> _references = new HashMap<>();

    private HashSet<Long> _referred;

    public ConsToString( Cons cons )
    {
        if ( Cons.NIL == cons )
        {
            append( "()" );
            return;
        }

        collectNodes( cons );

        if ( _references.isEmpty() )
        {
            writeConsSimple( cons );
            return;
        }

        _referred = new HashSet<Long>( _references.values() );

        writeCons( cons );
    }


    private long nextEnumId()
    {
        return _enumId++;
    }

    private void append( String format, Object ... objects )
    {
        if ( objects.length == 0 )
            _string.append( format );
        else
            _string.append( format.formatted( objects ) );
    }

    @Override
    public String toString()
    {
        var result = _string.toString().trim();
        System.out.println( result );
        return result;
    }

    private void collectNodes(
            Cons cons,
            Cons previous )
    {
        while ( true )
        {
            if ( cons == Cons.NIL )
                // Proper list end.
                break;

            if ( _nodes.containsKey( cons ) )
            {
                // Circle detected.
                _references.put(
                        previous,
                        _nodes.get( cons ) );
                break;
            }

            // Remember and label the current node.
            _nodes.put(
                    cons,
                    nextEnumId() );

            var cCar = cons.getCar();

            if ( FirstClassObject.is( Cons.class, cCar ) )
                collectNodes(
                        (Cons)cCar,
                        Cons.NIL );

            var cCdr = cons.getCdr();

            if ( ! FirstClassObject.is( Cons.class, cCdr ) )
                // Improper.
                break;

            previous =
                    cons;
            cons =
                    (Cons)cCdr;
        }
    }

    private void collectNodes(
            Cons cons )
    {
        collectNodes( cons, Cons.NIL );
    }

    private String needsLabel( Cons cons )
    {
        var id = _nodes.get( cons );

        if ( _referred.contains( id ) )
            return "#" + id + "=";

        return StringUtil.EMPTY_STRING;
    }

    private void writeConsSimple( Cons cons )
    {
        ArrayList<Runnable> writers = new ArrayList<>();

        while ( true )
        {
            if ( cons == Cons.NIL )
                break;

            var cCar = cons.getCar();

            if ( FirstClassObject.is( Cons.class, cCar ) )
                writers.add( () ->  writeConsSimple( (Cons)cCar )  );
            else
                writers.add(  () -> writeDataSimple( cCar ) );

            var cCdr = cons.getCdr();

            if ( ! FirstClassObject.is( Cons.class, cCdr ) )
            {
                // Improper.
                writers.add( () -> append( "." ) );
                writers.add( () -> writeDataSimple( cCdr ) );
                break;
            }

            cons = (Cons)cCdr;
        }

        append( "(" );

        for ( int i = 0 ; i < writers.size() ; i++ )
        {
            if ( i > 0 )
                append( " " );
            writers.get( i ).run();
        }

        append( ")" );
    }

    private void writeCons(
            Cons cons )
    {
        // Check if this node needs a label.
        var label = needsLabel( cons );
        if ( ! label.isEmpty() )
            append( label );
        append( "(" );

        while ( true )
        {
            if ( cons == Cons.NIL )
                break;

            var cCar = cons.getCar();

            if ( FirstClassObject.is( Cons.class, cCar ) )
                writeCons( (Cons)cCar );
            else
                writeData( cCar );

            // Check if node is followed by a reference.
            var ref = _references.get( cons );
            if ( ref != null )
            {
                append( ". #%d#", ref );
                break;
            }

            var cCdr = cons.getCdr();

            if ( ! FirstClassObject.is( Cons.class, cCdr ) )
            {
                // Improper.
                append( ". " );
                writeData( cCdr );
                break;
            }

            Cons next = (Cons)cCdr;
            if ( ! needsLabel( next ).isEmpty() )
            {
                append( ". " );
                writeCons( next );
                break;
            }

            cons = next;
        }

        appendTrimmed( ")" );
    }

    private void appendTrimmed( String string )
    {
        char lastchar = JavaUtil.make(
                () -> {
                    char[] result = new char[1];
                    var length = _string.length();

                    _string.getChars( length-1, length, result, 0 );

                    return result[0];
                } );

        if ( lastchar == ' ' )
            _string.deleteCharAt( _string.length()-1 );

        append( string );
    }

    private void writeData(
            FirstClassObject cons )
    {
        char lastchar = JavaUtil.make(
                () -> {
                    char[] result = new char[1];
                    var length = _string.length();

                    _string.getChars( length-1, length, result, 0 );

                    return result[0];
                } );

        if ( lastchar == ')' )
            append( " " );

        append( "%s ", cons );
    }

    private void writeDataSimple(
            FirstClassObject cons )
    {
        append( "%s", cons );
    }
}
