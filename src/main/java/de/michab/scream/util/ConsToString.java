/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2024 Michael G. Binz
 */

package de.michab.scream.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.StringJoiner;

import org.smack.util.JavaUtil;
import org.smack.util.LongHolder;
import org.smack.util.StringUtil;

import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;

/**
 * Supports the transformation of an arbitrary cyclic Cons structure
 * into a machine-readable string.
 */
public class ConsToString
{
    /**
     * A counter for generating ids.
     */
    private long _enumId = 0;

    /**
     * The result string.
     */
    private final String _string;

    /**
     * Holds all the nodes found while parsing with an assigned unique id.
     */
    private final HashMap<Cons,LongHolder> _nodes =
            new HashMap<>();
    /**
     * Holds all nodes in parsing order.
     */
    private final ArrayList<Cons> _nodesOrdered =
            new ArrayList<>();

    /**
     * Holds the nodes that refer to a node in _nodes.
     */
    private final HashMap<Cons,LongHolder> _references
        = new HashMap<>();

    /**
     * Holds the referred node ids.
     */
    private HashSet<LongHolder> _referred;

    public ConsToString( Cons cons )
    {
        if ( Cons.NIL == cons )
        {
            _string = "()";
            return;
        }

        collectNodes( cons );

        if ( _references.isEmpty() )
        {
            _string = writeConsx( cons );
            return;
        }

        _referred = new HashSet<LongHolder>( _references.values() );

        // Normalize the labels, i.e. renumber the labels starting with 0.

        _nodesOrdered.removeIf( t -> {
            return ! _referred.contains( _nodes.get( t ) ); } );

        JavaUtil.Assert( _nodesOrdered.size() == _references.size() );

        int idx = 0;
        for ( Cons c : _nodesOrdered )
            _nodes.get( c ).set( idx++ );

        _string = writexx( cons );
    }

    private void addNode( Cons cons )
    {
        _nodes.put( cons, new LongHolder( _enumId++ ) );
        _nodesOrdered.add( cons );
    }

    @Override
    public String toString()
    {
        return _string.toString().trim();
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
            addNode( cons );

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

    private String writeConsx( Cons cons )
    {
        StringJoiner result = new StringJoiner( " ", "(", ")" );

        while ( true )
        {
            if ( cons == Cons.NIL )
                break;

            var cCar = cons.getCar();

            if ( FirstClassObject.is( Cons.class, cCar ) )
                result.add( writeConsx( (Cons)cCar )  );
            else
                result.add( write( cCar ) );

            var cCdr = cons.getCdr();

            if ( ! FirstClassObject.is( Cons.class, cCdr ) )
            {
                // Improper.
                result.add( "." );
                result.add( write( cCdr ) );
                break;
            }

            cons = (Cons)cCdr;
        }

        return result.toString();

    }

    private String writexx(
            Cons cons )
    {
        // Check if this node needs a label.
        var label = needsLabel( cons );

        StringJoiner j = new StringJoiner(
                " ",
                label.isEmpty() ?
                        "(" :
                        label + "(",
                ")" );

        while ( true )
        {
            if ( cons == Cons.NIL )
                break;

            var cCar = cons.getCar();

            if ( FirstClassObject.is( Cons.class, cCar ) )
                j.add( writexx( (Cons)cCar ) );
            else
                j.add( write( cCar ) );

            // Check if node is followed by a reference.
            var ref = _references.get( cons );
            if ( ref != null )
            {
                j.add( "." );
                j.add( "#%s#".formatted( ref ) );
                break;
            }

            var cCdr = cons.getCdr();

            if ( ! FirstClassObject.is( Cons.class, cCdr ) )
            {
                // Improper.
                j.add( "." );
                j.add( write( cCdr ) );
                break;
            }

            Cons next = (Cons)cCdr;
            if ( ! needsLabel( next ).isEmpty() )
            {
                j.add( "." );
                j.add( writexx( next ) );
                break;
            }

            cons = next;
        }

        return j.toString();
    }

    private String write(
            FirstClassObject fco )
    {
        return FirstClassObject.toString( fco );
    }
}
