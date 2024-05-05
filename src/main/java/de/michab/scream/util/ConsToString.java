/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2024 Michael G. Binz
 */

package de.michab.scream.util;

import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;

import org.smack.util.CachedHolder;
import org.smack.util.StringUtil;

import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;

/**
 * Supports the transformation of an arbitrary cyclic Cons structure
 * into a machine-readable string.
 */
public class ConsToString
{
    private long enumId = 0;

    private final StringWriter _string =
            new StringWriter();

    CachedHolder<String> _cachedString =
            new CachedHolder<>( _string::toString );

    private final HashMap<Cons,Long> _nodes = new HashMap<>();
    private final HashMap<Cons,Long> _references = new HashMap<>();
    private final HashMap<Long, Cons> _id2cons = new HashMap<>();

    public ConsToString( Cons cons )
    {
        if ( Cons.NIL == cons )
        {
            append( "()" );
            return;
        }

        collectNodes( cons, _nodes, _references );

        for ( var c : _references.keySet() )
            _id2cons.put( _references.get( c ), c );

        writeCons( cons );
    }

    private long nextEnumId()
    {
        return enumId++;
    }

    private void append( String format, Object ... objects )
    {
        _string.write(
                objects.length == 0 ?
                format :
                format.formatted( objects ) );
    }

    @Override
    public String toString()
    {
        return _cachedString.get();
    }

    private void collectNodes(
            Cons cons,
            Cons previous,
            Map<Cons,Long> node2id,
            Map<Cons,Long> references )
    {
        while ( true )
        {
            if ( cons == Cons.NIL )
                // Proper list end.
                break;

            if ( node2id.containsKey( cons ) )
            {
                // Circle detected.
                references.put(
                        previous,
                        node2id.get( cons ) );
                break;
            }

            // Remember and label the current node.
            node2id.put(
                    cons,
                    nextEnumId() );

            var cCar = cons.getCar();

            if ( FirstClassObject.is( Cons.class, cCar ) )
                collectNodes(
                        (Cons)cCar,
                        Cons.NIL,
                        node2id,
                        references );

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
            Cons cons,
            HashMap<Cons,Long> nodes,
            HashMap<Cons,Long> references )
    {
        collectNodes( cons, Cons.NIL, nodes, references );
    }

    private String needsLabel( Cons cons )
    {
        var id = _nodes.get( cons );

        if ( _id2cons.containsKey( id ) )
            return "#" + id + "=";

        return StringUtil.EMPTY_STRING;
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

        append( ")" );
    }

    private void writeData(
            FirstClassObject cons )
    {
        append( "%s ", cons );
    }
}
