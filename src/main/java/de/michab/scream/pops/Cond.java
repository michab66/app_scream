/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import java.util.ArrayList;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.SchemeBoolean;
import de.michab.scream.Syntax;
import de.michab.scream.util.BiFunctionX;
import urschleim.Continuation.Cont;
import urschleim.Continuation.Thunk;

/**
 * The implementation of the primitive <code>cond</code> operation.  This is
 * the result of a compile operation.
 *
 * This primitive is centered around an expression map that looks like <code>
 * <br>
 * +-------------+--------+-----+<br>
 * | condition 1 | expr 1 | ... |<br>
 * +-------------+--------+-----+-----+<br>
 * | condition 2 | expr 1 | ... | ... |<br>
 * +-------------+--------+-----+-----+<br>
 * | ...         | expr 1 | ... |<br>
 * +-------------+--------+-----+-----+-----+<br>
 * | condition n | expr 1 | ... | ... | ... |<br>
 * +-------------+--------+-----+-----+-----+<br>
 * <br></code>
 * The conditions are evaluated from 1 to n.  As soon a condition evaluates
 * to true the expression sequence gets evaluated from expr1 to n and the
 * result of the last one is returned.
 */
public class Cond
    extends Syntax
{
    private final BiFunctionX<Environment, Cont<FirstClassObject>, Thunk> thunk;

    /**
     * The clause map.
     */
    private final FirstClassObject[][] _clauseMap;

    /**
     * Creates the primitive based on the expression map.
     *
     * @param clauses The expression map.
     */
    public Cond( FirstClassObject[][] clauses )
    {
        super( "popCond" );
        _clauseMap = clauses;

        ArrayList<Cons> clausesList = new ArrayList<>( clauses.length );

        for ( var clause : clauses )
            clausesList.add( Cons.create( clause ) );

        Cons clauseCons =
                Cons.create( clausesList.toArray( new FirstClassObject[clauses.length] ) );

        thunk = null;
    }

    /**
     * Executes the compiled syntax.
     *
     * @param p The execution environment.
     * @return The result of the syntax execution.
     * @throws RuntimeX In case of an execution error.
     */
    @Override
    public FirstClassObject evaluate( Environment p )
            throws RuntimeX
    {
        for ( int i = 0 ; i < _clauseMap.length ; i++ )
        {
            FirstClassObject cond = evaluate( _clauseMap[i][0], p );
            if ( cond != SchemeBoolean.F )
            {
                return ( _clauseMap[i].length > 1 ) ?
                        interpretTailSequence( _clauseMap[i], 1, p ) :
                            cond;
            }
        }

        return Cons.NIL;
    }
}
