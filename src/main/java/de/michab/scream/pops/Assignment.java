/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

package de.michab.scream.pops;

import java.util.function.BiFunction;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.Symbol;
import de.michab.scream.Syntax;
import urschleim.Continuation;
import urschleim.Continuation.Cont;
import urschleim.Continuation.Thunk;

/**
 * Assignment primitive.
 */
public class Assignment
    extends Syntax
{
    /**
     * The symbol to assign.
     */
    private final Symbol _symbol;

    /**
     * The expression for the assignment.
     */
    private final FirstClassObject _value;

    private final BiFunction<Environment, Cont<FirstClassObject>, Thunk> thunk;

    /**
     * Create an 'Assignment' primitive operation from the passed expression
     * sequence.
     *
     * @param symbol The symbol to assign.
     * @param value The value to assign.
     */
    public Assignment( Symbol symbol, FirstClassObject value )
    {
        super( "popAssignment" );
        _symbol = symbol;
        _value = value;

        thunk = (e,c) -> Continuation._assign(
                e,
                symbol,
                value,
                c );
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
        // Perform the assignment.
        p.assign( _symbol, evaluate( _value, p ) );
        // This is unspecified, so let's do it like PCS3.
        return Cons.NIL;
    }

    @Override
    public Thunk evaluate( Environment e, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        return thunk.apply( e, c );
    }
}
