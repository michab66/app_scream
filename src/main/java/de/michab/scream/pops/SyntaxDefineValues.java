/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.RuntimeX;
import de.michab.scream.Scream.Cont;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Syntax;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Scut;

/**
 * {@code (define-values <formals> <expression>)} ; syntax<br>
 * <p>
 * {@code r7rs 5.3.3 p26 }
 */
public class SyntaxDefineValues extends Syntax
{
    private SyntaxDefineValues()
    {
        super( "define-values" );
    }

    @Override
    protected Thunk _executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 2, Integer.MAX_VALUE, args );

        FirstClassObject.setConstant( args );

        // Get the first argument, a list of symbols.
        var formals = Scut.as(
                Cons.class,
                args.listRef( 0 ),
                s-> {
                    throw RuntimeX.mSyntaxError();
                } );

        // Get the second argument, an expression.
        var expression =
                args.listRef( 1 );

        // Receives the values from the evaluation of the
        // expression and sets the formals accordingly.
        Cont<FirstClassObject> defineValues = v -> {
            if ( ! FirstClassObject.is( Cons.class,v ) )
                v = new Cons( v, Cons.NIL );

            var receivedValueCount = Scut.as( Cons.class, v ).length();

            if ( receivedValueCount != formals.length() )
                throw RuntimeX.mWrongNumberOfArguments(
                        formals.length(),
                        receivedValueCount );

            return Primitives._x_defineList(
                    e,
                    formals,
                    Scut.as( Cons.class, v ),
                    // Constant result of the define-values procedure.
                    ignored -> c.accept( Cons.NIL ) );
        };

        // Evaluate the expression,
        // the resulting values are passed to
        // defineValues above.
        return Primitives._x_eval(
                e,
                expression,
                defineValues );
    }

    /**
     * Base operations setup.
     *
     * @param tle A reference to the environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendNullEnvironment( Environment tle )
            throws RuntimeX
    {
        tle.setPrimitive( new SyntaxDefineValues() );

        return tle;
    }
}
