/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.RuntimeX;
import de.michab.scream.Scream.Cont;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.fcos.Syntax;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Scut;

/**
 * {@code
 * (do ((<variable₁> <init₁> <step₁>)
 *      ... )
 *     (<test> <expression> ... )
 *     command ... )} syntax
 * <p>
 * {@code r7rs 4.2.4 p18}
 */
public class SyntaxDo extends Syntax
{
    private SyntaxDo()
    {
        super( "do" );
    }

    /**
     *
     * @param variables {@code ((<var1> <init1> opt<step1>) ... )}
     * @return Two lists: The car-list contains the init-expressions.  The cdr-list
     * contains the step-expressions.
     * @throws RuntimeX
     */
    private Cons validateVariables( Cons variables ) throws RuntimeX
    {

        Cons inits = Cons.NIL;
        Cons steps = Cons.NIL;

        while ( variables != Cons.NIL )
        {
            var variableDef =
                    Scut.as( Cons.class, variables.getCar() );
            checkArgumentCount( 2, 3, variableDef );

            Symbol variable =
                    Scut.as( Symbol.class, variableDef.listRef( 0 ) );

            // Add a new init pair.
            inits = new Cons(
                    Cons.create(
                            variable,
                            variableDef.listRef( 1 )), inits );

            if ( variableDef.length() == 3 )
            {
                // Add a new step pair.
                steps = new Cons(
                        Cons.create(
                                variable,
                                variableDef.listRef( 2 )), steps );
            }

            variables = (Cons)variables.getCdr();
        }

        return new Cons( inits, steps );
    }

    @Override
    protected Thunk _executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 2, Integer.MAX_VALUE, args );

        var variables =
                Scut.as( Cons.class, args.listRef( 0 ) );
        var test =
                Scut.as( Cons.class, args.listRef( 1 ) );
        // May be nil.
        var commands =
                args.listTail( 2 );

        var setup =
                validateVariables( variables );

        checkArgumentCount( 1, Integer.MAX_VALUE, test );


        return Primitives._x_do(
                        e,
                        (Cons)setup.getCar(),
                        (Cons)setup.getCdr(),
                        test,
                        commands,
                        c );
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
        tle.setPrimitive( new SyntaxDo() );

        return tle;
    }
}

