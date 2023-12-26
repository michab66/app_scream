/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
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
 * {@code (if <test> <consequent> <alternate>)} syntax<br>
 * {@code (if <test> <consequent>)} syntax
 * <p>
 * {@code r7rs 4.1.5 p13}
 */
public class SyntaxIf extends Syntax
{
    private SyntaxIf()
    {
        super( "if" );
    }

    private Thunk compImpl(
            Environment e,
            Cons cond,
            Cons positive,
            Cont<FirstClassObject> c)
            throws RuntimeX
    {
        var ccond =
                cond.getCar();
        var cpositive =
                positive.getCar();

        return () -> {
            Cont<FirstClassObject> pos =
                    fco -> Primitives._x_eval( e, cpositive, c );
            Cont<FirstClassObject> neg =
                    falseObject -> c.accept( falseObject );

            return Primitives._if(
                    e,
                    ccond,
                    pos,
                    neg );
        };
    }

    private Thunk compImpl(
            Environment e,
            Cons cond,
            Cons positive,
            Cons negative,
            Cont<FirstClassObject> c)
            throws RuntimeX
    {
        var ccond = cond.getCar();
        var cpositive = positive.getCar();
        var cnegative = negative.getCar();

        return () -> {
            Cont<FirstClassObject> pos =
                    fco -> Primitives._x_eval( e, cpositive, c );
            Cont<FirstClassObject> neg =
                    fco -> Primitives._x_eval( e, cnegative, c );

            return Primitives._if(
                    e,
                    ccond,
                    pos,
                    neg );
        };
    }

    @Override
    protected Thunk __executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        long argsLen =
                checkArgumentCount( 2, 3, args );

        if ( argsLen == 2 )
        {
            return compImpl(
                    e,
                    args,
                    Scut.as( Cons.class, args.listTail( 1 ) ),
                    c );
        }

        if ( argsLen == 3 )
        {
            return compImpl(
                    e,
                    args,
                    Scut.as( Cons.class, args.listTail( 1 ) ),
                    Scut.as( Cons.class, args.listTail( 2 ) ),
                    c );
        }

        throw new InternalError();
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
        tle.setPrimitive( new SyntaxIf() );

        return tle;
    }
}
