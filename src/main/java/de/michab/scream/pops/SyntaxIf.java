/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Continuation.Cont;
import de.michab.scream.Continuation.Thunk;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.Syntax;
import de.michab.scream.util.Scut;

/**
 * r7rs 4.1.5
 *
 * <code>
 * (if <test> <consequent> <alternate>)<br>
 * (if <test> <consequent>)<br>
 * </code><br>
 * Syntax: <Test>, <consequent>, and <alternate> may be arbitrary
 * expressions.<br>
 * Semantics: An if expression is evaluated as follows:  First, <test> is
 * evaluated. If it yields a true value, then
 * <consequent> is evaluated and its value(s) is(are) returned. Otherwise
 * <alternate> is evaluated and its value(s) is(are) returned. If <test>
 * yields a false value and no <alternate> is specified, then the result of
 * the expression is unspecified.
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
    protected Thunk _executeImpl( Environment e, Cons args,
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
        else if ( argsLen == 3 )
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
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( new SyntaxIf() );

        return tle;
    }
}
