/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Lambda;
import de.michab.scream.RuntimeX;
import de.michab.scream.Syntax;
import de.michab.scream.pops.Continuation.Cont;
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

    private Lambda compImpl( Environment env, Cons cond, Cons positive )
            throws RuntimeX
    {
        var ccond =
                cond.getCar();
        var cpositive =
                positive.getCar();

        Lambda.L result = (e,c) -> {
            Cont<FirstClassObject> pos =
                    fco -> Continuation._x_eval( e, cpositive, c );
            Cont<FirstClassObject> neg =
                    falseObject -> c.accept( falseObject );

            return Continuation._if(
                    e,
                    ccond,
                    pos,
                    neg );
        };

        return new Lambda( result, getName() );
    }

    private Lambda compImpl( Environment env, Cons cond, Cons positive, Cons negative )
            throws RuntimeX
    {
        var ccond = cond.getCar();
        var cpositive = positive.getCar();
        var cnegative = negative.getCar();

        Lambda.L result = (e,c) -> {
            Cont<FirstClassObject> pos =
                    fco -> Continuation._x_eval( e, cpositive, c );
            Cont<FirstClassObject> neg =
                    fco -> Continuation._x_eval( e, cnegative, c );

            return Continuation._if(
                    e,
                    ccond,
                    pos,
                    neg );
        };

        return new Lambda( result, getName() );
    }

    @Override
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        long argsLen =
                checkArgumentCount( 2, 3, args );

        if ( argsLen == 2 )
        {
            return compImpl(
                    env,
                    args,
                    Scut.as( Cons.class, args.listTail( 1 ) ) );
        }
        else if ( argsLen == 3 )
        {
            return compImpl(
                    env,
                    args,
                    Scut.as( Cons.class, args.listTail( 1 ) ),
                    Scut.as( Cons.class, args.listTail( 2 ) ) );
        }

        throw new InternalError();
    }

    // Legacy ..
    @Override
    public FirstClassObject compile( Environment parent, Cons args )
            throws RuntimeX
    {
        return _compile( parent, args );
    }
    @Override
    public FirstClassObject activate( Environment parent,
            Cons arguments )
                    throws RuntimeX
    {
        var λ = _compile( parent, arguments );

        return FirstClassObject.evaluate( λ, parent );
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
