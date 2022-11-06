package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Lambda;
import de.michab.scream.Operation;
import de.michab.scream.RuntimeX;
import de.michab.scream.util.Scut;
import urschleim.Continuation;

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
public class SyntaxIf extends Operation
{
    private SyntaxIf()
    {
        super( "if" );
    }

    private Lambda compImpl( Environment env, Cons cond, Cons positive )
            throws RuntimeX
    {
        var ccond = FirstClassObject.compile( cond.getCar(), env );
        cond.setCar( ccond );
        var cpositive = FirstClassObject.compile( positive.getCar(), env );
        positive.setCar( cpositive );

        Lambda.L result = (e,c) -> {
            return Continuation._if(
                    e,
                    ccond,
                    cpositive,
                    c );
        };

        return new Lambda( result, getName() );
    }
    private Lambda compImpl( Environment env, Cons cond, Cons positive, Cons negative )
            throws RuntimeX
    {
        var ccond = FirstClassObject.compile( cond.getCar(), env );
        cond.setCar( ccond );
        var cpositive = FirstClassObject.compile( positive.getCar(), env );
        positive.setCar( cpositive );
        var cnegative = FirstClassObject.compile( negative.getCar(), env );
        negative.setCar( cnegative );

        Lambda.L result = (e,c) -> {
            return Continuation._if(
                    e,
                    ccond,
                    cpositive,
                    cnegative,
                    c );
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
    public FirstClassObject compile( Environment parent, FirstClassObject[] args )
            throws RuntimeX
    {
        checkMinimumArgumentCount( 2, args );
        checkMaximumArgumentCount( 3, args );

        // Compile referenced nodes.
        for ( int i = 0 ; i < args.length ; i++ )
            args[i] = compile( args[i], parent );

        FirstClassObject condition = args[0];
        FirstClassObject onTrue = args[1];
        // Handle optional 'else' branch.
        FirstClassObject onFalse = args.length == 3 ? args[2] : Cons.NIL;

        // Optimisation of constant sub expressions.  If this is sth like
        // (if #t ...) no 'if' node is needed at all.
        //      if ( isConstant( condition ) )
        //      {
        //        System.err.println( "removed 'if'" );
        //
        //        if ( condition != SchemeBoolean.F )
        //          return onTrue;
        //        else
        //          return onFalse;
        //      }

        // Now create the compiled node.
        return new If( condition, onTrue, onFalse );
    }
    @Override
    protected FirstClassObject activate( Environment parent,
            FirstClassObject[] arguments )
                    throws RuntimeX
    {
        return evaluate( compile( parent, arguments ), parent );
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
