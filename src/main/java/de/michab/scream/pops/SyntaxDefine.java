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
import de.michab.scream.Procedure;
import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.Symbol;
import de.michab.scream.Syntax;
import de.michab.scream.util.Scut;

/**
 * (define <variable> <expression>) syntax; r5rs 16
 * (define (<variable> <formals>) <body>) syntax; r5rs 16
 * (define (<variable> . <formal>) <body>) syntax; r5rs 16
 */
public class SyntaxDefine extends Syntax
{
    private SyntaxDefine()
    {
        super( "define" );
    }

    private Lambda compileVariableDefinition(
            Symbol variable, Cons value ) throws RuntimeX
    {
        checkArgumentCount( 1, value );

        Lambda.L result = (e,c) -> {

            return Continuation._x_eval(
                    e,
                    value.getCar(),
                    fco -> Continuation._x_define(
                            e,
                            variable,
                            fco,
                            c ) );
            };

        return new Lambda(
                result,
                this.toString() ).setInfo( value );
    }

    @Override
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        checkArgumentCount( 2, Integer.MAX_VALUE, args );

        var variableSlot = args.getCar();

        var rest = Scut.as(
                Cons.class,
                args.getCdr(),
                s-> {
                    throw new RuntimeX( Code.SYNTAX_ERROR );
                } );

        if ( variableSlot instanceof Symbol )
            return compileVariableDefinition( (Symbol)variableSlot, rest );

        // This is a lambda define...

        Cons signature = Scut.as(
                Cons.class,
                variableSlot,
                s-> {
                    throw new RuntimeX( Code.SYNTAX_ERROR );
                } );
        Symbol name = Scut.as(
                Symbol.class,
                signature.listRef( 0 ),
                s -> {
                    throw new RuntimeX( Code.DEFINE_ERROR );
                } );
        FirstClassObject parameterList =
                signature.getCdr();
        Cons body = Scut.as(
                Cons.class,
                args.listTail( 1 ) );

        Lambda.L result = (e,c) -> {
            var value = new Procedure(
                    e,
                    parameterList,
                    body ).setName( name );
            return Continuation._x_define( e, name, value, c );
        };

        return new Lambda(
                result,
                this.toString() ).setInfo( args );
    }

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

//    @Override
//    public FirstClassObject activate( Environment parent, FirstClassObject[] args )
//            throws RuntimeX
//    {
//        checkMinimumArgumentCount( 2, args );
//
//        // Type check.
//        if ( args[0] instanceof Symbol )
//        {
//            if ( args.length > 2 )
//                throw new RuntimeX( Code.TOO_MANY_SUBEXPRESSIONS,
//                        "define" );
//            // Get the value.
//            FirstClassObject value = evaluate( args[1], parent );
//            // At last bind it.
//            parent.define( (Symbol)args[0], value );
//        }
//        else if ( args[0] instanceof Cons && ((Cons)args[0]).length() > 0 )
//        {
//            FirstClassObject symbol = ((Cons)args[0]).getCar();
//            if ( ! (symbol instanceof Symbol) )
//                throw new RuntimeX( Code.DEFINE_ERROR );
//
//            Procedure procToBind = new Procedure( parent,
//                    ((Cons)args[0]).getCdr(),
//                    Cons.create( args, 1 ) );
//            procToBind.setName( (Symbol)symbol );
//            parent.define( (Symbol)symbol, procToBind );
//        }
//        else
//            throw new RuntimeX( Code.SYNTAX_ERROR );
//
//        // This is unspecified.
//        return Cons.NIL;
//    }
//
    /**
     * Base operations setup.
     *
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( new SyntaxDefine() );

        return tle;
    }
}
