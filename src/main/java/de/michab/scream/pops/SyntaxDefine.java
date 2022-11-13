package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Procedure;
import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.Symbol;
import de.michab.scream.Syntax;

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

    @Override
    public FirstClassObject activate( Environment parent, FirstClassObject[] args )
            throws RuntimeX
    {
        checkMinimumArgumentCount( 2, args );

        // Type check.
        if ( args[0] instanceof Symbol )
        {
            if ( args.length > 2 )
                throw new RuntimeX( Code.TOO_MANY_SUBEXPRESSIONS,
                        "define" );
            // Get the value.
            FirstClassObject value = evaluate( args[1], parent );
            // At last bind it.
            parent.define( (Symbol)args[0], value );
        }
        else if ( args[0] instanceof Cons && ((Cons)args[0]).length() > 0 )
        {
            FirstClassObject symbol = ((Cons)args[0]).getCar();
            if ( ! (symbol instanceof Symbol) )
                throw new RuntimeX( Code.DEFINE_ERROR );

            Procedure procToBind = new Procedure( parent,
                    ((Cons)args[0]).getCdr(),
                    Cons.create( args, 1 ) );
            procToBind.setName( (Symbol)symbol );
            parent.define( (Symbol)symbol, procToBind );
        }
        else
            throw new RuntimeX( Code.SYNTAX_ERROR );

        // This is unspecified.
        return Cons.NIL;
    }

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
