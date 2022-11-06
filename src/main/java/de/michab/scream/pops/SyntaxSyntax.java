package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.Symbol;
import de.michab.scream.Syntax;

/**
 * (%syntax exp)
 *
 * Similar to 'define'.  Generates macros whose body gets called
 * with unevaluated arguments.
 */
public class SyntaxSyntax extends Syntax
{
    private SyntaxSyntax()
    {
        super( "%syntax" );
    }

    @Override
    public FirstClassObject activate( Environment parent, FirstClassObject[] args )
            throws RuntimeX
    {
        checkMinimumArgumentCount( 2, args );

        // Type check.
        if ( args[0] instanceof Cons && ((Cons)args[0]).length() > 0 )
        {
            FirstClassObject symbol = ((Cons)args[0]).getCar();
            if ( ! (symbol instanceof Symbol) )
                throw new RuntimeX( Code.DEFINE_ERROR );

            Syntax procToBind = new Syntax( parent,
                    ((Cons)args[0]).getCdr(),
                    Cons.create( args, 1 ) );
            procToBind.setName( (Symbol)symbol );
            parent.set( (Symbol)symbol, procToBind );
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
        tle.setPrimitive( new SyntaxSyntax() );

        return tle;
    }
}
