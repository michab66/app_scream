package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Lambda;
import de.michab.scream.Lambda.L;
import de.michab.scream.RuntimeX;
import de.michab.scream.SchemeBoolean;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.Symbol;
import de.michab.scream.Syntax;
import de.michab.scream.util.Scut;
import urschleim.Continuation;

public class SyntaxCond extends Syntax
{
    static Symbol ELSE = Symbol.createObject( "else" );

    private SyntaxCond()
    {
        super( "cond" );
    }

    @Override
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        checkArgumentCount( 1, Integer.MAX_VALUE, args );

        for ( Cons c = args ; c != Cons.NIL ; c = Scut.as( Cons.class, c.getCdr() ) )
        {
            var fco = c.getCar();
            if ( ! (fco instanceof Cons) )
                throw new RuntimeX( Code.BAD_CLAUSE,
                        toString( fco ) );
            Cons clause = Scut.as( Cons.class, fco);

            // TODO unexpected ELSE message.
            if ( eqv( ELSE, clause.getCar() ) )
            {
                if ( Cons.NIL != c.getCdr() )
                    throw new RuntimeX( Code.BAD_CLAUSE,
                            toString( fco ) );
                clause.setCar( SchemeBoolean.T );
            }
        }

        L l = (e,c) -> Continuation._cond(
                e,
                args,
                c);

        return new Lambda( l, getName() );
    }

    @Override
    public FirstClassObject compile( Environment parent, FirstClassObject[] args )
            throws RuntimeX
    {
        checkMinimumArgumentCount( 1, args );

        Cons[] clausesTmp = new Cons[ args.length ];
        // Check if all the clauses are actually lists.  The do-while loop is
        // used to keep 'i' in a local scope.  'i' in turn is needed in the try
        // and catch scope to create a meaningful error message.

        {
            int i = 0;
            try
            {
                for ( i = 0 ; i < args.length ; i++ )
                {
                    clausesTmp[i] = (Cons)args[i];
                    if ( Cons.NIL == clausesTmp[i] )
                        throw new ClassCastException();
                }
            }
            catch ( ClassCastException e )
            {
                throw new RuntimeX( Code.BAD_CLAUSE,
                        toString( args[i] ) );
            }
        }

        // Everything is fine so far.  Convert the lists into arrays that can be
        // handled much more efficiently.
        FirstClassObject[][] clauses = new FirstClassObject[ args.length ][];
        for ( int i = 0 ; i < clauses.length ; i++ )
            clauses[i] = clausesTmp[i].asArray();

        if ( eqv( ELSE, clauses[ args.length-1 ][0] ) )
            clauses[ args.length-1 ][0] = SchemeBoolean.T;

        // Finally compile all the subexpressions.
        for ( int i = 0 ; i < clauses.length ; i++ )
            for ( int j = 0 ; j < clauses[i].length ; j++ )
                clauses[i][j] = compile( clauses[i][j], parent );

        // TODO Remove static subexpressions.

        return new Cond( clauses );
    }

    /**
     * Base operations setup.
     *
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( new SyntaxCond() );

        return tle;
    }
};

