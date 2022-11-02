package de.michab.scream.pops;

import java.util.HashSet;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Lambda;
import de.michab.scream.Lambda.L;
import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.Symbol;
import de.michab.scream.Syntax;
import de.michab.scream.util.Scut;
import urschleim.Continuation;

/**
 * (case <key> <clause1> <clause2> ...) syntax; r7rs 15
 */
public class SyntaxCase extends Syntax
{
    static Symbol ELSE = Symbol.createObject( "else" );

    SyntaxCase()
    {
        super( "case" );
    }

    @Override
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        checkArgumentCount( 2, Integer.MAX_VALUE, args );

        var key = args.getCar();
        var clauses = Scut.as( Cons.class, args.getCdr() );

        var unifier = new HashSet<FirstClassObject>();

        // Validate clauses
        for ( var i = clauses ; i != Cons.NIL ; i = Scut.as( Cons.class, i.getCdr() ) )
        {
            final var fi = i;
            final var c = Scut.as(
                    Cons.class,
                    i.getCar(),
                    s -> { throw Scut.mBadClause( fi ); } );
            // datum exp1 ...
            checkArgumentCount( 2, Integer.MAX_VALUE, c );

            final var datum = c.getCar();

            if ( FirstClassObject.equal( ELSE, datum ) )
            {
                // must be last clause.
                if ( Cons.NIL != c.getCdr() )
                    throw new RuntimeX( Code.BAD_CLAUSE );
            }
            else
            {
                Cons cdatum = Scut.as(
                        Cons.class,
                        datum,
                        s -> { throw Scut.mBadClause( c ); });
                // datum must be at least a one element list.
                checkArgumentCount(
                        1,
                        Integer.MAX_VALUE,
                        cdatum );
                Scut.checkUnique(
                        unifier,
                        cdatum );
            }
        }

        L l = (e,c) -> Continuation._case(
                e,
                key,
                clauses,
                c);

        return new Lambda( l, getName() );
    }

    @Override
    public FirstClassObject activate( Environment parent,
            FirstClassObject[] args )
                    throws RuntimeX
    {
        checkMinimumArgumentCount( 2, args );

        // Evaluate the key.
        FirstClassObject key = evaluate( args[0], parent );

        // Now try to find the key in one of the clauses
        for ( int j = 1 ; j < args.length ; j++ )
        {
            if ( !( args[j] instanceof Cons ) )
                throw new RuntimeX( Code.BAD_CLAUSE,
                        toString( args[j] ) );

            FirstClassObject[] clause = ((Cons)args[j]).asArray();

            if ( clause.length < 2 )
                throw new RuntimeX( Code.BAD_CLAUSE,
                        toString( args[j] ) );

            // If this is the last clause and there is an 'else' clause...
            if ( j == args.length-1 && eqv( clause[0], ELSE ) )
                // ...make sure, that we are eqv to the key.
                clause[0] = new Cons( key, Cons.NIL );
            else if ( !( clause[0] instanceof Cons ) )
                throw new RuntimeX( Code.BAD_CLAUSE,
                        toString( args[j] ) );

            FirstClassObject[] clauseData = ((Cons)clause[0]).asArray();

            for ( int i = 0 ; i < clauseData.length ; i++ )
            {
                if ( eqv( key, clauseData[i] ) )
                    return interpretTailSequence( clause, 1, parent );
            }
        }

        // Unspecified according to the standard.
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
        tle.setPrimitive( new SyntaxCase() );

        return tle;
    }
}
