/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2022 Michael G. Binz
 */
package urschleim;

import java.util.function.Consumer;
import java.util.logging.Logger;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.SchemeBoolean;
import de.michab.scream.ScreamException;
import de.michab.scream.Symbol;

public class Continuation
{
    private static Logger LOG =
            Logger.getLogger( Continuation.class.getName() );


    public Continuation( Consumer<RuntimeX> errorHandler )
    {
        _errorHandler = errorHandler;
    }

    Consumer<RuntimeX> _errorHandler;

    @FunctionalInterface
    public static interface Cont<R> {
        Thunk accept(R result) throws RuntimeX;
    }

    @FunctionalInterface
    public static interface Thunk {
        Thunk run() throws RuntimeX;
    }

    public void trampoline(Thunk thunk)
    {
        try
        {
            while (thunk != null) {
                thunk = thunk.run();
            }
        }
        catch ( RuntimeX e )
        {
            _errorHandler.accept( e );
        }
    }

    public static void trampoline( Thunk t, Consumer<ScreamException> err )
    {
        try
        {
            while (t != null) {
                t = t.run();
            }
        }
        catch ( RuntimeX e )
        {
            err.accept( e );
        }
    }

    public static <T> Cont<T> endCall(Consumer<T> call) {
        return r -> {
            call.accept(r);
            return null;
        };
    }

    public static Thunk _quote(
            Environment e,
            FirstClassObject quote,
            Cont<FirstClassObject> c) throws RuntimeX
    {
        return c.accept( quote );
    }

    /**
     * Assing a value.
     *
     * @param e The environment for evaluation.
     * @param o The symbol to set.
     * @param o The object to evaluate.
     * @param c A continuation receiving NIL.
     * @return The thunk.
     */
    public static Thunk _assign(
            Environment e,
            Symbol s,
            FirstClassObject o,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        Cont<FirstClassObject> next = v -> {
            e.set( s, v );
            return c.accept( Cons.NIL );
        };

        return _eval( e, o, next );
    }

    /**
     * Evaluate an object.
     *
     * @param e The environment for evaluation.
     * @param o The object to evaluate.
     * @param c The continuation receiving the result.
     * @return The thunk.
     */
    public static Thunk _eval(
            Environment e,
            FirstClassObject o,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        return c.accept(
                    FirstClassObject.evaluate( o, e ) );
    }

    public static Thunk _resolve(
            Environment e,
            Symbol o,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        return c.accept( e.get( o ) );
    }

    /**
     * Evaluate a list of expressions and return the value of the final element.
     * @param e The environment for evaluation.
     * @param body A list of expressions.
     * @param previousResult The result of the previous expression.
     * @param c The continuation receiving the result.
     * @return The thunk.
     */
    private static Thunk _begin(
            Environment e,
            Cons body,
            FirstClassObject previousResult,
            Cont<FirstClassObject> c )
    {
        if ( body == Cons.NIL )
            return () -> c.accept( previousResult );

        Cont<FirstClassObject> next =
                (fco) -> _begin( e, (Cons)body.getCdr(), fco, c);

        return () -> _eval( e, body.getCar(), next );
    }

    /**
     * Evaluate a list of expressions and return the value of the final element.
     * @param e The environment for evaluation.
     * @param body A list of expressions.
     * @param previousResult The result of the previous expression.
     * @param c The continuation receiving the result.
     * @return The thunk.
     */
    public static Thunk _begin(
            Environment e,
            Cons body,
            Cont<FirstClassObject> c )
    {
        return _begin(
                e,
                body,
                Cons.NIL,
                c );
    }

    public static Thunk _if(
            Environment e,
            FirstClassObject condition,
            FirstClassObject trueBranch,
            FirstClassObject falseBranch,
            Cont<FirstClassObject> c)
                    throws RuntimeX
    {
        Cont<FirstClassObject> next = s -> _eval(
                e,
                s == SchemeBoolean.F ? falseBranch : trueBranch,
                c );

        return _eval( e, condition, next );
    }
    /**
     * r7rs - 4.1.5 -- no false branch.
     * Unspecified result if no 'else' -> #F
     *
     * @param e
     * @param condition
     * @param trueBranch
     * @param c
     * @return
     */
    public static Thunk _if(
            Environment e,
            FirstClassObject condition,
            FirstClassObject trueBranch,
            Cont<FirstClassObject> c)
                    throws RuntimeX
    {
        Cont<FirstClassObject> next = s -> _eval(
                e,
                s == SchemeBoolean.F ? SchemeBoolean.F : trueBranch,
                c );

        return _eval( e, condition, next );
    }

    public static Thunk _clause(
            Environment e,
            Cons clause,
            Cont<FirstClassObject> trueBranch,
            Thunk falseBranch)
                    throws RuntimeX
    {
        Cont<FirstClassObject> next = s -> {
            if (  s == SchemeBoolean.F )
                return falseBranch;

            Cons afterTest = (Cons)clause.getCdr();

            // rsr7: If the selected <clause> contains only the <test> and no
            // <expression>s, then the value of the <test> is returned as
            // the result.
            if ( Cons.NIL == afterTest )
                return trueBranch.accept( s );

            return _begin( e, (Cons)clause.getCdr(), trueBranch );
        };

        return _eval(
                e,
                clause.getCar(),
                next );
    }

    public static Thunk _cond(
            Environment e,
            Cons clauses,
            Cont<FirstClassObject> c) throws RuntimeX
    {
        if ( Cons.NIL == clauses )
            return c.accept( Cons.NIL );

        Thunk next = () -> _cond(
                e,
                (Cons)clauses.getCdr(),
                c );

        return _clause(
                e,
                (Cons)clauses.getCar(),
                c,
                next );
    }


    /**
     *
     * @param e
     * @param key the evaluted key.
     * @param clauses The remaining clauses.  May be nil.
     * @param datums The datumlist of the current clause. nil if no clauses remain.
     * @param body the expressions of the current clause.
     * @param c
     * @return
     * @throws RuntimeX
     */
    private static Thunk _caseImpl(
            Environment e,
            FirstClassObject key,
            Cons clauses,
            Cont<FirstClassObject> c) throws RuntimeX
    {
        if ( Cons.NIL == clauses )
            return c.accept( Cons.NIL );

        var currentClause = FirstClassObject.as( Cons.class, clauses.getCar() );

        if ( Symbol.createObject( "else" ).equals( currentClause.getCar() ))
        {
            return _begin(
                    e,
                    FirstClassObject.as( Cons.class, currentClause.getCdr() ),
                    c );
        }
        var datums = FirstClassObject.as( Cons.class, currentClause.getCar() );
        if ( datums.member( key ) != SchemeBoolean.F )
        {
            return _begin(
                    e,
                    FirstClassObject.as( Cons.class, currentClause.getCdr() ),
                    c );
        }

        return _caseImpl(
                e,
                key,
                (Cons)clauses.getCdr(),
                c );
    }

    public static Thunk _case(
            Environment e,
            FirstClassObject key,
            Cons clauses,
            Cont<FirstClassObject> c) throws RuntimeX
    {
        if ( Cons.NIL == clauses )
            return c.accept( Cons.NIL );

        Cont<FirstClassObject> next = (fco) -> _caseImpl(
                e,
                fco,
                clauses,
                c );

        return _eval(
                e,
                key,
                next );
    }

    private static Thunk listEval(
            Environment e,
            int i,
            FirstClassObject[] l,
            Cont<FirstClassObject[]> c )
    {
        if ( i == l.length )
            return () -> c.accept( l );

        return () -> {
            if ( l[i]  != Cons.NIL )
                l[i] = FirstClassObject.evaluate( l[i], e );
            return listEval( e, i+1, l, c );
        };
    }

    public static Thunk listEval(
            Environment e,
            FirstClassObject[] l,
            Cont<FirstClassObject[]> c )
    {
        return () -> listEval( e, 0, l, c );
    }

    /**
     * Evaluates the elements in the passed list.
     *
     * @param e The environment used for evaluation.
     * @param l The list to be evaluated.
     * @param c The continuation receiving a newly allocated list holding
     * the evaluated elements.
     * @return The thunk.
     */
    public static Thunk listEval( Environment e, Cons l, Cont<Cons> c )
    {
        Cont<FirstClassObject[]> next =
                s -> c.accept( Cons.create( s ) );

        return () -> listEval(
                e,
                0,
                Cons.asArray( l ),
                next );
    }
}
