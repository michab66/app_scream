/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2022 Michael G. Binz
 */
package de.michab.scream.pops;

import java.util.function.Consumer;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.SchemeBoolean;
import de.michab.scream.ScreamException;
import de.michab.scream.Symbol;

public class Continuation
{
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

    /**
     * Shortcut and.
     *
     * @param e The environment for evaluation.
     * @param expressions The list of tests.
     * @param previousResult The value of the previous test.
     * @param c The continuation.
     * @return A thunk.
     * @throws RuntimeX In case of an error.
     */
    private static Thunk _and(
            Environment e,
            Cons expressions,
            FirstClassObject previousResult,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        if ( expressions == Cons.NIL )
            return () -> c.accept( previousResult );
        if ( ! SchemeBoolean.isTrue( previousResult ) )
            return () -> c.accept( previousResult );

        Cont<FirstClassObject> next =
                (fco) -> _and( e, (Cons)expressions.getCdr(), fco, c);

        return () -> Continuation._eval(
                e,
                expressions.getCar(),
                next );
    }

    /**
     * Shortcut and.
     *
     * @param e The environment for evaluation.
     * @param expressions The list of tests.
     * @param previousResult The value of the previous test.
     * @param c The continuation.
     * @return A thunk.
     * @throws RuntimeX In case of an error.
     */
    public static Thunk _and(
            Environment e,
            Cons expressions,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        return _and(
                e,
                expressions,
                SchemeBoolean.T,
                c );
    }

    /**
     * Assign an existing value.
     *
     * @param e The environment for evaluation.
     * @param s The symbol to set.
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
            e.assign( s, v );
            return c.accept( Cons.NIL );
        };

        return _eval( e, o, next );
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

        return () -> Continuation._eval( e, body.getCar(), next );
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

    private static Thunk _bindLet(
            Environment e,
            Environment extended,
            Cons bindings,
            Cont<Environment> c )
                    throws RuntimeX
    {
        if ( bindings == Cons.NIL )
            return c.accept( extended );

        Cons bindingElement = (Cons)bindings.getCar();
        Symbol variable = (Symbol)bindingElement.listRef(0);
        FirstClassObject init = bindingElement.listRef(1);

        Cont<FirstClassObject> evalResult = fco -> {
            extended.set( variable, fco );
            return _bindLet( e, extended, (Cons)bindings.getCdr(), c );
        };

        return _eval( e, init, evalResult );
    }

    private static Thunk _bindLetAsterisk(
            Environment extended,
            Cons bindings,
            Cont<Environment> c )
                    throws RuntimeX
    {
        if ( bindings == Cons.NIL )
            return c.accept( extended );

        Cons bindingElement = (Cons)bindings.getCar();
        Symbol variable = (Symbol)bindingElement.listRef(0);
        FirstClassObject init = bindingElement.listRef(1);

        Cont<FirstClassObject> evalResult = fco -> {
            extended.set( variable, fco );
            return _bindLetAsterisk( extended, (Cons)bindings.getCdr(), c );
        };

        return _eval( extended, init, evalResult );
    }

    public static Thunk _let(
            Environment e,
            Cons bindings,
            Cons body,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        Cont<Environment> next =
                ext -> _begin( ext, body, c );
        return _bindLet(
                e,
                e.extend(),
                bindings,
                next );
    }

    public static Thunk _letAsterisk(
            Environment e,
            Cons bindings,
            Cons body,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        Cont<Environment> next =
                ext -> _begin( ext, body, c );
        return _bindLetAsterisk(
                e.extend(),
                bindings,
                next );
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
                SchemeBoolean.isTrue( s ) ? trueBranch : falseBranch,
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
                SchemeBoolean.isTrue( s ) ? trueBranch : SchemeBoolean.F,
                c );

        return _eval( e, condition, next );
    }

    /**
     * Shortcut or.
     *
     * @param e The environment for evaluation.
     * @param expressions The list of tests.
     * @param previousResult The value of the previous test.
     * @param c The continuation.
     * @return A thunk.
     * @throws RuntimeX In case of an error.
     */
    private static Thunk _or(
            Environment e,
            Cons expressions,
            FirstClassObject previousResult,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        if ( expressions == Cons.NIL )
            return () -> c.accept( previousResult );
        if ( SchemeBoolean.isTrue( previousResult ) )
            return () -> c.accept( previousResult );

        Cont<FirstClassObject> next =
                (fco) -> _or( e, (Cons)expressions.getCdr(), fco, c);

        return () -> Continuation._eval(
                e,
                expressions.getCar(),
                next );
    }

    /**
     * Shortcut or.
     *
     * @param e The environment for evaluation.
     * @param expressions The list of tests.
     * @param previousResult The value of the previous test.
     * @param c The continuation.
     * @return A thunk.
     * @throws RuntimeX In case of an error.
     */
    public static Thunk _or(
            Environment e,
            Cons expressions,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        return _or(
                e,
                expressions,
                SchemeBoolean.F,
                c );
    }

    public static Thunk _quote(
            Environment e,
            FirstClassObject quote,
            Cont<FirstClassObject> c) throws RuntimeX
    {
        return c.accept( quote );
    }

    public static Thunk _resolve(
            Environment e,
            Symbol o,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        return c.accept( e.get( o ) );
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
