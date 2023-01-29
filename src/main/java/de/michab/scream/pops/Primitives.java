/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2022-2023 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.RuntimeX;
import de.michab.scream.Scream.Cont;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.SchemeBoolean;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.util.Continuation.Thunk;

/**
 * Implementations of the interpreter primitives.
 *
 * public _x_... are externally visible primitives.
 * These always return an indirect thunk () -> ...
 *
 * @author micbinz
 */
public class Primitives
{
    /**
     * The if condition.
     *
     * @param e The environment used for evaluating the test.
     * @param test The test.  If the test evaluates to true, then the result
     * of the evaluation is passed to the true branch.  Otherwise
     * the false branch receives false.
     * @param trueBranch Taken if test evaluates to true.
     * @param falseBranch Taken if test evaluates to false.
     * @return A thunk.
     * @throws RuntimeX
     */
    public static Thunk _if(
            Environment e,
            FirstClassObject test,
            Cont<FirstClassObject> trueBranch,
            Cont<FirstClassObject> falseBranch ) throws RuntimeX
    {
        Cont<FirstClassObject> branch = testResult ->
        {
            return SchemeBoolean.isTrue( testResult ) ?
                    trueBranch.accept( testResult ) :
                    falseBranch.accept( testResult );
        };

        return FirstClassObject.evaluate( test, e, branch );
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

        return Primitives._x_eval(
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
    public static Thunk _x_and(
            Environment e,
            Cons expressions,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        return () -> _and(
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
    public static Thunk _x_assign(
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

        return () -> _x_eval( e, o, next );
    }

    /**
     * Define a new value.
     *
     * @param e The environment receiving the definition.
     * @param symbol The symbol to set.
     * @param value The value.  Not evaluated.
     * @param c A continuation receiving NIL.
     * @return A thunk.
     */
    public static Thunk _x_define(
            Environment e,
            Symbol symbol,
            FirstClassObject value,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        return () -> {
            e.define( symbol, value );
            return c.accept( Cons.NIL );
        };
    }

    /**
     * Create a list of new values.
     *
     * @param e The environment for evaluation.
     * @param s The symbol to set.
     * @param o The object to evaluate.
     * @param c A continuation receiving NIL.
     * @return The thunk.
     */
    private static Thunk _define(
            Environment e,
            Cons symbols,
            FirstClassObject o,
            Cont<Environment> c )
                    throws RuntimeX
    {
        if ( symbols == Cons.NIL )
            return c.accept( e );

        Cont<FirstClassObject> next = v -> {
            e.define( (Symbol)symbols.getCar(), v );
            return _define( e, (Cons)symbols.getCdr(), o, c );
        };

        return _x_eval( e, o, next );
    }

    /**
     * Evaluate a list of expressions and return the value of the final element.
     *
     * @param e The environment for evaluation.
     * @param body A list of expressions.
     * @param previousResult The result of the previous expression.
     * @param c The continuation receiving the result.
     * @return The thunk.
     * @throws RuntimeX
     */
    private static Thunk _begin(
            Environment e,
            Cons body,
            FirstClassObject previousResult,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        if ( body == Cons.NIL )
            return c.accept( previousResult );

        Cont<FirstClassObject> next =
                (fco) -> _begin( e, (Cons)body.getCdr(), fco, c);

        return Primitives._x_eval( e, body.getCar(), next );
    }

    /**
     * Evaluate a list of expressions and return the value of the final element.
     *
     * @param e The environment for evaluation.
     * @param body A list of expressions.
     * @param c The continuation receiving the result.
     * @return A thunk.
     * @throws RuntimeX
     */
    public static Thunk _x_begin(
            Environment e,
            Cons body,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        return () -> _begin(
                e,
                body,
                Cons.NIL,
                c );
    }

    /**
     * Performs variable binding for the {@code let...} syntax.
     *
     * @param e The environment used to evaluate the {@code <init>} expressions.
     * @param extended The environment receiving the bound values.
     * @param bindings The list of bindings to process.
     * Format is like {@code ((<variable1> <init1>) ...)}.
     * @param c A continuation that receives the extended environment.
     * @return A thunk.
     * @throws RuntimeX
     */
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
            extended.define( variable, fco );
            return _bindLet( e, extended, (Cons)bindings.getCdr(), c );
        };

        return _x_eval( e, init, evalResult );
    }

    public static Thunk _x_let(
            Environment e,
            Cons bindings,
            Cons body,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        Cont<Environment> begin =
                ext -> _x_begin(
                        ext,
                        body,
                        c );

        return () -> _bindLet(
                e,
                e.extend( Symbol.createObject( "x_let" ) ),
                bindings,
                begin );
    }

    public static Thunk _x_letStar(
            Environment e,
            Cons bindings,
            Cons body,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        Cont<Environment> begin =
                ext -> _x_begin(
                        ext,
                        body,
                        c );

        var extendedEnv = e.extend( Symbol.createObject( "x_let*" ) );

        return () -> _bindLet(
                extendedEnv,
                extendedEnv,
                bindings,
                begin );
    }

    /**
     *
     * @param e
     * @param bindings
     * @param body
     * @param symbols
     * @param c
     * @return
     * @throws RuntimeX
     */
    public static Thunk _x_letRec(
            Environment e,
            Cons bindings,
            Cons body,
            Cons symbols,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        Cont<Environment> begin =
                env -> _x_begin(
                        env,
                        body,
                        c );

        Cont<Environment> bind =
                env -> _bindLet(
                        env,
                        env,
                        bindings,
                        begin );

        return () -> _define(
                e.extend( Symbol.createObject( "x_letrec" ) ),
                symbols,
                Cons.NIL,
                bind );
    }

    /**
     * Evaluate an object.
     *
     * @param e The environment for evaluation.
     * @param o The object to evaluate.
     * @param c The continuation receiving the result.
     * @return The thunk.
     */
    public static Thunk _x_eval(
            Environment e,
            FirstClassObject o,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        return () -> FirstClassObject.evaluate( o, e, c );
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

        return Primitives._x_eval(
                e,
                expressions.getCar(),
                next );
    }

    /**
     * Shortcut or.
     *
     * @param e The environment for evaluation.
     * @param expressions The list of tests.
     * @param c The continuation.
     * @return A thunk.
     * @throws RuntimeX In case of an error.
     */
    public static Thunk _x_or(
            Environment e,
            Cons expressions,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        return () -> _or(
                e,
                expressions,
                SchemeBoolean.F,
                c );
    }

    public static Thunk _x_quote(
            Environment e,
            FirstClassObject quote,
            Cont<FirstClassObject> c) throws RuntimeX
    {
        return () -> c.accept( quote );
    }

    public static Thunk _x_resolve(
            Environment e,
            Symbol o,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        return () -> c.accept( e.get( o ) );
    }

    private static Thunk evalImpl(
            Environment e,
            Cons result,
            Cons current,
            Cont<Cons> c ) throws RuntimeX
    {
        if ( Cons.NIL == current )
            return c.accept( Cons.reverse( result ) );

        Cont<FirstClassObject> set = fco -> evalImpl(
                    e,
                    new Cons( fco, result ),
                    (Cons)current.getCdr(), c );

        return FirstClassObject.evaluate(
                current.getCar(),
                e,
                set );
    }

    /**
     * Evaluates the elements in the passed list.
     *
     * @param e The environment used for evaluation.
     * @param l The list to be evaluated.
     * @param c The continuation receiving a newly allocated list holding
     * the evaluated elements.
     * @return A thunk.
     */
    public static Thunk _x_evalCons( Environment e, Cons l, Cont<Cons> c )
    {
        return () -> evalImpl(
                e,
                Cons.NIL,
                l,
                c );
    }

    /**
     * The iteration step of a do syntax.
     *
     * @param e The environment used for evaluation.
     * @param test
     * @param steps
     * @param commands
     * @param c
     * @return A thunk.
     * @throws RuntimeX
     */
    private static Thunk _iteration(
            Environment e,
            Cons test,
            Cons steps,
            Cons commands,
            Cont<FirstClassObject> c
            ) throws RuntimeX
    {
        Cont<FirstClassObject> finish =
                trueValue -> _begin(
                        e,
                        (Cons)test.getCdr(),
                        trueValue,
                        c );

        Cont<Environment> restart =
                unused -> _iteration(
                        e,
                        test,
                        steps,
                        commands,
                        c );

        Cont<FirstClassObject> loopInit =
                unused -> _bindLet(
                        e,
                        e,
                        steps,
                        restart );

        // Process the command list.
        Cont<FirstClassObject> loopPerform =
                falseValue -> _begin(
                        e,
                        commands,
                        Cons.NIL,
                        loopInit );
        return _if(
                e,
                test.getCar(),
                finish,
                loopPerform );
    }

    public static Thunk _x_do(
            Environment e,
            Cons inits,
            Cons steps,
            Cons test,
            Cons commands,
            Cont<FirstClassObject> c )
    {
        Cont<Environment> iteration =
                ext -> _iteration(
                        ext,
                        test,
                        steps,
                        commands,
                        c );

        return () -> _bindLet(
                e,
                e.extend( Symbol.createObject( "x_do" ) ),
                inits,
                iteration );
    }

}
