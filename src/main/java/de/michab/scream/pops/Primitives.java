/*
 * Scream @ https://github.com/urschleim/scream
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
import de.michab.scream.util.Scut;

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
     * Defines a list of symbols in the passed environment.
     * All symbols are defined to the same value.
     *
     * @param e The environment for evaluation.
     * @param symbols The symbols to define.
     * @param o The value to set.
     * @param c The resulting extended environment.
     * @return The thunk.
     */
    private static Thunk _define(
            Environment e,
            Cons symbols,
            FirstClassObject o,
            Cont<Environment> c )
                    throws RuntimeX
    {
        var circular = new Cons( Cons.NIL, Cons.NIL );
        circular.setCdr( circular );

        Cont<FirstClassObject> done = ignored -> {
            return c.accept( e );
        };

        return _defineList(
                e,
                symbols,
                circular,
                done );
    }

    /**
     * Defines a list of symbols in the passed environment and returns the
     * passed environment e.
     *
     * @param e The environment receiving the definitions.
     * @param symbols The symbols to define.
     * @param values The values to define.  If the list of values is shorter
     * than the list of symbols, then the symbols to the end of the list are
     * defined to {@code Cons.NIL}.
     *
     * @param c returns nil.
     * @return A thunk.
     * @throws RuntimeX
     */
    private static Thunk _defineList(
            Environment e,
            Cons symbols,
            Cons values,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        if ( symbols == Cons.NIL )
            return c.accept( e );

        var firstSymbol =
                Scut.as( Symbol.class, symbols.getCar() );
        var firstValue = values == Cons.NIL ?
                Cons.NIL:
                values.getCar();

        e.define( firstSymbol, firstValue );

        var restSymbols =
                Scut.as( Cons.class, symbols.getCdr() );
        var restValues =
                values == Cons.NIL ?
                        Cons.NIL :
                        Scut.as( Cons.class, values.getCdr() );

        return _defineList(
                e,
                restSymbols,
                restValues,
                c );
    }

    /**
     * Defines a list of symbols in the passed environment and returns the
     * passed list of symbols.
     *
     * @param e The environment receiving the definitions.
     * @param symbols The symbols to define.
     * @param values The values to define.  If the list of values is shorter
     * than the list of symbols, then the symbols to the end of the list are
     * defined to {@code Cons.NIL}.
     * @param c Returns 'symbols'.
     * @return A thunk.
     * @throws RuntimeX
     */
    public static Thunk _x_defineList(
            Environment e,
            Cons symbols,
            Cons values,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {

        return _defineList(
                e,
                symbols,
                values,
                ignored -> c.accept( symbols ) );
    }

    /**
     *
     * @param target The environment to extend with the definitions.
     * @param eval The environment used for the evaluation of the expression.
     * @param values The values to be bound.
     * @param expression The expression generating the values.
     * @param c Returns Cons.NIL.
     * @return A thunk.
     * @throws RuntimeX
     */
    private static Thunk _x_defineValues(
            Environment target,
            Environment eval,
            Cons values,
            FirstClassObject expression,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        // Receives the values from the evaluation of the
        // expression and sets the formals accordingly.
        Cont<FirstClassObject> defineValues = v -> {
            if ( ! FirstClassObject.is( Cons.class,v ) )
                v = new Cons( v, Cons.NIL );

            var receivedValueCount = Scut.as( Cons.class, v ).length();

            if ( receivedValueCount != values.length() )
                throw RuntimeX.mWrongNumberOfArguments(
                        values.length(),
                        receivedValueCount );

            return Primitives._x_defineList(
                    target,
                    values,
                    Scut.as( Cons.class, v ),
                    // Constant result of the define-values procedure.
                    ignored -> c.accept( Cons.NIL ) );
        };

        // Evaluate the expression,
        // the resulting values are passed to
        // defineValues above.
        return Primitives._x_eval(
                eval,
                expression,
                defineValues );
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
     * Last phase of do iteration binding.
     *
     * @param e The environment receiving the bindings.
     * @param toBind The bindings to perform.
     * @param c The continuation receiving finally the environment
     * that contains the rebound bindings.
     * @return A thunk.
     * @throws RuntimeX
     */
    private static Thunk _bindDoFinish(
            Environment e,
            Cons toBind,
            Cont<Environment> c )
                    throws RuntimeX
    {
        if ( Cons.NIL == toBind )
            return c.accept( e );

        Cons current =
                Scut.as( Cons.class, toBind.getCar() );
        Symbol variable =
                Scut.as( Symbol.class, current.getCar() );
        FirstClassObject value =
                current.getCdr();

        e.assign(
                variable,
                value );

        return _bindDoFinish(
                e,
                Scut.as( Cons.class, toBind.getCdr() ),
                c );
    }

    /**
     * Performs the iteration step bindings of a do expression.
     *
     * @param e The environment finally receiving the results.
     * @param bindings The list of bindings to process.
     * Format is like {@code ((<variable1> <init1>) ...)}.
     * @param temporaryBound A list of (symbol, value) pairs holding
     * the evaluation results.
     * @param c A continuation that receives the extended environment.
     * @return A thunk.
     * @throws RuntimeX
     */
    private static Thunk _bindDo(
            Environment e,
            Cons bindings,
            Cons temporaryBound,
            Cont<Environment> c )
                    throws RuntimeX
    {
        if ( bindings == Cons.NIL )
            return _bindDoFinish( e, temporaryBound, c );

        Cons current =
                Scut.as( Cons.class, bindings.getCar() );
        Symbol variable =
                Scut.as( Symbol.class, current.listRef(0) );
        FirstClassObject init =
                current.listRef(1);

        Cont<FirstClassObject> evalResult = fco -> {
            return _bindDo(
                    e,
                    (Cons)bindings.getCdr(),
                    new Cons( new Cons( variable, fco ), temporaryBound ),
                    c );
        };

        return _x_eval(
                e,
                init,
                evalResult );
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

    /*
     * (
     *   ((a b) (init1))
     *   ((c d) (init2))
     * )
     */
    private static Thunk _bindLetValues(
            Environment e,
            Environment extended,
            Cons bindings,
            Cont<Environment> c )
                    throws RuntimeX
    {
        if ( bindings == Cons.NIL )
            return c.accept( extended );

        Cons currentBinding =
                (Cons)bindings.getCar();
        Cons remainingBindings =
                (Cons)bindings.getCdr();

        Cons values =
                (Cons)currentBinding.listRef(0);
        FirstClassObject init =
                currentBinding.listRef(1);

        return _x_defineValues(
                extended,
                e,
                values,
                init,
                ignored -> _bindLetValues( e, extended, remainingBindings, c ) );

//        Cont<FirstClassObject> evalResult = fco -> {
//            extended.define( variable, fco );
//            return _bindLetValues( e, extended, remainingBindings, c );
//        };
//
//        return _x_eval( e, init, evalResult );
    }

    public static Thunk _x_let_values(
            Environment current,
            Environment extended,
            Cons bindings,
            Cons body,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        // Process the body with the extended environment.
        Cont<Environment> begin =
                ext -> _x_begin(
                        ext,
                        body,
                        c );

        return () -> _bindLetValues(
                current,
                extended,
                bindings,
                begin );
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
                    Scut.as( Cons.class, current.getCdr() ),
                    c );

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
                unused -> _bindDo(
                        e,
                        steps,
                        Cons.NIL,
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
