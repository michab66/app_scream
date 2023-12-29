/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2022-2023 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.RuntimeX;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.fcos.SchemeBoolean;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.util.Continuation.Cont;
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
     * Casts a FirstClassObject to a certain class and passes the casted object
     * to its continuation.
     *
     * @param <T> The target type of the cast.
     * @param cl The target type's class object.
     * @param fco The object to cast.
     * @param c The continuation receiving the cast object.
     * @return The thunk.
     */
    static public <T extends FirstClassObject> Thunk _cast(
            Class<T> cl,
            FirstClassObject fco,
            Cont<T> c )
    {
        return () -> c.accept( Scut.as( cl, fco ) );
    }

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
     */
    public static Thunk _if(
            Environment e,
            FirstClassObject test,
            Cont<FirstClassObject> trueBranch,
            Cont<FirstClassObject> falseBranch )
    {
        return () -> {
            Cont<FirstClassObject> branch = testResult ->
            {
                return SchemeBoolean.isTrue( testResult ) ?
                        trueBranch.accept( testResult ) :
                        falseBranch.accept( testResult );
            };

            return FirstClassObject.evaluate( test, e, branch );
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
     */
    private static Thunk _and(
            Environment e,
            Cons expressions,
            FirstClassObject previousResult,
            Cont<FirstClassObject> c )
    {
        if ( expressions == Cons.NIL )
            return () -> c.accept( previousResult );
        if ( ! SchemeBoolean.isTrue( previousResult ) )
            return () -> c.accept( previousResult );

        Cont<FirstClassObject> next =
                fco -> _and(
                        e,
                        (Cons)expressions.getCdr(),
                        fco,
                        c);

        return Primitives._eval(
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
     */
    public static Thunk _and(
            Environment e,
            Cons expressions,
            Cont<FirstClassObject> c )
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
     * @param o The value to assign.  This is not evaluated, but assigned as-is.
     * @param c A continuation receiving the assigned value.
     * @return A thunk.
     * @see #_define(Environment, Symbol, FirstClassObject, Cont)
     */
    public static Thunk _assign(
            Environment e,
            Symbol symbol,
            FirstClassObject value,
            Cont<FirstClassObject> c )
    {
        return () -> {
            e.assign( symbol, value );
            return c.accept( value );
        };
    }

    /**
     * Define a new value.
     *
     * @param e The environment receiving the definition.
     * @param symbol The symbol to set.
     * @param value The value.
     * @param c A continuation receiving the value.
     * @return A thunk.
     */
    public static Thunk _define(
            Environment e,
            Symbol symbol,
            FirstClassObject value,
            Cont<FirstClassObject> c )
    {
        return () -> {
            e.define( symbol, value );
            return c.accept( value );
        };
    }

    /**
     * Defines a list of symbols in the passed environment.
     * All symbols are bound to the same value.
     *
     * @param e The environment for evaluation.
     * @param symbols The symbols to define.
     * @param o The value to set.
     * @param c The resulting extended environment.
     * @return The thunk.
     */
    private static Thunk _defineSymbols(
            Environment e,
            Cons symbols,
            FirstClassObject o,
            Cont<Environment> c )
    {
        return () -> {
            var circular = new Cons( Cons.NIL, Cons.NIL );
            circular.setCdr( circular );

            return _defineList(
                    e,
                    symbols,
                    circular,
                    c );
        };
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
     * @param c returns the passed environment.
     * @return A thunk.
     */
    static Thunk _defineList(
            Environment e,
            Cons symbols,
            Cons values,
            Cont<Environment> c )
    {
        return () -> {
            if ( symbols == Cons.NIL )
                return c.accept( e );

            var firstSymbol =
                    Scut.as( Symbol.class, symbols.getCar() );
            var firstValue = values == Cons.NIL ?
                    Cons.NIL:
                    values.getCar();
            var restSymbols =
                    Scut.as( Cons.class, symbols.getCdr() );
            var restValues =
                    values == Cons.NIL ?
                            Cons.NIL :
                            Scut.as( Cons.class, values.getCdr() );

            return _define(
                    e,
                    firstSymbol,
                    firstValue,
                    ignored -> _defineList(
                            e,
                            restSymbols,
                            restValues,
                            c ) );
        };
    }

    /**
     * Define the values for a multi-value operation.
     *
     * @param target The environment to extend with the definitions.
     * @param symbols The symbols to be bound.
     * @param evalResult The evaluated values.  If this is a single fco, it is
     * placed in a single element list.  If this is already a list it is not changed.
     * The list length must correspond to the length of {@code symbols}.
     * @param c Returns Cons.NIL.
     * @return A thunk.
     */
    static public Thunk _defineValues(
            Environment target,
            Cons symbols,
            FirstClassObject evalResult,
            Cont<FirstClassObject> c)
    {
        return () -> {
            // Make a list from the evaluation result.
            Cons v_ = ! FirstClassObject.is( Cons.class, evalResult ) ?
                // We received only a single value, transform to a
                // single-element list.
                new Cons( evalResult ) :
                // Was already a list.
                Scut.as( Cons.class, evalResult );

            var receivedValueCount = v_.length();

            if ( receivedValueCount != symbols.length() )
                throw RuntimeX.mWrongNumberOfArguments(
                        symbols.length(),
                        receivedValueCount );

            return Primitives._defineList(
                    target,
                    symbols,
                    v_,
                    // Constant result of the define-values procedure.
                    ignored -> c.accept( Cons.NIL ) );
        };
    }

    /**
     * Evaluate a list of expressions and return the value of the final element.
     *
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
        return () -> {
            if ( body == Cons.NIL )
                return c.accept( previousResult );

            Cont<FirstClassObject> next =
                    (fco) -> _begin( e, (Cons)body.getCdr(), fco, c);

            return Primitives._eval( e, body.getCar(), next );
        };
    }

    /**
     * Evaluate a list of expressions and return the value of the final element.
     *
     * @param e The environment for evaluation.
     * @param body A list of expressions.
     * @param c The continuation receiving the result.
     * @return A thunk.
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

    /**
     * Last phase of do iteration binding.
     *
     * @param e The environment receiving the bindings.
     * @param toBind The bindings to perform.
     * @param c The continuation receiving finally the environment
     * that contains the rebound bindings.
     * @return A thunk.
     */
    private static Thunk _bindDoFinish(
            Environment e,
            Cons toBind,
            Cont<Environment> c )
    {
        return () -> {
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
        };
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
     */
    private static Thunk _bindDo(
            Environment e,
            Cons bindings,
            Cons temporaryBound,
            Cont<Environment> c )
    {
        return () -> {
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

            return _eval(
                    e,
                    init,
                    evalResult );
        };
    }

    /**
     * Performs variable binding for the {@code let...} syntax.
     *
     * @param eval The environment used to evaluate the {@code <init>} expressions.
     * @param extended The environment receiving the bound values.
     * @param bindings The list of bindings to process.
     * Format is like {@code ((<variable1> <init1>) ...)}.
     * @param c A continuation that receives the extended environment.
     * @return A thunk.
     */
    private static Thunk _bind(
            Environment eval,
            Environment extended,
            Cons bindings,
            Cont<Environment> c )
    {
        return () -> {
            if ( bindings == Cons.NIL )
                return c.accept( extended );

            Cons bindingElement = (Cons)bindings.getCar();
            Symbol variable = (Symbol)bindingElement.listRef(0);
            FirstClassObject init = bindingElement.listRef(1);

            Cons next = Scut.as( Cons.class, bindings.getCdr() );

            return _eval(
                    eval,
                    init,
                    result -> _define(
                            extended,
                            variable,
                            result,
                            value -> _bind(
                                    eval,
                                    extended,
                                    next,
                                    c ) ) );
        };
    }

    /*
     * (
     *   ((a b) (init1))
     *   ((c d) (init2))
     * )
     */
    public static Thunk _x_bindValues(
            Environment e,
            Environment extended,
            Cons bindings,
            Cont<Environment> c )
    {
        return () -> {
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

            return Primitives._eval(
                    e,
                    init,
                    result -> _defineValues(
                            extended,
                            values,
                            result,
                            ignored -> _x_bindValues(
                                    e,
                                    extended,
                                    remainingBindings,
                                    c ) ) );
        };
    }

    public static Thunk _x_let_values(
            Environment current,
            Environment extended,
            Cons bindings,
            Cons body,
            Cont<FirstClassObject> c )
    {
        // Process the body with the extended environment.
        Cont<Environment> begin =
                ext -> _begin(
                        ext,
                        body,
                        c );

        return () -> _x_bindValues(
                current,
                extended,
                bindings,
                begin );
    }

    /**
     * Executes a let expression.
     *
     * @param e The environment used for evaluation of the expressions.
     * @param extended The environment receiving the new bindings.
     * @param bindings The bindings.
     * @param body The body executed after the binding phase.
     * @param c Returns the result of body evaluation.
     * @return A think.
     */
    public static Thunk _x_let(
            Environment e,
            Environment extended,
            Cons bindings,
            Cons body,
            Cont<FirstClassObject> c )
    {
        Cont<Environment> begin =
                ext -> _begin(
                        ext,
                        body,
                        c );

        return () -> _bind(
                e,
                extended,
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
     */
    public static Thunk _x_letRec(
            Environment e,
            Cons bindings,
            Cons body,
            Cons symbols,
            Cont<FirstClassObject> c )
    {
        Cont<Environment> begin =
                env -> _begin(
                        env,
                        body,
                        c );

        Cont<Environment> bind =
                env -> _bind(
                        env,
                        env,
                        bindings,
                        begin );

        return () -> _defineSymbols(
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
    public static Thunk _eval(
            Environment e,
            FirstClassObject o,
            Cont<FirstClassObject> c )
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
     */
    private static Thunk _or(
            Environment e,
            Cons expressions,
            FirstClassObject previousResult,
            Cont<FirstClassObject> c )
    {
        if ( expressions == Cons.NIL )
            return () -> c.accept( previousResult );
        if ( SchemeBoolean.isTrue( previousResult ) )
            return () -> c.accept( previousResult );

        Cont<FirstClassObject> next =
                (fco) -> _or( e, (Cons)expressions.getCdr(), fco, c);

        return Primitives._eval(
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
     */
    public static Thunk _x_or(
            Environment e,
            Cons expressions,
            Cont<FirstClassObject> c )
    {
        return _or(
                e,
                expressions,
                SchemeBoolean.F,
                c );
    }

    public static Thunk _quote(
            FirstClassObject quote,
            Cont<FirstClassObject> c)
    {
        return () -> c.accept( quote );
    }

    public static Thunk _x_resolve(
            Environment e,
            Symbol o,
            Cont<FirstClassObject> c )
    {
        return () -> c.accept( e.get( o ) );
    }

    private static Thunk evalImpl(
            Environment e,
            Cons result,
            Cons current,
            Cont<Cons> c )
    {
        return () -> {
            if ( Cons.NIL == current )
                return c.accept( Cons.reverse( result ) );

            Cons next =
                    Scut.as( Cons.class, current.getCdr() );

            Cont<FirstClassObject> set = fco -> evalImpl(
                    e,
                    new Cons( fco, result ),
                    next,
                    c );

            return FirstClassObject.evaluate(
                    current.getCar(),
                    e,
                    set );
        };
    }

    /**
     * Evaluates the elements in the passed list and returns a list with the
     * evaluation results to the passed continuation.
     *
     * @param e The environment used for evaluation.
     * @param l The list to be evaluated.
     * @param c The continuation receiving a newly allocated list holding
     * the evaluated elements.
     * @return A thunk.
     */
    public static Thunk _x_evalCons( Environment e, Cons l, Cont<Cons> c )
    {
        return evalImpl(
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
     */
    private static Thunk _iteration(
            Environment e,
            Cons test,
            Cons steps,
            Cons commands,
            Cont<FirstClassObject> c
            )
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

        return () -> _bind(
                e,
                e.extend( Symbol.createObject( "x_do" ) ),
                inits,
                iteration );
    }

    private static Thunk _mapImpl(
            Environment env,
            Procedure procedure,
            Cons results,
            Cons todo,
            Cont<FirstClassObject> result )
    {
        return () -> {
            if ( Cons.NIL == todo )
                return result.accept( Cons.reverse( results ) );

            Cons next = Scut.as( Cons.class, todo.getCdr() );

            Cont<FirstClassObject> step = r -> {
                return _mapImpl(
                        env,
                        procedure,
                        new Cons( r, results ),
                        next,
                        result );
            };

            return procedure.apply(
                    env,
                    new Cons( todo.getCar() ),
                    step );
            };
    }

    private static Thunk _map(
            Environment env,
            FirstClassObject procedure,
            Cons list,
            Cont<FirstClassObject> result )
    {
        return () -> _mapImpl(
                env,
                Scut.as( Procedure.class, procedure ),
                Cons.NIL,
                list,
                result );
    }

    /**
     * Maps a single-argument-procedure to each element in the passed list and
     * returns the results for each call in its result list.
     *
     * @param e The environment used for evaluation.
     * @param procedure A symbol that must evaluate to a procedure.
     * @param list A list of argument lists for procedure.
     * @param c Returns a list of the results.
     * @return A thunk.
     */
    public static Thunk _x_map(
            Environment e,
            Symbol procedure,
            Cons list,
            Cont<FirstClassObject> c )
    {
        return _eval(
                e,
                procedure,
                proc -> _map( e, proc, list, c ) );
    }
}
