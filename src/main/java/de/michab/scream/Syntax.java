/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

package de.michab.scream;

import de.michab.scream.Lambda.L;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.pops.Assignment;
import de.michab.scream.pops.Cond;
import de.michab.scream.pops.If;
import de.michab.scream.pops.Let;
import de.michab.scream.pops.LetAsterisk;
import de.michab.scream.pops.Letrec;
import de.michab.scream.pops.Loop;
import de.michab.scream.pops.ShortcutAnd;
import de.michab.scream.pops.ShortcutOr;
import de.michab.scream.util.Scut;
import urschleim.Continuation;
import urschleim.Continuation.Cont;
import urschleim.Continuation.Thunk;
import urschleim.Holder;

/**
 * Followed the scheme spec in naming this class.  An alternate name would be
 * 'macro', also common is 'special form'.
 *
 * The implementation of this syntax facility is currently very simple.  All
 * syntactic elements of Scheme are mapped to straightforward method calls.
 * The problem here is that for each use of the 'do' machinery all syntax
 * analysis has to be done over and over again.  Solution is to switch to some
 * sort of primitive notation.  Basically a 'do' is a primitive operation for
 * all kinds of iteration sequences.  Primitives get evaluated (compiled?) once
 * and will be installed in the expression.  Another problem that could be
 * solved with this approach is the switch of the frontend language.  E.g.
 * more C-like notation could be possible then, where all of the special
 * processing is encapsulated in the frontend.  One advantage of Scheme being
 * the first implemented frontend is that this language by design tries to
 * implement the universal superset of language mechanisms so most other
 * languages should map onto this -- at least control structure.<br>
 *
 * Well, to be honest, this is not really new but is standard compiler design
 * since the seventies.  In a nutshell what we need is a compiled intermediate
 * language.<br>
 *
 * TODO: At the moment this class is pretty bare bones.  Only makes sense as a
 * base class for native java syntax implementations.  Will have to look into
 * syntax related scheme sections to implement the functionality specified there.
 *
 * @author Michael Binz
 */
public class Syntax
    extends Operation
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "syntax";

    /**
     *
     */
    private static final Symbol ELSE = Symbol.createObject( "else" );

    /**
     * Default constructor.  Used for Java-defined specializations.
     *
     * @param name The new syntax' symbolic name.
     */
    private Syntax( Symbol name )
    {
        super( name );
    }

    /**
     * Constructor for Java-defined specializations.  The passed string names
     * the symbol bound to this syntax.
     *
     * @param name The string name of the symbol bound to the new syntax.  Used
     *        for error reporting.
     */
    protected Syntax( String name )
    {
        this( Symbol.createObject( name ) );
    }

    /**
     *
     * @param e
     * @param args
     * @param body
     * @throws RuntimeX
     */
    protected Syntax( Environment e,
            FirstClassObject args,
            Cons body  )
                    throws RuntimeX
    {
        super( args, body, e );
    }

    /**
     * TODO fix comment
     * Activate, receives an array of FirstClassObjects instead of a Cons.  May
     * be handier for native java syntax implementations.<br>
     * Note that the different <code>activate</code> calls implement a chain of
     * responsibility -- one of them has to be overridden and implement the
     * actual functionality.  The default behavior of this final part in the
     * chain is to throw an <code>InternalError</code>.
     *
     * @param parent The parent environment.
     * @param arguments The argument list.
     * @return The result of the activation.
     * @throws RuntimeX In case an error occurred.
     * @throws InternalError In case this method is not overridden.
     */
    @Override
    protected FirstClassObject activate( Environment parent,
            FirstClassObject[] arguments )
                    throws RuntimeX
    {
        // TODO Below is the only reference to body from a subclass of Operation.
        // This is really a HACK since i have no better idea currently how to
        // differentiate btw Java and Scheme implemented Syntaxes/Operations.
        // Think.  We should get rid of the _body reference and set body as private
        // to Operation.
        if ( _body == Cons.NIL )
            return evaluate( compile( parent, arguments ), parent );

        return super.activate( parent, arguments );
    }

    @Override
    public Thunk _activate( Environment e, Cons args, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        if ( _body == Cons.NIL )
        {
            var toEval = compile( e, args );

            return toEval.evaluate( e, c );
        }

        return super._activate( e, args, c );
    }

    /**
     * @return A string representation for this syntax.
     * @see FirstClassObject#toString
     */
    @Override
    public String toString()
    {
        return "<Syntax " + getName() + ">";
    }

    /**
     * Interprets a tail sequence.  A tail sequence is represented by a list of
     * expressions whose last one will be evaluated as trailing context.
     *
     * @param seq The list of expressions.
     * @param env The environment used for expression evaluation.
     * @return The result of the last expression in the list.
     * @throws RuntimeX If an error occurs while evaluating the expressions.
     */
    protected static FirstClassObject interpretTailSequence(
            FirstClassObject[] seq,
            Environment env )
                    throws
                    RuntimeX
    {
        return interpretTailSequence( seq, 0, env );
    }

    /**
     * Interprets a tail sequence.  A tail sequence is represented by a list of
     * expressions whose last one will be evaluated as trailing context.
     *
     * @param seq The list of expressions.
     * @param startIdx The index of the first expression to evaluate.
     * @param env The environment used for expression evaluation.
     * @return The result of the last expression in the list.
     * @throws RuntimeX If an error occurs while evaluating the expressions.
     */
    protected static FirstClassObject interpretTailSequence(
            FirstClassObject[] seq,
            int startIdx,
            Environment env )
                    throws
                    RuntimeX
    {
        for ( int i = startIdx ; i < seq.length -1 ; i++ )
            evaluate( seq[i], env );

        return evaluateTrailingContext( seq[ seq.length-1 ], env );
    }

    /**
     * r7rs 4.1.5
     *
     * <code>
     * (if <test> <consequent> <alternate>)<br>
     * (if <test> <consequent>)<br>
     * </code><br>
     * Syntax: <Test>, <consequent>, and <alternate> may be arbitrary
     * expressions.<br>
     * Semantics: An if expression is evaluated as follows:  First, <test> is
     * evaluated. If it yields a true value, then
     * <consequent> is evaluated and its value(s) is(are) returned. Otherwise
     * <alternate> is evaluated and its value(s) is(are) returned. If <test>
     * yields a false value and no <alternate> is specified, then the result of
     * the expression is unspecified.
     */
    static private Operation ifSyntax = new Syntax( "if" )
    {
        private Lambda compImpl( Environment env, Cons cond, Cons positive )
                throws RuntimeX
        {
            var ccond = cond.getCar().compile( env );
            cond.setCar( ccond );
            var cpositive = positive.getCar().compile( env );
            positive.setCar( cpositive );

            Lambda.L result = (e,c) -> {
                return Continuation._if(
                        e,
                        ccond,
                        cpositive,
                        c );
            };

            return new Lambda( result, getName() );
        }
        private Lambda compImpl( Environment env, Cons cond, Cons positive, Cons negative )
                throws RuntimeX
        {
            var ccond = cond.getCar().compile( env );
            cond.setCar( ccond );
            var cpositive = positive.getCar().compile( env );
            positive.setCar( cpositive );
            var cnegative = negative.getCar().compile( env );
            negative.setCar( cnegative );

            Lambda.L result = (e,c) -> {
                return Continuation._if(
                        e,
                        ccond,
                        cpositive,
                        cnegative,
                        c );
            };

            return new Lambda( result, getName() );
        }

        @Override
        protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
        {
            long argsLen =
                    checkArgumentCount( 2, 3, args );

            if ( argsLen == 2 )
            {
                return compImpl(
                        env,
                        args,
                        Scut.as( Cons.class, args.listTail( 1 ) ) );
            }
            else if ( argsLen == 3 )
            {
                return compImpl(
                        env,
                        args,
                        Scut.as( Cons.class, args.listTail( 1 ) ),
                        Scut.as( Cons.class, args.listTail( 2 ) ) );
            }

            throw new InternalError();
        }

        // Legacy ..
        @Override
        public FirstClassObject compile( Environment parent, FirstClassObject[] args )
                throws RuntimeX
        {
            checkMinimumArgumentCount( 2, args );
            checkMaximumArgumentCount( 3, args );

            // Compile referenced nodes.
            for ( int i = 0 ; i < args.length ; i++ )
                args[i] = compile( args[i], parent );

            FirstClassObject condition = args[0];
            FirstClassObject onTrue = args[1];
            // Handle optional 'else' branch.
            FirstClassObject onFalse = args.length == 3 ? args[2] : Cons.NIL;

            // Optimisation of constant sub expressions.  If this is sth like
            // (if #t ...) no 'if' node is needed at all.
            //      if ( isConstant( condition ) )
            //      {
            //        System.err.println( "removed 'if'" );
            //
            //        if ( condition != SchemeBoolean.F )
            //          return onTrue;
            //        else
            //          return onFalse;
            //      }

            // Now create the compiled node.
            return new If( condition, onTrue, onFalse );
        }
    };

    /**
     * (cond <clause1> <clause2> ...)  syntax r7rs, 4.2.1, p14
     */
    static private Operation condSyntax = new Syntax( "cond" )
    {
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
    };

    /**
     * (and <test1> ...) syntax; r5rs 11
     *
     * The test expressions are evaluated from left to right, and the value of
     * the first expression that evaluates to a false value (see section 6.3.1)
     * is returned. Any remaining expressions are not evaluated. If all the
     * expressions evaluate to true values, the value of the last expression is
     * returned.  If there are no expressions then #t is returned.
     */
    static private Syntax andSyntax = new Syntax( "and" )
    {
        @Override
        public FirstClassObject compile( Environment parent, FirstClassObject[] args )
                throws RuntimeX
        {
            // Constant expression optimization.
            if ( args.length == 0 )
                return SchemeBoolean.T;

            // Compile all passed expressions.
            for ( int i = args.length-1 ; i >= 0 ; i-- )
                args[i] = compile( args[i], parent );

            return new ShortcutAnd( args );
        }
    };

    /**
     * (or <test1> ... ) syntax; r5rs 11
     *
     * The test expressions are evaluated from left to right, and the value of
     * the first expression that evaluates to a true value (see section 6.3.1) is
     * returned. Any remaining expressions are not evaluated. If all expressions
     * evaluate to false values, the value of the last expression is returned. If
     * there are no expressions then #f is returned.
     */
    static private Syntax orSyntax = new Syntax( "or" )
    {
        @Override
        public FirstClassObject compile( Environment parent, FirstClassObject[] args )
                throws RuntimeX
        {
            // Constant expression optimisation.
            if ( args.length == 0 )
                return SchemeBoolean.F;

            // Compile all passed expressions.
            for ( int i = args.length-1 ; i >= 0 ; i-- )
                args[i] = compile( args[i], parent );

            return new ShortcutOr( args );
        }
    };

    /**
     *
     */
    static abstract class LetSyntax
    extends Syntax
    {
        public LetSyntax( String name )
        {
            super( name );
        }

        @Override
        public FirstClassObject compile( Environment parent, FirstClassObject[] args )
                throws RuntimeX
        {
            checkMinimumArgumentCount( 2, args );

            FirstClassObject exceptionInfo = Cons.NIL;

            Symbol[] variables = null;
            FirstClassObject[] inits = null;

            // If there are bindings...
            if ( args[0] != Cons.NIL )
            {
                // ...decompose them.  As soon something is wrong a ClassCastException
                // is thrown, resulting in an error message.
                try
                {
                    exceptionInfo = args[0];
                    // CCEx here if bindings was no Cons.
                    FirstClassObject[] bindings = ((Cons)args[0]).asArray();
                    variables = new Symbol[ bindings.length ];
                    inits = new FirstClassObject[ bindings.length ];

                    for ( int i = 0 ; i < bindings.length ; i++ )
                    {
                        exceptionInfo = bindings[i];
                        // CCEx here if a single binding was no cons.
                        Cons binding = (Cons)bindings[i];
                        if ( Cons.NIL == binding )
                            throw new ClassCastException();
                        // Check whether the single binding is a proper two element list
                        // and throw an artificial CCEx to get in the BAD_BINDING handler.
                        if ( !binding.isProperList() || binding.length() != 2 )
                            throw new ClassCastException();
                        // Decompose the single binding.  CCEx here if the first element in
                        // the list is no symbol.
                        variables[i] = (Symbol)binding.listRef( 0 );
                        // CCEx here is not really possible.
                        inits[i] = compile( binding.listRef( 1 ), parent );
                    }
                }
                catch ( ClassCastException e )
                {
                    throw new RuntimeX(
                            Code.BAD_BINDING,
                            toString( getName() ),
                            toString( exceptionInfo ) );
                }
            }
            else
            {
                // We received no bindings.
                variables = new Symbol[0];
                inits = new FirstClassObject[0];
            }

            FirstClassObject[] body = new FirstClassObject[ args.length - 1 ];
            System.arraycopy( args, 1, body, 0, body.length );
            for ( int i = body.length-1 ; i >= 0 ; i-- )
                body[i] = compile( body[i], parent );

            return createPop( variables, inits, body );
        }

        /**
         * Create the actual primitive operation that implements the let syntax.
         * This is a template method to be overridden by the concrete let
         * implementations.
         *
         * @param variables
         * @param inits
         * @param body
         * @return The newly created primitive.
         */
        abstract FirstClassObject createPop( Symbol[] variables,
                FirstClassObject[] inits,
                FirstClassObject[] body );
    };

    /**
     * (let <bindings> <body>) syntax r5rs, 11
     * where bindings is ((variable1 init1) ...) and body is a sequence of
     * expressions.
     */
    static private Syntax letSyntax = new LetSyntax( "let" )
    {
        @Override
        FirstClassObject createPop( Symbol[] variables,
                FirstClassObject[] inits,
                FirstClassObject[] body )
        {
            return new Let( variables, inits, body );
        }
    };

    /**
     * (let* <bindings> <body>) syntax r5rs, 11
     * where bindings is ((variable1 init1) ...) and body is a sequence of
     * expressions.
     */
    static private Syntax letAsteriskSyntax = new LetSyntax( "let*" )
    {
        @Override
        FirstClassObject createPop( Symbol[] variables,
                FirstClassObject[] inits,
                FirstClassObject[] body )
        {
            return new LetAsterisk( variables, inits, body );
        }
    };


    /**
     * (letrec <bindings> <body>) syntax r5rs, 11
     * where bindings is ((variable1 init1) ...) and body is a sequence of
     * expressions.
     */
    static private Syntax letrecSyntax = new LetSyntax( "letrec" )
    {
        @Override
        FirstClassObject createPop( Symbol[] variables,
                FirstClassObject[] inits,
                FirstClassObject[] body )
        {
            return new Letrec( variables, inits, body );
        }
    };

    /**
     * (begin exp1 exp2 ...) library syntax; r7rs 17
     */
    static private Operation beginSyntax = new Operation( "begin" )
    {
        @Override
        public FirstClassObject compile( Environment parent, FirstClassObject[] args )
                throws RuntimeX
        {
            throw new InternalError();
        }

        @Override
        protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
        {
            L l = (e,c) -> Continuation._begin(
                    e,
                    args,
                    c);

            return new Lambda( l, getName() );
        }

        @Override
        public FirstClassObject activate( Environment e, Cons argumentList )
                throws RuntimeX
        {
            Holder<FirstClassObject> r =
                    new Holder<FirstClassObject>( null );
            Holder<ScreamException> error =
                    new Holder<>( null );

            Continuation.trampoline( _activate(
                    e,
                    argumentList,
                    Continuation.endCall( r::set ) ),
                    error::set );

            if ( error.get() != null )
                throw (RuntimeX)error.get();

           return r.get();
        }

        @Override
        public Thunk _activate( Environment e, Cons args, Cont<FirstClassObject> c )
                throws RuntimeX
        {

            return Continuation._begin( e, args, c );
        }
    };

    /**
     * <code>
     * (do ((<variable1> <init1> <step1>)
     *      ... )
     *     (<test> <expression> ... )
     *     command ... )
     * </code><br>
     * Do is an iteration construct. It specifies a set of variables to be bound,
     * how they are to be initialized at the start, and how they are to be
     * updated on each iteration. When a termination condition is met, the loop
     * exits after evaluating the <code>expression</code>s.<br>
     * Do expressions are evaluated as follows: The <code>init</code> expressions
     * are evaluated (in some unspecifed order), the <code>variable</code>s are
     * bound to fresh locations, the results of the <code>init</code> expressions
     * are stored in the bindings of the <code>variable</code>s, and then the
     * iteration phase begins.<br>
     * Each iteration begins by evaluating <code>test</code>; if the result is
     * false (see section 6.3.1), then the <code>command</code> expressions are
     * evaluated in order for effect, the <code>step</code> expressions are
     * evaluated in some unspecified order, the <code>variable</code>s are bound
     * to fresh locations, the results of the <code>step</code>s are stored in
     * the bindings of the <code>variable</code>s, and the next iteration
     * begins.<br>
     * If <code>test</code> evaluates to a true value, then the
     * <code>expression</code>s are evaluated from left to right and the value(s)
     * of the last <code>expression<code> is(are) returned. If no
     * <code>expression<code>s are present, then the value of the do expression
     * is unspecified.<br>
     * The region of the binding of a <code>variable</code> consists of the
     * entire do expression except for the <code>init<code>s. It is an error for
     * a <code>variable<code> to appear more than once in the list of do
     * variables.<br>
     * A <code>step<code> may be omitted, in which case the effect is the same as
     * if <code>(<variable> <init> <variable>)</code> had been written instead of
     * <code>(<variable> <init>)</code>.
     */
    static private Syntax doSyntax = new Syntax( "do" )
    {
        /**
         * Checks if the passed argument is a <code>Cons</code>, is not NIL and is
         * a proper list.  Transforms the list into an array and returns that.
         * @throws RuntimeX
         */
        private FirstClassObject[] isNonNilAndProper( FirstClassObject fco ) throws RuntimeX
        {
            if ( Cons.NIL == fco )
                throw new ClassCastException();
            if ( ! ((Cons)fco).isProperList() )
                throw new ClassCastException();
            return ((Cons)fco).asArray();
        }

        @Override
        public FirstClassObject compile( Environment parent, FirstClassObject[] args )
                throws RuntimeX
        {
            checkMinimumArgumentCount( 2, args );

            try
            {
                FirstClassObject[] bindings = isNonNilAndProper( args[0] );

                Symbol[] vars = new Symbol[ bindings.length ];
                FirstClassObject[] inits = new FirstClassObject[ vars.length ];
                FirstClassObject[] steps = new FirstClassObject[ vars.length ];

                for ( int i = 0 ; i < vars.length ; i++ )
                {
                    FirstClassObject[] binding = isNonNilAndProper( bindings[i] );

                    if ( Cons.NIL == binding[0] )
                        throw new ClassCastException();

                    switch ( binding.length )
                    {
                    case 2:
                        vars[i] = (Symbol)binding[0];
                        inits[i] = compile( binding[1], parent );
                        steps[i] = vars[i];
                        break;
                    case 3:
                        vars[i] = (Symbol)binding[0];
                        inits[i] = compile( binding[1], parent );
                        steps[i] = compile( binding[2], parent );
                        break;

                    default:
                        throw new ClassCastException();
                    }
                }

                FirstClassObject[] testSequence = isNonNilAndProper( args[1] );
                for ( int i = 0 ; i < testSequence.length ; i++ )
                    testSequence[i] = compile( testSequence[i], parent );

                FirstClassObject test = testSequence[0];

                FirstClassObject[] exps = new FirstClassObject[ testSequence.length -1 ];
                System.arraycopy( testSequence, 1, exps, 0, exps.length );

                FirstClassObject[] cmds = new FirstClassObject[ args.length -2 ];
                System.arraycopy( args, 2, cmds, 0, cmds.length );
                for ( int i = 0 ; i < cmds.length ; i++ )
                    cmds[i] = compile( cmds[i], parent );

                return new Loop( vars, inits, steps, test, exps, cmds );
            }
            catch ( ClassCastException e )
            {
                throw new RuntimeX( Code.BAD_BINDING );
            }
        }
    };

    /**
     * (define <variable> <expression>) syntax; r5rs 16
     * (define (<variable> <formals>) <body>) syntax; r5rs 16
     * (define (<variable> . <formal>) <body>) syntax; r5rs 16
     */
    static private Syntax defineSyntax = new Syntax( "define" )
    {
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
                parent.set( (Symbol)args[0], value );
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
                parent.set( (Symbol)symbol, procToBind );
            }
            else
                throw new RuntimeX( Code.SYNTAX_ERROR );

            // This is unspecified.
            return Cons.NIL;
        }
    };

    /**
     * (set! <variable> <expression>) syntax; r7rs 14
     */
    static private Operation setSyntax = new Syntax( "set!" )
    {
        @Override
        protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
        {
            checkArgumentCount( 2, args );

            var symbol = Scut.as(
                    Symbol.class,
                    args.getCar() );
            var value = args.listRef( 1 );

            L l = (e,c) -> Continuation._assign(
                    e,
                    symbol,
                    value,
                    c);

            return new Lambda( l, getName() );
        }

        private Class<?>[] formalArglist =
                new Class[]{ Symbol.class,
                        FirstClassObject.class };
        /**
         *
         */
        @Override
        public FirstClassObject compile( Environment parent, FirstClassObject[] args )
                throws RuntimeX
        {
            checkArguments( formalArglist, args );
            return new Assignment( (Symbol)args[0], compile( args[1], parent ) );
        }
    };

    /**
     * (%time exp)
     *
     * Returns a pair, where the car part holds the time that <code>exp</code>
     * needed to execute and the cdr holds the result of <code>exp</code>.
     */
    static private Syntax timeSyntax = new Syntax( "%time" )
    {
        @Override
        public FirstClassObject activate( Environment parent, FirstClassObject[] args )
                throws RuntimeX
        {
            checkArgumentCount( 1, args );

            // Trigger an explicit garbage collection before starting with the time
            // measurements.
            System.gc();

            // Get the start time...
            long startTime = System.currentTimeMillis();
            // ...do the actual evaluation...
            FirstClassObject resultCdr = evaluate( args[0], parent );
            // ...and compute the time that was needed for the evaluation.
            FirstClassObject resultCar = SchemeInteger.createObject(
                    System.currentTimeMillis() - startTime );

            return new Cons( resultCar, resultCdr );
        }
    };

    /**
     * (%syntax exp)
     *
     * Very similar to 'define'.  Generates macros whose body gets called
     * with not evaluated arguments.
     */
    static private Syntax syntaxSyntax = new Syntax( "%syntax" )
    {
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
    };

    /**
     * Base operations setup.
     *
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( ifSyntax );
        tle.setPrimitive( condSyntax );
        tle.setPrimitive( andSyntax );
        tle.setPrimitive( orSyntax );
        tle.setPrimitive( letSyntax );
        tle.setPrimitive( letAsteriskSyntax );
        tle.setPrimitive( letrecSyntax );
        tle.setPrimitive( beginSyntax );
        tle.setPrimitive( doSyntax );
        tle.setPrimitive( defineSyntax );
        tle.setPrimitive( setSyntax );
        tle.setPrimitive( syntaxSyntax );

        tle.setPrimitive( timeSyntax );

        return tle;
    }
}
