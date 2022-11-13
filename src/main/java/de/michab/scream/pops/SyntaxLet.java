package de.michab.scream.pops;

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
import de.michab.scream.util.Scut.ConsumerX;

/**
 *
 */
public abstract class SyntaxLet
    extends Syntax
{
    private SyntaxLet( String name )
    {
        super( name );
    }

    /**
     * A proper list with length greater than 0 of two element lists, each having
     * a symbol as the first element.
     *
     * @return A proper list of symbols that are assigned in the binding list.
     */
    protected Cons validateBindings( Cons bindings ) throws RuntimeX
    {
        final var originalBindings = bindings;
        ConsumerX<Long> ic = s -> {
            throw Scut.mBadBinding( getName(), originalBindings );
        };

        Scut.checkProperLength(
                bindings,
                1,
                Integer.MAX_VALUE,
                ic ,
                ic );

        Cons result = Cons.NIL;

        while ( bindings != Cons.NIL )
        {
            // Car is a list.
            var c = Scut.as(
                    Cons.class,
                    bindings.getCar(),
                    s-> {
                        throw Scut.mBadBinding( getName(), originalBindings );
                    });
            // Of length 2.
            Scut.checkProperLength(
                    c,
                    2,
                    2,
                    ic,
                    ic );
            // First element is symbol.
            var symbol = Scut.as(
                    Symbol.class,
                    c.getCar(),
                    s -> {
                        throw Scut.mBadBinding( getName(), c );
                    } );

            result = new Cons( symbol, result );

            bindings = (Cons)bindings.getCdr();
        }

        return result;
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

    /**
     * (let <bindings> <body>) syntax r7rs, p16
     * where bindings is ((variable1 init1) ...) and body is a sequence of
     * expressions.
     */
    static private Syntax letSyntax = new SyntaxLet( "let" )
    {
        @Override
        protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
        {
            checkArgumentCount( 2, Integer.MAX_VALUE, args );

            var bindings =
                    Scut.as( Cons.class, args.getCar() );
            var body =
                    Scut.as( Cons.class, args.getCdr() );

            validateBindings( bindings );

            L l = (e,c) -> Continuation._x_let(
                    e,
                    bindings,
                    body,
                    c);

            return new Lambda( l, getName() );
        }

        @Override
        FirstClassObject createPop( Symbol[] variables,
                FirstClassObject[] inits,
                FirstClassObject[] body )
        {
            throw new InternalError();
        }

        @Override
        public FirstClassObject compile( Environment parent, Cons args )
                throws RuntimeX
        {
            return _compile( parent, args );
        }
        @Override
        public FirstClassObject activate( Environment parent,
                Cons arguments )
                        throws RuntimeX
        {
            var λ = _compile( parent, arguments );

            return FirstClassObject.evaluate( λ, parent );
        }

    };

    /**
     * (let* <bindings> <body>) syntax r5rs, 11
     * where bindings is ((variable1 init1) ...) and body is a sequence of
     * expressions.
     */
    static private Syntax letAsteriskSyntax = new SyntaxLet( "let*" )
    {
        @Override
        protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
        {
            checkArgumentCount( 2, Integer.MAX_VALUE, args );

            var bindings =
                    Scut.as( Cons.class, args.getCar() );
            var body =
                    Scut.as( Cons.class, args.getCdr() );

            validateBindings( bindings );

            L l = (e,c) -> Continuation._x_letStar(
                    e,
                    bindings,
                    body,
                    c);

            return new Lambda( l, getName() );
        }

        @Override
        FirstClassObject createPop( Symbol[] variables,
                FirstClassObject[] inits,
                FirstClassObject[] body )
        {
            throw new InternalError();
        }

        @Override
        public FirstClassObject compile( Environment parent, Cons args )
                throws RuntimeX
        {
            return _compile( parent, args );
        }
        @Override
        public FirstClassObject activate( Environment parent,
                Cons arguments )
                        throws RuntimeX
        {
            var λ = _compile( parent, arguments );

            return FirstClassObject.evaluate( λ, parent );
        }
    };

    /**
     * (letrec <bindings> <body>) syntax r5rs, 11
     * where bindings is ((variable1 init1) ...) and body is a sequence of
     * expressions.
     */
    static private Syntax letrecSyntax = new SyntaxLet( "letrec" )
    {
        @Override
        protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
        {
            checkArgumentCount( 2, Integer.MAX_VALUE, args );

            var bindings =
                    Scut.as( Cons.class, args.getCar() );
            var body =
                    Scut.as( Cons.class, args.getCdr() );

            var symbols = validateBindings( bindings );

            L l = (e,c) -> Continuation._x_letRec(
                    e,
                    bindings,
                    body,
                    symbols,
                    c);

            return new Lambda( l, getName() );
        }

        @Override
        public FirstClassObject compile( Environment parent, Cons args )
                throws RuntimeX
        {
            return _compile( parent, args );
        }
        @Override
        public FirstClassObject activate( Environment parent,
                Cons arguments )
                        throws RuntimeX
        {
            var λ = _compile( parent, arguments );

            return FirstClassObject.evaluate( λ, parent );
        }

        @Override
        FirstClassObject createPop( Symbol[] variables,
                FirstClassObject[] inits,
                FirstClassObject[] body )
        {
            throw new InternalError();
        }
    };

    /**
     * Base operations setup.
     *
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( letAsteriskSyntax );
        tle.setPrimitive( letSyntax );
        tle.setPrimitive( letrecSyntax );

        return tle;
    }
}
