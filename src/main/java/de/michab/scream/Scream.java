/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;

import org.smack.util.Holder;
import org.smack.util.JavaUtil;
import org.smack.util.ServiceManager;
import org.smack.util.resource.ResourceManager;
import org.smack.util.resource.ResourceManager.Resource;

import de.michab.scream.binding.SchemeObject;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Number;
import de.michab.scream.fcos.Port;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.fcos.SchemeString;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.fcos.Syntax;
import de.michab.scream.frontend.SchemeParser;
import de.michab.scream.pops.Primitives;
import de.michab.scream.pops.SyntaxAnd;
import de.michab.scream.pops.SyntaxAssign;
import de.michab.scream.pops.SyntaxBegin;
import de.michab.scream.pops.SyntaxCase;
import de.michab.scream.pops.SyntaxCond;
import de.michab.scream.pops.SyntaxDefine;
import de.michab.scream.pops.SyntaxDo;
import de.michab.scream.pops.SyntaxIf;
import de.michab.scream.pops.SyntaxLambda;
import de.michab.scream.pops.SyntaxLet;
import de.michab.scream.pops.SyntaxOr;
import de.michab.scream.pops.SyntaxQuote;
import de.michab.scream.pops.SyntaxSyntax;
import de.michab.scream.pops.SyntaxTime;
import de.michab.scream.util.Continuation;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Continuation.ToStackOp;
import de.michab.scream.util.FunctionX;
import de.michab.scream.util.LoadContext;
import de.michab.scream.util.LogUtil;
import de.michab.scream.util.Scut;
import de.michab.scream.util.SupplierX;

/**
 * Facade to the Scheme interpreter.
 *
 * @author Michael G. Binz
 */
public class Scream implements ScriptEngineFactory
{
    /**
     * Reads the resources.  Terminates the application if any problem occurs.
     * This represents the very first phase of boot up.
     */
    static
    {
        org.smack.util.ServiceManager.getApplicationService(
                org.smack.util.resource.ResourceManager.class )
        .injectResources( Scream.class );

        LogUtil.setLevel(
                Level.WARNING,
                Scream.class.getPackage().getName() );
    }

    /**
     * The logger for this class.
     */
    private final static Logger LOG =
            Logger.getLogger( Scream.class.getName() );

    /**
     * Symbol is bound to an error stream.  Available in static initialization.
     */
    public final static Symbol INIT_ERR_STREAM =
            Symbol.createObject( "%%errOut%%" );

    /**
     * This is the relative path to our extensions package.
     */
    private final static String EXTENSION_POSITION = "extensions/";

    /**
     * The name of the file that holds the Scheme-implemented parts of the
     * engine common to all script engine instance.
     */
    private static String schemeExtensions = "common-init.s";

    /**
     * The name of the file that holds the Scheme-implemented parts that are
     * specific for each script engine.
     */
    private static final String schemeInstanceExtensions = "instance-init.s";

    @Resource
    private static String engineName;
    @Resource
    private static String engineVersion;

    /**
     * Creates an instance of an working Scheme interpreter.
     *
     * @param in The reader to use.
     * @param out The writer to use by this interpreter.
     */
    public Scream()
    {
        ServiceManager.getApplicationService( ResourceManager.class )
            .injectResources( Scream.class );
    }

    private static Thunk evalImpl_(
            Environment e,
            SupplierX<FirstClassObject,RuntimeX> s,
            FirstClassObject previousResult,
            FirstClassObject newExpression,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        if ( newExpression == Port.EOF )
            return c.accept( previousResult );

        return Primitives._x_eval(
                e,
                newExpression,
                fco -> evalImpl_( e, s, fco, s.get(), c ) );
    }

    public static Thunk evalImpl(
            Environment e,
            SupplierX<FirstClassObject,RuntimeX> s,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        return evalImpl_(
                e,
                s,
                Cons.NIL,
                s.get(),
                c );
    }

    @FunctionalInterface
    public interface FcoOp {
        Thunk call( Cont<FirstClassObject> c )
            throws RuntimeX;
    }

    /**
     * A Scheme continuation.
     *
     * @param <R> The type accepted.
     */
    @FunctionalInterface
    public static interface Cont<R> {
        Thunk accept(R result) throws RuntimeX;
    }

    private static Cont<FirstClassObject> mapCont( de.michab.scream.util.Continuation.Cont<FirstClassObject> cont )
    {
        return  c -> {
            try
            {
                return cont.accept( c );
            }
            catch ( Exception e )
            {
                throw new InternalError();
            }
        };
    }

    private static ToStackOp<FirstClassObject> mapOp( FcoOp op )
    {
        return c -> op.call( mapCont( c )  );
    }

    @Deprecated
    private static FirstClassObject toStack( FcoOp op )
            throws RuntimeX
    {
        ToStackOp<FirstClassObject> tso2 = mapOp( op );

        try
        {
            return Continuation.toStack( tso2 );
        }
        catch (Exception e) {
            if ( RuntimeX.class.isAssignableFrom( e.getClass() ))
                throw RuntimeX.class.cast( e );

            throw RuntimeX.mInternalError( e );
        }
    }
    public static FirstClassObject toStack( FcoOp op,
            Holder<FirstClassObject> result,
            Holder<Exception> exception)
            throws RuntimeX
    {
        ToStackOp<FirstClassObject> tso2 = mapOp( op );

        try
        {
            return Continuation.toStack( tso2, result, exception );
        }
        catch (Exception e) {
            if ( RuntimeX.class.isAssignableFrom( e.getClass() ))
                throw RuntimeX.class.cast( e );

            throw RuntimeX.mInternalError( e );
        }
    }

    static FirstClassObject evalImpl(
            Environment env,
            SupplierX<FirstClassObject,RuntimeX> spl,
            Holder<FirstClassObject> result,
            Holder<Exception> exception)

                    throws RuntimeX
    {
        return toStack(
                c -> evalImpl( env, spl, c ),
                result,
                exception );
    }

    @Deprecated
    private static FirstClassObject evalImpl(
            Environment env,
            SupplierX<FirstClassObject,RuntimeX> spl )
                    throws RuntimeX
    {
        return toStack(
                c -> evalImpl( env, spl, c ) );
    }

    /**
     * Loads the specified extensions from Scheme source files. These
     * have to be located in a special package de.michab.scream.extensions and
     * each file has to be specified in the Scream.properties file by the
     * kernel.schemeExtensions key.
     *
     * @param env The environment used for evaluating the extensions.
     * @param fileNames The files to load.
     */
    static void addExtensions(
            Environment env,
            String filename )
    {
        // The getResourceAsStream in the next line addresses resources relative
        // to the classes package.  So here we create a name like
        // extensions/foo.s.
        String crtFileName = EXTENSION_POSITION + filename;

        // Try to get a stream on the file...
        var url = Scream.class.getResource( crtFileName );

        JavaUtil.Assert(
                url != null,
                "File for processing not found: '%s'",
                crtFileName );

        try
        {
            load( url, env );
        }
        catch ( RuntimeX e )
        {
            LOG.log(
                    Level.SEVERE,
                    e.getMessage() );
            throw new InternalError( e );
        }
    }

    /**
     * Loads a Scheme source file into the passed environment.
     *
     * @param filename The name of the file to load.
     * @throws RuntimeX In case of errors.
     */
    @Deprecated
    public static FirstClassObject load( SchemeString filename, Environment environment )
            throws RuntimeX
    {
        return load( new LoadContext( filename.getValue() ), environment );
    }

    /**
     * Loads a Scheme source file into the passed environment.
     *
     * @param filename The URL of the file to load.
     * @throws RuntimeX In case of errors.
     */
    public static FirstClassObject load( URL filename, Environment environment )
            throws RuntimeX
    {
        return load( new LoadContext( filename ), environment );
    }

    /**
     * Load a Scheme source file.
     *
     * @param file The name of the file to load.
     * @throws RuntimeX In case of errors.
     */
    @Deprecated
    private static FirstClassObject load( LoadContext file, Environment e )
            throws RuntimeX
    {
        try ( var reader  = LoadContext.getReader( file ) )
        {
            SchemeParser parser =
                    new SchemeParser( reader );

            return evalImpl( e, parser::getExpression );
        }
        catch ( IOException ioe )
        {
            throw RuntimeX.mIoError( ioe );
        }
    }

    /**
     * Entry point. Rarely used.
     */
    public static void main( String[] argv ) throws Exception
    {
        Thread.currentThread().setName( "screamMain" );

        var interpreter = new Scream();

        var engine = interpreter.getScriptEngine();

        engine.eval( new InputStreamReader( System.in ) );
    }

    /**
     * Apply an operation on a list of arguments.
     *
     * @param elementType The target type for the list elements.
     * @param operation The operation to apply.
     * @param e The environment for evaluation.
     * @param args The argument list.
     * @param previousResult The result of the previous application.
     * @param c The continuation receiving the result.
     * @return A thunk.
     * @throws RuntimeX
     */
    private static <T extends FirstClassObject>
    Thunk _apply(
            Class<T> elementType,
            FunctionX<T, FirstClassObject, RuntimeX> operation,
            Environment e,
            Cons args,
            FirstClassObject previousResult,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        if ( args == Cons.NIL )
            return c.accept( previousResult );


        Cont<FirstClassObject> next =
                (fco) -> _apply(
                        elementType,
                        operation,
                        e,
                        Scut.as( Cons.class, args.getCdr() ),
                        fco,
                        c );

        return Primitives._x_eval(
                e,
                operation.apply( Scut.as( elementType, args.getCar() ) ),
                next );
    }

    /**
     * Apply an operation on a list of arguments.
     *
     * @param elementType The target type for the list elements.
     * @param operation The operation to apply.
     * @param e The environment for evaluation.
     * @param args The argument list.
     * @param c The continuation receiving the result.
     * @return A thunk.
     * @throws RuntimeX
     */
    public static <T extends FirstClassObject>
    Thunk _x_apply(
            Class<T> elementType,
            FunctionX<T, FirstClassObject, RuntimeX> operation,
            Environment e,
            Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        return () -> _apply(
                elementType,
                operation,
                e,
                args,
                Cons.NIL,
                c );
    }

    /**
     * {@code (include <string₁> <string₂> ...)}
     * <p>
     * {@code r7rs 4.1.7 p14} syntax
     */
    static private Syntax includeProcedure = new Syntax( "include" )
    {
        @Override
        protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                throws RuntimeX
        {
            checkArgumentCount( 1, Integer.MAX_VALUE, args );

            return _x_apply(
                    SchemeString.class,
                    s -> { return load( s, e ); },
                    e,
                    args,
                    c );
        }
    };

    /**
     * {@code (eval exp-or-def environment-specifier)}
     * <p>
     * {code r7rs 6.1.2 p55} eval library procedure
     */
    static private Procedure evalProcedure = new Procedure( "eval" )
    {
        @Override
        protected Thunk _executeImpl(
                Environment e,
                Cons args,
                Cont<FirstClassObject> c )
            throws RuntimeX
        {
            checkArgumentCount( 2, args );

            var expOrDef =
                    args.listRef( 0 );
            Environment environment = Scut.as(
                    Environment.class,
                    args.listRef( 1 ) );

            return expOrDef.evaluate( environment, c );
        }
    };

    /**
     * Environment operations setup.
     *
     * @param tle A reference to the system private top level environment.
     * @return A reference to the environment including the additional entries
     *        defined by this class.
     * @throws RuntimeX
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
            throws RuntimeX
    {
        tle.setPrimitive( evalProcedure );

        return tle;
    }

    @Override
    public String getEngineName() {
        return engineName;
    }

    @Override
    public String getEngineVersion() {
        return engineVersion;
    }

    @Override
    public List<String> getExtensions() {
        return Collections.emptyList();
    }

    @Override
    public List<String> getMimeTypes() {
        return Collections.emptyList();
    }

    private static final List<String> _names =
            Collections.unmodifiableList(
                        Arrays.asList( "scheme", "scream" ) );

    @Override
    public List<String> getNames() {
        return _names;
    }

    @Override
    public String getLanguageName() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getLanguageVersion() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Object getParameter(String key) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getMethodCallSyntax(String obj, String m, String... args) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getOutputStatement(String toDisplay) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getProgram(String... statements) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ScriptEngine getScriptEngine()
    {
        JavaUtil.Assert( _topLevelEnvironment.isConstant() );

        try {
            return new ScreamEvaluator(
                    this,
                    _topLevelEnvironment,
                    schemeInstanceExtensions );
        }
        catch ( RuntimeX rx )
        {
            throw new InternalError( rx );
        }
    }

    /**
     * A reference to the top level environment.  This is a single common
     * instance holding all the core scheme definitions.  This instance is shared
     * between all interpreter instances and represents the root in the
     * environment hierarchy.
     *
     * Assignment of values to symbols bound in this environment will take place
     * for all SchemeInterpreter instances.
     *
     * @see de.michab.scream.Scream#_localTle
     */
    private final static Environment _topLevelEnvironment =
            FirstClassObject.setConstant( createTle() );

    /**
     * Loads all classes defined in the kernel.integrateClasses property and
     * invokes the following method signature on the class:
     * <p>
     *  {@code public static Environment extendTopLevelEnvironment( Environment tle )}
     *
     * @param tle The top level environment to contain the new bindings.
     */
    private static Environment createTle()
    {
        var result = createNullEnvironment().extend( "tle-common" );

        try
        {
            RuntimeX.extendTopLevelEnvironment( result );
            extendTopLevelEnvironment( result );
            de.michab.scream.fcos.Continuation.extendTopLevelEnvironment( result );
            Number.extendTopLevelEnvironment( result );
            Port.extendTopLevelEnvironment( result );
            Environment.extendTopLevelEnvironment( result );
            SchemeObject.extendTopLevelEnvironment( result );
        }
        catch ( Exception e )
        {
            LOG.log(
                    Level.SEVERE,
                    "Init threw exception.",
                    e.getCause() );
            throw new InternalError( e );
        }

        // Load extensions defined in scheme source files.
        addExtensions(
                result,
                schemeExtensions );

        return result;
    }

    /**
     * Creates the {@code null-environment}.
     * <p>
     * {@code r7rs 6.12 p55}
     *
     * @return the immutable {@code null-environment}.
     */
    private static Environment createNullEnvironment()
    {
        Environment result = new Environment( "null" );

        try
        {
            SyntaxAnd.extendNullEnvironment( result );
            SyntaxAssign.extendNullEnvironment( result );
            SyntaxBegin.extendNullEnvironment( result );
            SyntaxCase.extendNullEnvironment( result );
            SyntaxCond.extendNullEnvironment( result );
            SyntaxDefine.extendNullEnvironment( result );
            SyntaxDo.extendNullEnvironment( result );
            SyntaxIf.extendNullEnvironment( result );
            SyntaxLambda.extendNullEnvironment( result );
            SyntaxLet.extendNullEnvironment( result );
            SyntaxOr.extendNullEnvironment( result );
            SyntaxQuote.extendNullEnvironment( result );
            SyntaxSyntax.extendNullEnvironment( result );
            SyntaxTime.extendNullEnvironment( result );

            result.setPrimitive( includeProcedure );

            result.define(
                    Symbol.createObject( "scream:null-environment" ),
                    result );

            return FirstClassObject.setConstant( result );
        }
        catch ( Exception e )
        {
            throw new InternalError( e );
        }
    }
}
