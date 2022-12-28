/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Writer;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Stack;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;

import org.smack.util.JavaUtil;
import org.smack.util.ServiceManager;
import org.smack.util.resource.ResourceManager;
import org.smack.util.resource.ResourceManager.Resource;

import de.michab.scream.Continuation.Cont;
import de.michab.scream.Continuation.Thunk;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.frontend.SchemeParser;
import de.michab.scream.pops.Primitives;
import de.michab.scream.util.LoadContext;
import de.michab.scream.util.LogUtil;
import de.michab.scream.util.Scut;
import de.michab.scream.util.SupplierX;
import urschleim.Holder;

/**
 * Facade to the Scheme interpreter.  This class is the only connection between
 * a client and a scripting engine.  The actual connection between those two
 * entities is represented by a reader/writer pair, i.e. the primary interface
 * into the interpreter is stream based.</br>
 * The top level environment of each instance of this class contains a binding
 * %%interpreter%% that refers to the instance and makes access possible from
 * inside Scream.</br>
 * Interpreter instances are created by the <code>createInterpreter</code>
 * method.  An instance shuts down if an EOF is read from its reader or if the
 * <code>dispose()</code> method is called.
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

    private final static ThreadLocal<Stack<LoadContext>> loadStack =
            ThreadLocal.withInitial( Stack<LoadContext>::new );

    /**
     * @see de.michab.scream.Scream#getErrorPort
     * @see de.michab.scream.Scream#_errorPort
     */
//    private final static PrintWriter _errorWriter = new PrintWriter( System.err );

    /**
     * The symbol being bound to an object reference of the interpreter itself.
     */
    public final static Symbol ANCHOR_SYMBOL =
            Symbol.createObject( "%%interpreter%%" );

    /**
     * Symbol receives an error id when an error occurred.
     */
//    private final static Symbol ERROR_ID =
//            Symbol.createObject( "%error-id" );

    /**
     * Symbol receives the actual exception when an error occurred.
     */
//    private final static Symbol ERROR_OBJ =
//            Symbol.createObject( "%error-object" );

    /**
     * Symbol is bound to an error stream.  Available in static initialization.
     */
    public final static Symbol INIT_ERR_STREAM =
            Symbol.createObject( "%%errOut%%" );

    /**
     * This is the relative path to our extensions package.
     */
    private final static String EXTENSION_POSITION = "extensions/";
//    private final static String schemeTestPosition = "test/";

    /**
     * Property key: specifies additional classes the kernel should load on
     * startup.
     * <code>kernel.integrateClasses = fooExtension bar baz</code>
     */
    @Resource
    private static String[] integrateClasses;

    /**
     * Property key: specifies additional scheme files the kernel should load on
     * startup.  These have to be located in a special subpackage 'extensions'.
     *
     * Sample: <code>kernel.schemeExtensions = math.s common.s patch.s</code>
     *
     * @see de.michab.scream.Scream#kernelSchemeInstanceExtensionsP
     */
    @Resource
    private static String[] schemeExtensions;

    /**
     * Property key: specifies scheme files containing regression tests.  These
     * have to be located in a special subpackage 'tests'.
     *
     * Sample: <code>kernel.regression = tmath.s</code>
     */
    @Resource
    private static String[] regression;

    /**
     * Property key: specifies additional scheme files the kernel should load
     * for each interpreter instance.  In general these contain function
     * definitions that depend on a reference to the individual interpreter
     * instance, accessible via the symbol ANCHOR_SYMBOL also defined in this
     * class.  The extension definition files have to be located in a special
     * subpackage 'extensions'.
     *
     * Sample: <code>kernel.schemeExtensions = math.s common.s patch.s</code>
     */
    @Resource
    private static String[] schemeInstanceExtensions;

    @Resource
    private static String shell;

    /**
     * This is the name of the method that is looked up on the classes that are
     * added via the "kernel.integrateClasses" property.
     */
    private final static String INIT_NAME = "extendTopLevelEnvironment";

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

    /**
     * Get the standard input port for this interpreter instance.
     *
     * @return This interpreter's standard in.
     */
    public Port getInPort()
    {
        return null; //_inPort;
    }

    /**
     * Get the standard output port for this interpreter instance.
     *
     * @return This interpreter's standard out.
     */
    public Port getOutPort()
    {
        return null; //_outPort;
    }

    /**
     * Get the common error port for the system.  This is normally a reference to
     * the System.err stream.  To redefine the error port the initialize() method
     * has to be called before any other operation on the SchemeInterpreter
     * class.
     *
     * @return This interpreter's standard error port.
     * @see de.michab.scream.Scream#initialise
     */
    static public Port getErrorPort()
    {
        return null; //_errorPort;
    }

    /**
     * Loads all classes defined in the kernel.integrateClasses property and
     * invokes the following method signature on the class:<br>
     *  <code>public static Environment extendTopLevelEnvironment( Environment tle )</code><br>
     *
     * @param tle The top level environment to contain the new bindings.
     */
    private static Environment createTle()
    {
        var result =
                new Environment();

        for ( var crtClassName : integrateClasses )
        {
            LOG.info( "Initializing: " + crtClassName );

            try
            {
                Class<?> clazz = Class.forName( crtClassName );
                java.lang.reflect.Method init = clazz.getDeclaredMethod(
                        INIT_NAME,
                        new Class[]{ result.getClass() } );
                init.invoke( null, new Object[]{ result } );
            }
            catch ( ClassNotFoundException e )
            {
                LOG.log(
                        Level.WARNING,
                        "Class for initialization not found: ''{0}''",
                        crtClassName );
            }
            catch ( NoSuchMethodException e )
            {
                LOG.fine(
                        "No init needed for class '" +
                                crtClassName +
                        "'." );
            }
            catch ( IllegalAccessException e )
            {
                LOG.log(
                        Level.WARNING,
                        "Illegal access: ''{0}''",
                        crtClassName );
            }
            catch ( java.lang.reflect.InvocationTargetException e )
            {
                LOG.log(
                        Level.WARNING,
                        "Init threw exception.",
                        e.getCause() );
            }
        }

        // Load extensions defined in scheme source files.
        addExtensions(
                result,
                schemeExtensions );

        return result;
    }


    private static Thunk evalImpl_(
            Environment e,
            SupplierX<FirstClassObject> s,
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
            SupplierX<FirstClassObject> s,
            // TODO
            Writer sink,
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

    public static FirstClassObject evalImpl(
            Environment e,
            SupplierX<FirstClassObject> spl,
            // TODO
            Writer sink )
                    throws RuntimeX
    {
        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                evalImpl(
                        e,
                        spl,
                        sink,
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set( s ) );

        if ( error.get() != null )
            throw (RuntimeX)error.get();

        return r.get();
    }

    /**
     * This method loads the specified extensions from Scheme source files. These
     * have to be located in a special package de.michab.scream.extensions and
     * each file has to be specified in the Scream.properties file by the
     * kernel.schemeExtensions key.
     *
     * @param env The environment used for evaluating the extensions.
     * @param propertyName The properties file to use.
     * @param err The error stream to be used in case of problems.
     */
    private static void addExtensions(
            Environment env,
            String[] fileNames )
    {
        // Init the parser...
        for ( var c : fileNames )
        {
            // The getResourceAsStream in the next line addresses resources relative
            // to the classes package.  So here we create a name like
            // extensions/foo.s.
            String crtFileName = EXTENSION_POSITION + c;

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
    }

    /**
     * Loads a Scheme source file into the passed environment.
     *
     * @param filename The name of the file to load.
     * @throws RuntimeX In case of errors.
     */
    public static FirstClassObject load( String filename, Environment environment )
            throws RuntimeX
    {
        return load( new LoadContext( filename ), environment );
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
     * Loads the scheme source file in the port into the passed environment.  The
     * port is closed before the file's contents is evaluated.
     *
     * @param filename The name of the file to load.
     * @throws RuntimeX In case of errors.
     */
    private static FirstClassObject load( LoadContext current, Environment e )
            throws RuntimeX
    {
        var stack = loadStack.get();

        if ( stack.isEmpty() )
            ;
        else if ( current.isAbsolute() )
            ;
        else if ( ! stack.peek().hasParent() )
            ;
        else
            current = current.relate( stack.peek() );

        stack.push( current );

        try ( var reader  = new InputStreamReader( current.getStream() ) )
        {
            SchemeParser parser =
                    new SchemeParser( reader );

            return evalImpl( e, parser::getExpression, null );
        }
        catch ( IOException ioe )
        {
            throw new RuntimeX( Code.IO_ERROR, ioe.getMessage() );
        }
        finally
        {
            stack.pop();
        }
    }

    /**
     * Entry point.
     */
    public static void main( String[] argv ) throws Exception
    {
        Thread.currentThread().setName( "screamMain" );

        var interpreter = new Scream();

        var engine = interpreter.getScriptEngine();

        engine.eval( new InputStreamReader( System.in ) );
    }

    /**
     * (load <expression>)
     *
     * Currently the environment arguments are not supported.
     */
    static private Procedure loadProcedure = new Procedure( "load" )
    {
        @Override
        protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                throws RuntimeX
        {
            checkArgumentCount( 1, args );

            var string = Scut.as(
                    SchemeString.class,
                    args.getCar() );

            return c.accept( load(
                    string.getValue(),
                    e ) );
        }
    };

    /**
     * (eval <expression>)
     *
     * Currently the environment arguments are not supported.
     */
//    static private Procedure evalProcedure = new Procedure( "eval" )
//    {
//        private Class<?>[] formalArglist =
//                new Class[]{ FirstClassObject.class, Environment.class };
//
//        @Override
//        public FirstClassObject apply( Environment parent, FirstClassObject[] args )
//                throws RuntimeX
//        {
//            checkArguments( formalArglist, args );
//
//            // Do it.
//            return evaluate( args[0], (Environment)args[1] );
//        }
//    };

    /**
     * (scheme-report-environment)
     *
     * Currently the environment arguments are not supported.
     */
//    static private Procedure tleProcedure = new Procedure( "scheme-report-environment" )
//    {
//        private Class<?>[] formalArglist =
//                new Class[]{ SchemeInteger.class };
//
//        @Override
//        public FirstClassObject apply( Environment parent, FirstClassObject[] args )
//                throws RuntimeX
//        {
//            checkArguments( formalArglist, args );
//
//            // Do it.
//            return _topLevelEnvironment;
//        }
//    };

    /**
     * Environment operations setup.
     *
     * @param tle A reference to the system private top level environment.
     * @return A reference to the environment including the additional entries
     *        defined by this class.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
//        tle.setPrimitive( evalProcedure );
        tle.setPrimitive( loadProcedure.setClosure( tle ) );
//        tle.setPrimitive( tleProcedure );

        return tle;
    }

    @Override
    public String getEngineName() {
        return "Scream";
    }

    @Override
    public String getEngineVersion() {
        // TODO Auto-generated method stub
        return null;
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
        // Create the interaction environment.
        var tle = new Environment(
                _topLevelEnvironment );

        addExtensions(
                tle,
                schemeInstanceExtensions );

        return new ScreamEvaluator(
                this,
                tle );
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
    private final static Environment _topLevelEnvironment = createTle();
}
