/* $Id: SchemeInterpreter.java 788 2015-01-10 23:07:11Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.smack.util.resource.ResourceManager.Resource;

import de.michab.scream.binding.SchemeObject;

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
 * @version $Rev: 788 $
 * @author Michael G. Binz
 */
public class SchemeInterpreter
{
    /**
     * The logger for this class.
     */
    private final static Logger log =
            Logger.getLogger( SchemeInterpreter.class.getName() );

    /**
     * Denotes whether class initialization has been executed.
     */
    private static boolean _initialised = false;

    /**
     * A reference to the top level environment.  This is a single common
     * instance holding all the core scheme definitions.  This instance is shared
     * between all interpreter instances and represents the root in the
     * environment hierarchy.<br>
     * Assignment of values to symbols bound in this environment will take place
     * for all SchemeInterpreter instances.
     *
     * @see de.michab.scream.SchemeInterpreter#_localTle
     */
    private static Environment _topLevelEnvironment = null;

    /**
     * A reference to the per interpreter top level environment.  Parent of this
     * is always _topLevelEnvironment.
     *
     * @see de.michab.scream.SchemeInterpreter#_topLevelEnvironment
     * @see de.michab.scream.SchemeInterpreter#getTopLevelEnvironment
     */
    private final Environment _localTle;

    /**
     * The symbol being bound to an object reference of the interpreter itself.
     */
    public final static Symbol ANCHOR_SYMBOL =
            Symbol.createObject( "%%interpreter%%" );

    /**
     * Symbol is bound to an error stream.  Available in static initialization.
     */
    public final static Symbol INIT_ERR_STREAM =
            Symbol.createObject( "%%errOut%%" );

    /**
     * This is the relative path to our extensions package.  This is needed
     * for the method Class.getResourceAsStream() which is used for addressing
     * resources and which uses path addressing relative to it package.
     */
    private final static String schemeExtensionPosition = "extensions/";
    private final static String schemeTestPosition = "test/";

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
     * @see de.michab.scream.SchemeInterpreter#kernelSchemeInstanceExtensionsP
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

    /**
     *
     */
//    private final static String KEY_SCREAM_SHELL = "scream.shell";

    @Resource
    private static String shell;

    /**
     * This is the name of the method that is looked up on the classes that are
     * added via the "kernel.integrateClasses" property.
     */
    private final static String INIT_NAME = "extendTopLevelEnvironment";

    /**
     * The parser used by this interpreter.
     */
    private final SchemeReader _parser;

    /**
     * The evaluator used by this interpreter.
     */
    private final SchemeEvaluator _evaluator;

    /**
     * The standard input port for this interpreter instance.
     *
     * @see de.michab.scream.SchemeInterpreter#getInPort
     */
    private final Port _inPort;

    /**
     * The standard output port for this interpreter instance.
     *
     * @see de.michab.scream.SchemeInterpreter#getOutPort
     */
    private final Port _outPort;

    /**
     * The standard common error port for the system.
     *
     * @see de.michab.scream.SchemeInterpreter#getErrorPort
     * @see de.michab.scream.SchemeInterpreter#_errorWriter
     */
    private static Port _errorPort;

    /**
     * @see de.michab.scream.SchemeInterpreter#getErrorPort
     * @see de.michab.scream.SchemeInterpreter#_errorPort
     */
    private static PrintWriter _errorWriter;

    /**
     * Creates an instance of an working Scheme interpreter.  Since the
     * interpreter is multithreaded internally, at some unexpected point in time
     * it starts reading on the provided Reader and writing onto the Writer.
     *
     * @param in The reader to use.
     * @param out The writer to use by this interpreter.
     */
    private SchemeInterpreter( Reader in, Writer out )
    {
        _localTle = _topLevelEnvironment.extend(
                Symbol.createObject( "interaction" ) );

        // Now do the per instance initialization of this environment.
        _inPort = new Port( "standard-input", in );
        _outPort = new Port( "standard-output", out );

        _localTle.set( ANCHOR_SYMBOL, new SchemeObject( this ) );

        processExtensions(
                _localTle,
                schemeInstanceExtensions,
                schemeExtensionPosition,
                _errorWriter );
        _errorWriter.flush();

        // Init the parser...
        _parser = new SchemeReader( in );

        // Init the evaluator...
        _evaluator = new SchemeEvaluator(
                _localTle,
                _parser,
                out );

        // Start up.
        new Thread( _evaluator ).start();
    }

    /**
     * Factory method.  Creates interpreters linked to the passed in and out
     * streams.
     *
     * @param in The input stream to be used by this interpreter instance.  This
     *           stream has to deliver boot code.
     * @param out An output stream to be used by the Scheme IO procedures.
     * @return A newly constructed interpreter.
     * @see de.michab.scream.SchemeInterpreter#initialise
     */
    public static SchemeInterpreter createSchemeInterpreter(
            Reader in,
            Writer out )
    {
        if ( ! _initialised )
            initialise( new PrintWriter( System.err ), false );

        return new SchemeInterpreter( in, out );
    }

    /**
     * Get the standard input port for this interpreter instance.
     *
     * @return This interpreter's standard in.
     */
    public Port getInPort()
    {
        return _inPort;
    }

    /**
     * Get the standard output port for this interpreter instance.
     *
     * @return This interpreter's standard out.
     */
    public Port getOutPort()
    {
        return _outPort;
    }

    /**
     * Get the common error port for the system.  This is normally a reference to
     * the System.err stream.  To redefine the error port the initialize() method
     * has to be called before any other operation on the SchemeInterpreter
     * class.
     *
     * @return This interpreter's standard error port.
     * @see de.michab.scream.SchemeInterpreter#initialise
     */
    static public Port getErrorPort()
    {
        return _errorPort;
    }

    /**
     * Return a reference to this interpreter's top level environment.
     *
     * @return This interpreter's top level environment.
     */
    public Environment getTopLevelEnvironment()
    {
        return _localTle;
    }

//    /**
//     * Return all the bindings whose name starts with the passed prefix from the
//     * interpreter's top level environment.
//     *
//     * @param prefix A prefix used for subsetting the returned list.
//     * @return A sorted list of symbol names.
//     */
//    public String[] complete( String prefix )
//    {
//        Set<String> collector = new HashSet<String>();
//
//        SchemeScanner preCompleter = new SchemeScanner( new java.io.StringReader( prefix ) );
//
//        // This is the last token before EOF.
//        Token lastToken = null;;
//
//        // Compute that last token.
//        try
//        {
//            for ( Token t = preCompleter.getNextToken() ;
//                    t.getType() != SchemeParser.TkEof ;
//                    t = preCompleter.getNextToken() )
//            {
//                // Put the symbols used in the prefix into our set of elements to
//                // filter.
//                if ( t.getType() == SchemeParser.TkSymbol )
//                {
//                    String tokenName = t.stringValue();
//                    collector.add( tokenName );
//                }
//
//                lastToken = t;
//            }
//        }
//        catch ( Exception e )
//        {
//            return new String[0];
//        }
//
//        // Check if there was a token at all and whether this was a Symbol.
//        if ( lastToken == null || lastToken.getType() != SchemeParser.TkSymbol )
//            return new String[0];
//
//        // Create the prefix we are trying to complete.  If prefix was
//        // "(define (lumumba x) (len" on entry to that method, the prefix will now
//        // be set to "len".
//        prefix = lastToken.stringValue();
//
//        // Put
//        Symbol[] symbols = getTopLevelEnvironment().getDefinedSymbols();
//        for ( int i = 0 ; i < symbols.length ; i++ )
//            collector.add( symbols[i].toString() );
//
//        return filterSymbols( prefix, collector );
//    }

    /**
     * Selects all the keys starting with the passed <code>prefix</code> from the
     * passed hashtable.  Implements parts of the <code>complete()</code>
     * functionality.
     *
     * @param prefix The prefix string used for filtering.
     * @param collector The <code>Hashtable</code> containing the strings for
     *         filtering as <code>key</code>s.  The <code>value</code>s are not
     *         taken into account.
     * @return A sorted array holding the filtered strings.
     */
    private String[] filterSymbols( String prefix, Set<String> collector )
    {
        java.util.Vector<String> setOfGood =
                new java.util.Vector<String>( collector.size() );

        Iterator<String> e = collector.iterator();

        while ( e.hasNext() )
        {
            String currentKey = e.next();
            if ( currentKey.startsWith( prefix ) && ! currentKey.equals( prefix ) )
                setOfGood.add( currentKey );
        }

        String[] result = new String[ setOfGood.size() ];
        result = setOfGood.toArray( result );

        // Finally sort the returned array.
        Arrays.sort( result );

        return result;
    }

    /**
     * Loads all classes defined in the kernel.integrateClasses property and
     * invokes the following method signature on the class:<br>
     *  <code>public static Environment extendTopLevelEnvironment( Environment tle )</code><br>
     *
     * @param tle The top level environment to contain the new bindings.
     */
    private static void integrateClasses(
            Environment tle )
    {
        for ( var crtClassName : integrateClasses )
        {
            log.info( "Initializing: " + crtClassName );

            try
            {
                Class<?> clazz = Class.forName( crtClassName );
                java.lang.reflect.Method init = clazz.getDeclaredMethod(
                        INIT_NAME,
                        new Class[]{ tle.getClass() } );
                init.invoke( null, new Object[]{ tle } );
            }
            catch ( ClassNotFoundException e )
            {
                log.log(
                        Level.WARNING,
                        "Class for initialization not found: ''{0}''",
                        crtClassName );
            }
            catch ( NoSuchMethodException e )
            {
                log.fine(
                        "No init needed for class '" +
                                crtClassName +
                        "'." );
            }
            catch ( IllegalAccessException e )
            {
                log.log(
                        Level.WARNING,
                        "Illegal access: ''{0}''",
                        crtClassName );
            }
            catch ( java.lang.reflect.InvocationTargetException e )
            {
                log.log(
                        Level.WARNING,
                        "Init threw exception.",
                        e.getCause() );
            }
        }
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
    private static void processExtensions(
            Environment env,
            String[] fileNames,
            String packageName,
            PrintWriter err )
    {
//        // Get the base property...
//        String schemeFiles =
//                Localiser.localise( _screamResources, propertyName );
//        // ...and check if there is valid contents.
//        if ( null == schemeFiles || 0 == schemeFiles.length() )
//            // No?  Then we're ready.
//            return;

//        String[] fileNames = schemeFiles.split( "\\s" );

        // Init the parser...
        SchemeReader sreader = new SchemeReader();

        // Init the evaluator...
        SchemeEvaluator sevaluator = new SchemeEvaluator(
                env,
                sreader,
                err );

        for ( int i = fileNames.length -1 ; i >= 0 ; i-- )
        {
            // The getResourceAsStream in the next line addresses resources relative
            // to the classes package.  So here we create a name like
            // extensions/foo.s.
            String crtFileName = packageName + fileNames[i];
            log.info( "Processing: " + crtFileName );

            // Try to get a stream on the file...
            InputStream is =
                    SchemeInterpreter.class.getResourceAsStream( crtFileName );
            // ...and check if we had success.
            if ( is == null )
            {
                log.log(
                        Level.WARNING,
                        "File for processing not found: ''{0}''",
                        crtFileName );
            }
            else
            {
                // We have a stream...
                Reader isr = new InputStreamReader( is, StandardCharsets.UTF_8 );
                // ...and load its contents.
                sreader.push( isr );
            }
        }

        sevaluator.run();
    }



    /**
     * Initialize the class.  This is automatically done when creating new
     * interpreter instances.  This method is of use if a special writer should
     * be used for error reporting while reading the runtime system definitions.
     * @see SchemeInterpreter#createSchemeInterpreter
     */
    private static void initialise( PrintWriter initWriter, boolean test )
    {
        // Guard against repeated initialization.
        if ( _initialised )
            return;

        _initialised = true;

        _topLevelEnvironment =
                new Environment();
        _errorWriter =
                initWriter;
        _errorPort =
                new Port( "standard-error", _errorWriter );
        _topLevelEnvironment.set( INIT_ERR_STREAM, _errorPort );

        // Initialize java runtime system.
        integrateClasses( _topLevelEnvironment );

        // Load extensions defined in scheme source files.
        processExtensions(
                _topLevelEnvironment,
                schemeExtensions,
                schemeExtensionPosition,
                initWriter );

        if ( test )
            processExtensions(
                    _topLevelEnvironment.extend( Symbol.createObject( "test-environment" ) ),
                    regression,
                    schemeTestPosition,
                    initWriter );

        initWriter.flush();
    }

    /**
     * Loads the scheme source file in the port into the passed environment.  The
     * port is closed before the file's contents is evaluated.
     *
     * @param filename The name of the file to load.
     * @throws RuntimeX In case of errors.
     */
    public void load( String filename )
            throws RuntimeX
    {
        try
        {
            Reader reader =
                    new FileReader( filename );

            _parser.push( reader );
        }
        catch ( IOException e )
        {
            throw new RuntimeX( "IO_ERROR",
                    new Object[]{ e.getMessage() } );
        }
    }



    /**
     * Reads the resources.  Terminates the application if any problem occurs.
     * This represents the very first phase of boot up.
     */
    static
    {
//        _screamResources = Localiser.loadResourceBundle( screamProperties );
//
//        if ( _screamResources == null )
//        {
//            log.severe( "Resources not found: " + screamProperties );
//            System.exit( 1 );
//        }
    }



//    /**
//     * Reads the shell property key from the properties file and returns a ready
//     * made Reader object made from that.
//     */
//    private static Reader getShellReader( String extensionName )
//    {
//        // The getResourceAsStream in the next line addresses resources relative
//        // to the classes package.  So here we create name like extensions/foo.s
//        String crtFileName = schemeExtensionPosition + extensionName;
//
//        // Try to get a stream on the file...
//        InputStream is =
//                SchemeInterpreter.class.getResourceAsStream( crtFileName );
//        // ...and check if we had success.
//        if ( is == null )
//            return null;
//
//        return new InputStreamReader( is );
//    }



    /**
     * Checks whether the passed parameter is passed on the command line.
     *
     * @param parameter The name of the parameter to check.  Note that only the
     *                  name of the parameter has to be passed.  If looking for
     *                  parameter '-arg', then only 'arg' has to be passed.  The
     *                  operation allows two different parameter notations: The
     *                  parameter 'arg' is found as either '/arg' or '-arg'.
     * @param commandLine The command line to check.
     * @return
     */
    private static boolean parameterExists(
            String parameter,
            String[] commandLine )
    {
        String regex = "[-/]" + parameter;
        for ( String arg : commandLine )
        {
            if ( arg.matches( regex ) )
                return true;
        }

        return false;
    }

//    /**
//     * Removes parameters from the passed string array.  E.g. if an array of
//     * {"-arg", "michael", "-param", "poop.s" } is passed, then the array
//     * {"michael", "poop.s"} is returned.  Parameters can start with '-' or
//     * '/'.
//     *
//     * @param argv The original parameters.
//     * @return The filtered parameter list.
//     */
//    private static String[] removeParameters( String argv[] )
//    {
//        ArrayList<String> result = new ArrayList<String>();
//
//        for ( String c : argv )
//        {
//            if ( ! c.matches( "[/-].*" ) )
//                result.add( c );
//        }
//
//        return result.toArray( new String[result.size()] );
//    }

    /**
     * Is responsible for triggering the startup phase based on command line and
     * property configuration.  Implements the decision strategy for system
     * bootstrap.  And is on the other hand the place where all fun starts...
     */
    public static void main( String[] argv )
    {
        org.smack.util.ServiceManager.getApplicationService(
                org.smack.util.resource.ResourceManager.class )
        .injectResources( SchemeInterpreter.class );

        // Give the init thread a name for debug purposes.
        Thread.currentThread().setName( "screamMain" );

        // Check if "-test" is specified on the command line and perform regression
        // test if yes.
        initialise(
                new PrintWriter( System.err ),
                parameterExists( "test", argv ) );

        //    boolean stdIoMode = true; // parameterExists( "cli", argv );
        //
        //    argv = removeParameters( argv );
        //
        //    String shell = Localiser.localise( _screamResources, KEY_SCREAM_SHELL );
        //
        //    if ( stdIoMode )
        //    {
        startStandardIoMode();
        //    }
        //    // Scheme files specified on the command line.
        //    else if ( argv.length >= 1 )
        //    {
        //      log.fine( "Reading command line file: " + argv[0] );
        //      try
        //      {
        //        createSchemeInterpreter( new FileReader( argv[0] ),
        //                                 new PrintWriter( System.err ) );
        //      }
        //      catch ( FileNotFoundException e )
        //      {
        //        log.warning( "File not found: " + argv[0] );
        //      }
        //    }
        //    // Second comes the shell property key definition.
        //    else if ( shell != null && shell.length() > 0 )
        //    {
        //      log.fine( "Reading shell pk: " + shell );
        //      createSchemeInterpreter(
        //          getShellReader( shell ),
        //          new PrintWriter( System.err ) );
        //    }
        //    // No shell key set.  We go down...
        //    else
        //    {
        //      log.severe(
        //        "Property key <" +
        //        KEY_SCREAM_SHELL  +
        //        "> not set." );
        //      System.exit( 1 );
        //    }
    }

    /**
     * Starts Scream in standard IO mode, that is, only the standard IO streams
     * are used for input and output.
     */
    private static void startStandardIoMode()
    {
        try
        {
            // This throws an IO exception, if the VM is running in background.
            // That is tested with 1.2.2, 1.3, 1.4 beta on NT4.0
            System.in.available();
            createSchemeInterpreter(
                    new InputStreamReader( System.in ),
                    new PrintWriter( System.out ) );
        }
        catch ( IOException e )
        {
            System.exit( 1 );
        }
    }

    /**
     * (eval <expression>)
     *
     * Currently the environment arguments are not supported.
     */
    static private Procedure evalProcedure = new Procedure( "eval" )
    {
        private Class<?>[] formalArglist =
                new Class[]{ FirstClassObject.class, Environment.class };

        @Override
        public FirstClassObject apply( Environment parent, FirstClassObject[] args )
                throws RuntimeX
        {
            checkArguments( formalArglist, args );

            // Do it.
            return evaluate( args[0], (Environment)args[1] );
        }
    };

    /**
     * (scheme-report-environment)
     *
     * Currently the environment arguments are not supported.
     */
    static private Procedure tleProcedure = new Procedure( "scheme-report-environment" )
    {
        private Class<?>[] formalArglist =
                new Class[]{ SchemeInteger.class };

        @Override
        public FirstClassObject apply( Environment parent, FirstClassObject[] args )
                throws RuntimeX
        {
            checkArguments( formalArglist, args );

            // Do it.
            return _topLevelEnvironment;
        }
    };

    /**
     * Environment operations setup.
     *
     * @param tle A reference to the system private top level environment.
     * @return A reference to the environment including the additional entries
     *        defined by this class.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( evalProcedure );
        tle.setPrimitive( tleProcedure );

        return tle;
    }
}
