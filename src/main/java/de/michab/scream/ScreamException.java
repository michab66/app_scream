/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;

import org.smack.util.JavaUtil;
import org.smack.util.StringUtil;

import de.michab.scream.fcos.Symbol;

/**
 * The base class for all Scream-specific exceptions.  The message that this
 * exception receives at instantiation time is used as a key into Scream's
 * ErrorMessages resource bundle.  Note that a suffix specifying the number of
 * parameters for the exception will be added to the key at resolution time.
 * For example if a runtime exception is created with the key "ERROR" and two
 * arguments are provided then resolution of key "ERROR_2" is tried in the
 * resource bundle.
 *
 * <p>The entries in the resource bundle have to follow the format
 * {@code ERROR_KEY<_NUM_OF_ARGS> = <error-number> : <ERROR_MESSAGE>}.
 * In case the passed message is not found in the resource bundle, the original
 * message is printed with a question mark prepended.
 *
 * <p>One of the design goals of this exception is to be lightweight in the
 * sense that instantiation does not imply high overhead.  Accesses to the
 * resource bundle occur only if either the message itself or the numeric
 * message id are requested from a {@code RuntimeX} instance.
 *
 * @author Michael Binz
 */
@SuppressWarnings("serial")
public class ScreamException
extends Exception
{
    public enum Code
    {
        INTERNAL_ERROR,
        NOT_IMPLEMENTED,
        SYMBOL_NOT_DEFINED,
        SYMBOL_NOT_ASSIGNABLE,
        TOO_MANY_SUBEXPRESSIONS,
        SYNTAX_ERROR,
        DEFINE_ERROR,
        EXPECTED_PROPER_LIST,
        INDEX_OUT_OF_BOUNDS,
        CALLED_NON_PROCEDURAL,
        INVALID_ASSOC_LIST,
        CAR_FAILED,
        TYPE_ERROR,
        NOT_ENOUGH_ARGUMENTS,
        TOO_MANY_ARGUMENTS,
        WRONG_NUMBER_OF_ARGUMENTS,
        REQUIRES_EQUIVALENT_CONS_LEN,
        BAD_BINDING,
        BAD_CLAUSE,
        DIVISION_BY_ZERO,
        PORT_CLOSED,
        EXPECTED_INPUT_PORT,
        EXPECTED_OUTPUT_PORT,
        IO_ERROR,
        DUPLICATE_FORMAL,
        INVALID_FORMALS,
        CLASS_NOT_FOUND,
        FIELD_NOT_FOUND,
        METHOD_NOT_FOUND,
        ILLEGAL_ACCESS,
        INVOCATION_EXCEPTION,
        CANT_ACCESS_INSTANCE,
        CREATION_FAILED,
        ILLEGAL_ARGUMENT,
        SCAN_UNBALANCED_QUOTE,
        SCAN_UNEXPECTED_CHAR,
        PARSE_EXPECTED,
        PARSE_UNEXPECTED_EOF,
        PARSE_UNEXPECTED,
        INTERRUPTED,
        CANT_MODIFY_CONSTANT,
        NO_PROXY,
        PROXY_CANNOT_INSTANTIATE,
        TEST_FAILED,
        ONLY_IN_QUASIQUOTE_CONTEXT,
        RADIX_NOT_SUPPORTED,
        DUPLICATE_ELEMENT;


        public int id()
        {
            return ordinal() -1;
        }
    }

    private static final Map<String,Code> nameToCode = JavaUtil.make(
            () -> {
                var result = new HashMap<String, Code>();

                for ( var c : Code.values() )
                    result.put( c.toString(), c );
                return result;
            });

    private static Code getCode( String name )
    {
        var result = nameToCode.get( name );

        if ( result != null )
            return result;

        throw new IllegalArgumentException( "Unknown ScreamException name='" + name + "'" );
    }

    private final Code _code;

    /**
     * Delimits the error id from the message.
     */
    private final static char ID_DELIMITER = ':';

    /**
     * The actual error message.  Initialized only by an access to the message
     * accessor or error id.
     */
    private String _errorMessage = null;

    /**
     * The error id.  Initialized only by an access to the message accessor or
     * error id.  -2 initial value is returned if error id resolution failed.
     */
    private int _errorId = -2;

    /**
     * Specifies whether the error message and id have been initialized from
     * the resource bundle file.
     */
    private boolean _lazyInitDone = false;

    /**
     * A reference to the arguments to be formatted into the error message.
     */
    private final Object[] _errorArguments;

    /**
     * The name of the operation where the exception occurred.
     */
    private Symbol _operationName = null;

    /**
     * Sets the name of the throwing operation.  Note that this is only set once
     * and locked.  Further calls to set do <i>not</i> change the operation name
     * set by the first call to this method.
     *
     * @param operation The name of the throwing operation.
     * @see ScreamException#getOperationName
     */
    public void setOperationName( Symbol operation )
    {
        if ( _operationName == null )
            _operationName = operation;
    }

    /**
     * Read the symbolic name of the operation that reported the exception.
     *
     * @return The symbolic name of the operation that reported the exception.
     * @see ScreamException#setOperationName
     */
    public Symbol getOperationName()
    {
        return _operationName;
    }

    public Object[] getArguments()
    {
        return _errorArguments;
    }
    public Object getArgument( int idx )
    {
        return _errorArguments[idx];
    }

    /**
     * Get a numeric error id.  In case there was a misconfiguration in the
     * resource bundle -2 is returned.
     *
     * @return The numeric error id.
     */
    public int getId()
    {
        if ( ! _lazyInitDone )
        {
            _lazyInitDone = true;
            initialise();
        }

        return _errorId;
    }

    public Code getCode()
    {
        return _code;
    }

    /**
     * Get the message from this exception.
     *
     * @return This exception's message.
     */
    @Override
    public String getMessage()
    {
        if ( ! _lazyInitDone )
        {
            _lazyInitDone = true;
            initialise();
        }

        return _errorMessage;
    }

    /**
     * Initialize the _errorMessage and _errorId attributes.
     */
    private void initialise()
    {
        // Get the original message.  Note that this can neither be null nor
        // the empty string.  See invariant in constructor.
        String message = super.getMessage();
        // Append the number of arguments to the message key.
        if ( _errorArguments != null  && _errorArguments.length > 0 )
            message += "_" + _errorArguments.length;

        message = ErrorMessages.map.get( message );
        // Use the message string as the resource key to get the ultimate message.
        //        message = Localiser.localise( _errorBundle, message, message );
        //        if ( ! message.equals( tmp ) )
        //            System.out.println( "warn scream x" );

        // Return the message with the arguments formatted in.
        _errorMessage = MessageFormat.format( message, _errorArguments );


        // Now get the error id from the message.
        int colonIdx = _errorMessage.indexOf( ID_DELIMITER );
        if ( colonIdx == -1 )
            // No error id specified.
            return;

        try
        {
            _errorId = Integer.parseInt(
                    _errorMessage.substring( 0, colonIdx ).trim() );
        }
        catch ( NumberFormatException e )
        {
            // Do nothing.  _errorId defaults to error code.
        }
    }

    /**
     * Creates a scream exception.
     *
     * @param msg The message that will be used as a key into Scream's error
     * message resource bundle.
     * @param args The arguments to be formatted into the message.
     * @exception IllegalArgumentException In case the passed message was
     * either @{code null} or the empty string.
     */
    @Deprecated
    protected ScreamException( String msg, Object ... args )
    {
        super( msg );

        // Ensure that we received a valid non-null and non-empty message.
        if ( StringUtil.isEmpty( msg ) )
            throw new IllegalArgumentException( "ScreamException: Invalid message." );

        _code =
                getCode( msg );
        _errorArguments =
                args;
    }

    protected ScreamException( Code c )
    {
        this( c.toString() );
    }

    protected ScreamException( Code c, Object ... arguments )
    {
        this( c.toString(), arguments );
    }
}
