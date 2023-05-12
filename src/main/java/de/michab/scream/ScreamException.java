/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import java.text.MessageFormat;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.smack.util.CachedHolder;
import org.smack.util.JavaUtil;

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
        CANNOT_ACCESS_INSTANCE,
        CREATION_FAILED,
        ILLEGAL_ARGUMENT,
        SCAN_UNBALANCED_QUOTE,
        SCAN_UNEXPECTED_CHAR,
        ERROR,
        PARSE_EXPECTED,
        PARSE_UNEXPECTED_EOF,
        PARSE_UNEXPECTED,
        INTERRUPTED,
        CANNOT_MODIFY_CONSTANT,
        NO_PROXY,
        PROXY_CANNOT_INSTANTIATE,
        TEST_FAILED,
        ONLY_IN_QUASIQUOTE_CONTEXT,
        RADIX_NOT_SUPPORTED,
        DUPLICATE_ELEMENT,
        EXPECTED_BINARY_PORT,
        EXPECTED_TEXTUAL_PORT,
        SCAN_UNBALANCED_COMMENT,
        RANGE_EXCEEDED;

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
                return Collections.unmodifiableMap( result );
            });

    private final CachedHolder<String> _message =
            new CachedHolder<String>( this::makeMessage );

    /**
     * Creates a scream exception.
     *
     * @param msg The message that will be used as a key into Scream's error
     * message resource bundle. This must be neither null nor a blank string.
     * @param args The arguments to be formatted into the message.
     * @throws IllegalArgumentException In case the passed message was blank.
     * @throws NullPointerException In case the passed message was
     * @{code null}.
     */
    @Deprecated
    ScreamException( String msg, Object ... args )
    {
        super( Objects.requireNonNull( msg ) );

        // Ensure that we received a non-empty message.
        if ( msg.isBlank() )
            throw new IllegalArgumentException( "Empty message." );

        _code =
                getCode( msg );
        _errorArguments =
                args == null ?
                    new String[0] :
                    args;
    }

    ScreamException( Code c, Object ... args )
    {
        super( Objects.requireNonNull( c ).toString() );
        _code = c;
        _errorArguments =
                args == null ?
                    new String[0] :
                    args;
    }

    /**
     * Get an error code for the passed name.
     *
     * @param name An error name.
     * @return If the name does not match one of the well-defined codes a default code of
     * Code.ERROR is returned, otherwise the respective error code.
     */
    private static Code getCode( String name )
    {
        var result = nameToCode.get( name );

        if ( result != null )
            return result;

        return Code.ERROR;
    }

    private final Code _code;

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
     * @return The numeric error id.
     */
    public int getId()
    {
        return _code.id();
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
        return _message.get();
    }

    private String makeMessage()
    {
        // Get the original message.  Note that this can neither be null nor
        // the empty string.  See invariant in constructor.
        String messageId = super.getMessage();

        if ( _errorArguments.length > 0 )
        {
            // Append the number of arguments to the message key.
            String messageKey =
                    messageId +
                    "_" +
                    _errorArguments.length;

            var result = ErrorMessages.map.get( messageKey );

            if ( result != null )
                return getId() + " : " + MessageFormat.format(
                    result,
                    _errorArguments );
        }

        StringBuilder result =
                ErrorMessages.map.containsKey( messageId ) ?
                        new StringBuilder( ErrorMessages.map.get( messageId ) ) :
                        new StringBuilder( messageId );

        for ( var c : _errorArguments )
        {
            result.append( " " );
            result.append( c );
        }

        return getId() + " : " + result.toString();
    }
}
