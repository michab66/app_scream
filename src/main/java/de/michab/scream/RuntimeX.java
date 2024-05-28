/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream;

import java.text.MessageFormat;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.smack.util.CachedHolder;

import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.SchemeString;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.util.ErrorMessages;

/**
 * An exception to be thrown at run-time of a Scheme program.
 *
 * The Exception message is set by the constructor to either
 * <p>
 *  * a Code element in stringified form. In this case the
 *  Code-member is the the passed code.
 * <p>
 *  * a string passed into the constructor. The code element
 *  is then set to {@link Code#ERROR}.
 * <p>
 * Message generation:
 * <p>
 * If error arguments are set, the number of error arguments
 * is appended to the message (e.g. MESSAGE_2) and the resulting
 * key is looked up in the error messages property file. If found
 * the message is resolved using MessageFormat#format using the message
 * arguments and returned.  If MessageFormat#format throws an exception
 * this is not catched.
 * <p>
 *
 * If the above did not result in a message, then a second lookup
 * into the error message property file is performed using the
 * plain message (e.g. MESSAGE).  If the key exists, then the
 * error arguments are appended as space-delimited string to the
 * resolved value and returned.
 * <p>
 *
 * If the above did not result in a message the plain error message
 * is used and the error arguments are appended as space-delimited
 * strings and returned.
 *
 * @author Michael Binz
 */
@SuppressWarnings("serial")
public class RuntimeX
    extends Exception
{
    /**
     * The message code.
     */
    private final Code _code;

    /**
     * A reference to the arguments passed in the constructors.
     * Kept as an array for efficient access.
     *
     * This is never {@code null}.
     */
    private final FirstClassObject[] _irritants;

    /**
     * The name of the operation that caused the exception.
     */
    private final Symbol _operationName;

    /**
     * Predefined error codes.
     */
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
        _UNUSED_,
        ONLY_IN_QUASIQUOTE_CONTEXT,
        RADIX_NOT_SUPPORTED,
        DUPLICATE_ELEMENT,
        EXPECTED_BINARY_PORT,
        EXPECTED_TEXTUAL_PORT,
        SCAN_UNBALANCED_COMMENT,
        RANGE_EXCEEDED,
        RAISE,
        NOT_CONTINUABLE;
    }

    private static final CachedHolder<Map<String,Code>>
    _nameToCode =
        new CachedHolder<Map<String,Code>>(
            () -> {
                var result = new HashMap<String, Code>();

                for ( var c : Code.values() )
                    result.put( c.toString(), c );
                return Collections.unmodifiableMap( result );
            }
    );

    /**
     * Creates a Scream exception.  Used by the {@code (error ...)} procedure.
     *
     * @param msg The message that will be used as a key into Scream's error
     * message properties. This must be neither null nor a blank string.
     * If this is not a stringified element of the Code-enumeration, the
     * Code-member is set to {@link Code#ERROR}.
     * @param args The arguments to be formatted into the message.
     * @throws IllegalArgumentException In case the passed message was blank.
     * @throws NullPointerException In case the passed message was
     * @{code null}.
     */
    public RuntimeX( SchemeString msg, FirstClassObject ... args  )
    {
        super( Objects.requireNonNull( msg.getValue() ) );

        var msgValue = msg.getValue();

        if ( msgValue.isBlank() )
            throw new IllegalArgumentException( "Empty message." );

        _operationName =
                Symbol.createObject( "unknown" );
        _code =
                getCode( msg.getValue() );
        _irritants =
                Objects.requireNonNull( args );
    }

    RuntimeX( Code c, FirstClassObject ... args )
    {
        super( Objects.requireNonNull( c ).toString() );

        _operationName =
                Symbol.createObject( "unknown" );
        _code =
                c;
        _irritants =
                Objects.requireNonNull( args );
    }

    public RuntimeX( Symbol function, Code c, FirstClassObject ... args )
    {
        super( Objects.requireNonNull( c ).toString() );

        _operationName =
                Objects.requireNonNull( function );
        _code =
                c;
        _irritants =
                Objects.requireNonNull( args );
    }

    private static String validateMessage( SchemeString ss )
    {
        var result = Objects.requireNonNull( ss ).getValue();

        if ( result.isBlank() )
            throw new IllegalArgumentException( "Empty message." );

        return result;
    }

    /**
     * Strategic target ctor.
     *
     * @param operation
     * @param msg
     * @param irritants
     */
    public RuntimeX(
            Symbol operation,
            SchemeString msg,
            FirstClassObject ... irritants )
    {
        super( validateMessage( msg ) );

        _operationName =
                Objects.requireNonNull( operation );
        _code =
                getCode( msg.getValue() );
        _irritants =
                Objects.requireNonNull( irritants );
    }

    private final CachedHolder<Cons> _cachedIrritants =
            new CachedHolder<>(
                    () -> { return Cons.create( getIrritantsImpl() ); } );

    private FirstClassObject[] getIrritantsImpl()
    {
        return _irritants;
    }

    /**
     * @return The list of irritants.
     */
    public Cons getIrritants()
    {
        return _cachedIrritants.get();
    }

    /**
     * @return The unformatted message as passed in the first constructor
     * argument. If the constructor was called with a {@link Code} then
     * the literal enumeration name is returned.
     */
    public String getRawMessage()
    {
        return super.getMessage();
    }

    public RuntimeX addCause( Throwable cause )
    {
        super.initCause( cause );
        return this;
    }

    /**
     * Get an error code for the passed name.
     *
     * @param name An error name.
     * @return If the name does not match one of the well-defined codes a
     * default code of Code.ERROR is returned, otherwise the respective
     * error code.
     */
    private static Code getCode( String name )
    {
        var result = _nameToCode.get().get( name );

        if ( result != null )
            return result;

        return Code.ERROR;
    }

    /**
     * @return The symbolic name of the operation that reported the exception.
     */
    public Symbol getOperationName()
    {
        return _operationName;
    }

    /**
     * @return Never {@code null}.
     */
    public FirstClassObject[] getArguments()
    {
        return _irritants;
    }
    public FirstClassObject getArgument( int idx )
    {
        return getArguments()[ idx ];
    }

    /**
     * @return The exception's code.
     */
    public Code getCode()
    {
        return _code;
    }

    private final CachedHolder<String> _message =
            new CachedHolder<String>( this::makeMessage );

    /**
     * @return This exception's message.
     */
    @Override
    public String getMessage()
    {
        return _message.get();
    }

    /**
     * @return A message. See {@link RuntimeX} class documentation.
     */
    private String makeMessage()
    {
        // Get the original message.  Note that this can neither be null nor
        // the empty string.  See invariant in constructor.
        String messageId = super.getMessage();

        StringBuilder result = new StringBuilder().
                append( getCode() ).
                append( " : " );

        if ( getOperationName() != null )
            result.append( getOperationName() ).append( " : " );

        Object[] fmtArgs = new String[_irritants.length];
        int i = 0;
        for ( var c : _irritants )
            fmtArgs[i++] = FirstClassObject.toString( c );

        final var messageKey = fmtArgs.length > 0 ?
                messageId + "_" + fmtArgs.length :
                messageId;

        if ( ErrorMessages.map.containsKey( messageKey ) )
        {
            return result.toString() + MessageFormat.format(
                    ErrorMessages.map.get( messageKey ),
                    fmtArgs );
        }

        result.append( messageId );

        for ( var c : fmtArgs )
        {
            result.append( " " );
            result.append( c );
        }

        return result.toString();
    }
}
