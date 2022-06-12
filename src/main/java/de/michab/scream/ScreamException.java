/* $Id: ScreamException.java 788 2015-01-10 23:07:11Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2009 Michael G. Binz
 */
package de.michab.scream;

import java.text.MessageFormat;

/**
 * <p>The base class for all scream specific exceptions.  The message that this
 * exception receives at instantiation time is used as a key into Scream's
 * ErrorMessages resource bundle.  Note that a suffix specifying the number of
 * parameters for the exception will be added to the key at resolution time.
 * For example if a runtime exception is created with the key "ERROR" and two
 * arguments are provided then resolution of key "ERROR_2" is tried in the
 * resource bundle.</p>
 *
 * <p>The entries in the resource bundle have to follow the format <code>
 * ERROR_KEY<_NUM_OF_ARGS> = <error-number> : <ERROR_MESSAGE></code><br>
 * In case the passed message is not found in the resource bundle, the original
 * message is printed with a question mark prepended.</p>
 *
 * <p>One of the design goals of this exception is to be lightweight in the
 * sense that instantiation does not imply high overhead.  Accesses to the
 * resource bundle occur only if either the message itself or the numeric
 * message id are requested from a RuntimeX instance.</p>
 *
 * @author Michael Binz
 */
@SuppressWarnings("serial")
public class ScreamException
    extends java.lang.Exception
{
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
     * A reference to the resource bundle used to localize error messages.
     */
    //private static ResourceBundle _errorBundle = null;



    /**
     * A reference to the arguments to be formatted into the error message.
     */
    private final Object[] _errorArguments;



    /**
     * The name of the operation where the exception occurred.
     */
    private Symbol _operationName = null;



    //    /**
    //     * Load the resource bundle.
    //     */
    //    static
    //    {
    //        try
    //        {
    //            _errorBundle =
    //                    ResourceBundle.getBundle( "de.michab.scream.ErrorMessages" );
    //        }
    //        catch ( MissingResourceException e )
    //        {
    //            _log.severe( "Resource not found: " + e.getMessage() );
    //            System.exit( 1 );
    //        }
    //    }
    //


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
     * Has the sole purpose to initialize the _errorMessage and _errorId
     * attributes.
     */
    private void initialise()
    {
        // Get the original message.  Note that this can neither be null nor the
        // empty string.  See invariant in constructor.
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
     *        message resource bundle.
     */
    public ScreamException( String msg )
    {
        this( msg, null );
    }



    /**
     * Creates a scream exception.
     *
     * @param msg The message that will be used as a key into Scream's error
     *        message resource bundle.
     * @param args The arguments to be formatted into the message.
     * @exception IllegalArgumentException In case the passed message was either
     *            <code>null</code> or the empty string.
     */
    public ScreamException( String msg, Object[] args )
    {
        super( msg );

        // Ensure that we received a valid non-null and non-empty message.
        if ( msg == null || msg.length() == 0 )
            throw new IllegalArgumentException( "ScreamException: Invalid message." );

        _errorArguments = args;
    }
}
