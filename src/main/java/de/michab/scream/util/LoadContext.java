/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2022-2023 Michael G. Binz
 */

package de.michab.scream.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * Support for file loading.  Implements the necessary logic to load files from
 * the filesystem, inside a jar file or from the internet.  Supports loading of
 * several files that are rooted to a single source.  That means if the first
 * file is rooted in a jar file then follow-up files with a relative name are
 * loaded from the same source.
 * <p>
 * See // https://flylib.com/books/en/1.134.1/jarurlconnection.html for the
 * basic idea.
 *
 * @author micbinz
 */
public class LoadContext
{
    private final String _prefix;
    private final File _file;
    private final boolean _isFile;

    private final static String FILE_PROTOCOL = "file";
    private final static String JAR_PROTOCOL = "jar";
    private final static String EXCLAMATION = "!";

    private static URL nonThrowingUrl( String name )
    {
        try
        {
            return new URL( name );
        }
        catch ( MalformedURLException ignore )
        {
        }

        try
        {
            return new URL( FILE_PROTOCOL + ":" + name );
        }
        catch ( MalformedURLException ignore )
        {
        }

        throw new IllegalArgumentException( name );
    }

    public LoadContext( String filename ) {
        this( nonThrowingUrl( filename ) );
    }

    public LoadContext( URL filename )
    {
        if ( FILE_PROTOCOL.equals( filename.getProtocol() ) )
        {
            _prefix = FILE_PROTOCOL + ":";
            _file = new File( filename.getPath() );
            _isFile = true;
            return;
        }
        if ( JAR_PROTOCOL.equals( filename.getProtocol() ) )
        {
            var split = filename.toExternalForm().split( EXCLAMATION );
            _prefix = split[0] + EXCLAMATION;
            _file = new File( split[1] );
            _isFile = false;
            return;
        }

        throw new IllegalArgumentException( filename.toExternalForm() );
    }

    private LoadContext( String prefixx, File file, boolean isFile )
    {
        _prefix = prefixx;
        _file = file;
        _isFile = isFile;
    }

    File getFile()
    {
        return _file;
    }

    public boolean isAbsolute()
    {
        return _file.getPath().startsWith( File.separator );
    }

    public boolean hasParent()
    {
        return _file.getParent() != null;
    }

    public LoadContext relate( LoadContext prev )
    {
        var f = isAbsolute() ?
                getFile() :
                new File( prev._file.getParentFile(), _file.getName() );

        return new LoadContext(
                prev._prefix,
                f,
                prev._isFile );
    }

    public InputStream getStream() throws IOException
    {
        if ( _isFile )
            return new FileInputStream( _file );

        return new URL( toString() ).openStream();
    }

    private URL toUrl()
    {
        var asString =
                _prefix + _file;
        return nonThrowingUrl(
                asString.replace( '\\', '/' ) );
    }

    @Override
    public String toString()
    {
        return toUrl().toExternalForm();
    }

    /**
     * @return {@code true} if this represents a file, otherwise
     * this is an URL.
     */
    public boolean isFile()
    {
        return _isFile;
    }
}
