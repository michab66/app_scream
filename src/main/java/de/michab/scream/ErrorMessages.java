/*
 * usbview @ https://github.com/michab66/usbview
 *
 * Copyright © 2018-2022 Michael Binz
 */
package de.michab.scream;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.smack.util.JavaUtil;
import org.smack.util.StringUtil;
import org.smack.util.resource.ResourceMap;

/**
 * The error messages.
 */
class ErrorMessages
{
    /**
     * The raw vendor id to vendor name mappings.
     */
    public final static Map<String, String> map = JavaUtil.make( () -> {
        ResourceMap rm = ResourceMap.getResourceMap( ErrorMessages.class );

        if ( rm == null )
            throw new AssertionError( "No resources for " + ErrorMessages.class.getName() );

        HashMap<String, String> n = new HashMap<>();

        for ( var c : rm.keySet() )
        {
            try
            {
                n.put( c, rm.get( c ) );
            }
            catch ( NumberFormatException e )
            {
                // Skip qualified names.
                continue;
            }
        }

        return Collections.unmodifiableMap( n );
    });

    /**
     * @param id The id to resolve.
     * @return A resolved vendor id in the form of 'Vendor (0x9999)'.  If the
     * id is not known, then 'Unknown' is returned in place of 'Vendor' above.
     */
    public final static String _resolve( String id )
    {
        var name = map.get( id );

        if ( StringUtil.isEmpty( name ) )
            name = "Unknown";

        return String.format( "%s (0x%04x)", name, id );
    }
}
