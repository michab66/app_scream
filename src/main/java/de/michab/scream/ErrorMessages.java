/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.smack.util.JavaUtil;
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
}
