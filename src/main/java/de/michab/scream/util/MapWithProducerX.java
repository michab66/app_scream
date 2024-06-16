/*
 * Smack Java @ https://github.com/smacklib/dev_smack
 *
 * Copyright © 2018-2023 Michael G. Binz
 */
package de.michab.scream.util;

import java.util.AbstractMap;
import java.util.HashMap;
import java.util.Objects;
import java.util.function.Supplier;

import org.smack.util.FunctionalUtil.FunctionX;

/**
 * A map that produces content on demand using a factory.
 *
 * @author Michael Binz
 */
public class MapWithProducerX<K,V, X extends Exception>
{
    private final AbstractMap<K,V> _cache;

    private final FunctionX<K, V, X> _factory;

    /**
     * Create an instance.
     *
     * @param m The hashmap supplier, for example
     * {@code java.util.WeakHashMap<K,V>::new}.
     *
     * @param factory A content factory.
     */
    public MapWithProducerX(
            Supplier<AbstractMap<K,V>> m,
            FunctionX<K, V, X> factory )
    {
        _cache =
                m.get();
        _factory =
                factory;
    }

    /**
     * Create an instance based on a java.util.HashMap.
     *
     * @param factory A content factory.
     */
    public MapWithProducerX(
            FunctionX<K,V,X> factory )
    {
        this( HashMap<K,V>::new , factory );
    }

    /**
     * Get a value.  If the value is not yet contained, create it.
     *
     * @param key The key.
     * @return The value for the key.
     * @throws X An exception from the factory.
     */
    public V get( K key ) throws X
    {
        var result = _cache.get( key );

        if ( result != null )
            return result;

        result = _factory.apply( key );

        _cache.put(
                key,
                Objects.requireNonNull( result, "null not allowed." ) );

        return result;
    }
}
