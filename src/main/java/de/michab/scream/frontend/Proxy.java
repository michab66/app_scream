/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2024 Michael G. Binz
 */
package de.michab.scream.frontend;

import java.util.Objects;

import org.smack.util.Holder;

import de.michab.scream.fcos.FirstClassObject;

class Proxy extends FirstClassObject
{
    /**
     * The name of the type.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "proxy";

    private final Holder<FirstClassObject> _fcoHolder;

    private final long _labelNumber;

    Proxy( long labelNumber, Holder<FirstClassObject> fcoHolder )
    {
        _fcoHolder = Objects.requireNonNull( fcoHolder );
        _labelNumber = labelNumber;
    }

    public FirstClassObject value()
    {
        return _fcoHolder.get();
    }

    @Override
    public String toString()
    {
        return String.format( "%s:%d=%s", TYPE_NAME, _labelNumber, _fcoHolder );
    }
}
