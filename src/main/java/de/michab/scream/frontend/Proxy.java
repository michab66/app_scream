/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2024 Michael G. Binz
 */
package de.michab.scream.frontend;

import java.util.Objects;

import org.smack.util.Holder;

import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;

/**
 * An fco that represents another fco as a reference.  Used by
 * datum label processing.  Elements of this type must not be
 * visible after the parsing phase.
 */
class Proxy extends FirstClassObject
{
    /**
     * The name of the type.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "proxy";

    private final Holder<FirstClassObject> _fcoHolder;

    Proxy( Holder<FirstClassObject> fcoHolder )
    {
        _fcoHolder =
                Objects.requireNonNull( fcoHolder );
    }

    /**
     * @return The proxied element.
     */
    public FirstClassObject value()
    {
        return _fcoHolder.get();
    }

    @Override
    public String toString()
    {
        long fcoId = _fcoHolder.get() == Cons.NIL ?
                0 :
                _fcoHolder.get().id();

        return String.format( "%s:fcoId=%d", TYPE_NAME, fcoId );
    }
}
