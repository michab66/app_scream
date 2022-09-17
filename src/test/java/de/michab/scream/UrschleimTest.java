package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import urschleim.Continuations;
import urschleim.Holder;

public class Urschleim
{
    @Test
    public void typeIntegerTest() throws Exception
    {
        var i = SchemeInteger.createObject( 313 );

        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );

        Continuations.trampoline(
                i.evaluate( null,
                        Continuations.endCall( s -> r.set( s ) ) ));

        assertEquals( "13", r.get().toString() );
    }
}
