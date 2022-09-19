package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import urschleim.Continuations;
import urschleim.Holder;

public class UrschleimTest
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

        assertEquals( "313", r.get().toString() );
    }

    @Test
    public void typeSymbolTest() throws Exception
    {
        var symbol = Symbol.createObject( "car" );
        var env = new Environment();
        env.set( symbol, SchemeInteger.createObject( 313 ) );

        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );

        Continuations.trampoline(
                symbol.evaluate( env,
                        Continuations.endCall( s -> r.set( s ) ) ));

        assertEquals( "313", r.get().toString() );
    }
}
