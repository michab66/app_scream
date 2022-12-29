package urschleim;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiFunction;
import java.util.function.Function;

import org.junit.jupiter.api.Test;
import org.smack.util.IntegerHolder;

import urschleim.Continuations.Cont;
import urschleim.Continuations.Thunk;

public class ContinuationsTest
{
    private static Thunk factorial(BigInteger n, Cont<BigInteger> cont)
    {
        return Continuations.eq(
                n,
                BigInteger.ZERO,
                isNZero -> Continuations.when(
                        isNZero,
                        trueArg -> cont.apply(
                                BigInteger.ONE),
                        falseArg -> Continuations.add(
                                n,
                                BigInteger.ONE.negate(),
                                nm1 -> factorial(
                                        nm1,
                                        fnm1 -> Continuations.multiply(
                                                n,
                                                fnm1,
                                                cont)))));
    }

    private static Thunk factorial(int n, Cont<Integer> cont)
    {
        return factorial(
                BigInteger.valueOf( n ),
                r -> cont.apply( r.intValueExact() ) );
    }

    @Test
    public void factorialInteger() {
        AtomicInteger res = new AtomicInteger(-1);
        Continuations.trampoline(factorial(4, Continuations.endCall(res::set)));
        assertEquals(24, res.get());
    }

    @Test
    public void factorialIntegerOv()
    {
        try
        {
            AtomicInteger res = new AtomicInteger(-1);
            Continuations.trampoline(factorial(20, Continuations.endCall(res::set)));
            fail();
        }
        catch ( ArithmeticException expected )
        {

        }
    }

    @Test
    public void factorialBigInteger()
    {
        Continuations.trampoline(
                factorial(
                        BigInteger.valueOf( 20 ),
                        Continuations.endCall(
                                r -> assertEquals("2432902008176640000", r.toString() ) ) ) );
    }

    @Test
    public void function() {

        Function<String, Integer> toInteger  = s -> {
            return Integer.valueOf( s );
        };

        IntegerHolder i = new IntegerHolder();

        Continuations.trampoline(

            Continuations.f(
                    toInteger,
                    "313",
                    Continuations.endCall(i::set))
        );

        assertEquals(313, i.get());
    }

    @Test
    public void function_()
    {
        Function<String, Integer> toInteger  = s -> {
            return Integer.valueOf( s );
        };

        Continuations.trampoline(

            Continuations.f(
                    toInteger,
                    "313",
                    Continuations.endCall(r -> assertEquals(313, r)))
        );
    }

    @Test
    public void biFunction()
    {
        BiFunction<Integer, Integer, Integer> add  = (a,b) -> {
            return a + b;
        };

        IntegerHolder i = new IntegerHolder();

        Continuations.trampoline(

            Continuations.f(
                    add,
                    301,
                    12,
                    Continuations.endCall(i::set))
        );

        assertEquals(313, i.get());
    }

    @Test
    public void biFunction_()
    {
        BiFunction<Integer, Integer, Integer> add  = (a,b) -> {
            return a + b;
        };

        Continuations.trampoline(

            Continuations.f(
                    add,
                    301,
                    12,
                    Continuations.endCall(
                            r -> assertEquals(313, r) ) ) );
    }
}
