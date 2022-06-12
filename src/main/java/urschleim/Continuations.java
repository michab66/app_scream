package urschleim;

import java.math.BigInteger;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.logging.Logger;

public class Continuations
{
    private static Logger LOG =
            Logger.getLogger( Continuations.class.getName() );

    @FunctionalInterface
    public interface Cont<R> {
        Thunk apply(R result);
    }

    @FunctionalInterface
    public interface Thunk {
        Thunk run();
    }

    public static void trampoline(Thunk thunk) {
        while (thunk != null) {
            thunk = thunk.run();
        }
    }

    public static <T> Cont<T> endCall(Consumer<T> call) {
        return r -> {
            call.accept(r);
            return null;
        };
    }

    public static Thunk add(int a, int b, Cont<Integer> cont) {
        int sum = a + b;
        return () -> cont.apply(sum);
    }
    public static Thunk add(BigInteger a, BigInteger b, Cont<BigInteger> cont) {
        var sum = a.add( b );
        return () -> cont.apply(sum);
    }

    public static Thunk add(int a, int b, int c, Cont<Integer> cont) {
        return add(a, b, sum ->
                add(sum, c, cont));
    }

    public static Thunk multiply(int a, int b, Cont<Integer> cont)
    {
        int product = a * b;

        return () -> cont.apply(product);
    }
    public static Thunk multiply(BigInteger a, BigInteger b, Cont<BigInteger> cont)
    {
        var product = a.multiply(b);
        return () -> cont.apply(product);
    }

    public static <A, R> Thunk f(
            Function<A, R> f,
            A p0,
            Cont<R> c )
    {
        return c.apply( f.apply( p0 ) );
    }
    public static <A, B, R> Thunk f(
            BiFunction<A, B, R> f,
            A p0,
            B p1,
            Cont<R> c )
    {
        return c.apply( f.apply( p0, p1 ) );
    }

    /**
     * Calls the passed continuation with the result of the comparison between
     * parameter a and b.  If a equals b then true is passed, otherwise false.
     *
     * @param a Left hand comparison parameter.
     * @param b Rights hand comparison parameter.
     * @param cont A continuation taking a boolean parameter.
     * @return A thunk.
     */
    public static Thunk eq(int a, int b, Cont<Boolean> cont) {
        boolean result = (a == b);
        return () -> cont.apply(result);
    }
    public static <T> Thunk eq(T a, T b, Cont<Boolean> cont) {
        boolean result = (a.equals( b ));
        return () -> cont.apply(result);
    }

    public static Thunk lt(int a, int b, Cont<Boolean> cont) {
        boolean result  = (a < b);
        return () -> cont.apply(result);
    }

    public static Thunk when(
            boolean expr,
            Cont<Boolean> trueBranch,
            Cont<Boolean> falseBranch)
    {
        return (expr)
                ? () -> trueBranch.apply(true)
                : () -> falseBranch.apply(false);
    }
}
