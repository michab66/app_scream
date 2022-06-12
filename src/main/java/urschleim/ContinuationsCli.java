package urschleim;

import java.math.BigInteger;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.smack.application.CliApplication;

import urschleim.Continuations.Cont;
import urschleim.Continuations.Thunk;

public class ContinuationsCli extends CliApplication
{
    private static Logger LOG =
            Logger.getLogger( ContinuationsCli.class.getName() );

    private static Thunk factorial(int n, Cont<Integer> cont)
    {
        LOG.log( Level.INFO, "fact: " + n );

        return Continuations.eq(
                n,
                0,
                isNZero -> Continuations.when(
                        isNZero,
                        trueArg -> cont.apply(
                                1),
                        falseArg -> Continuations.add(
                                n,
                                -1,
                                nm1 -> factorial(
                                        nm1,
                                        fnm1 -> Continuations.multiply(
                                                n,
                                                fnm1,
                                                cont)))));
    }

    private static Thunk factorial(BigInteger n, Cont<BigInteger> cont)
    {
        LOG.log( Level.INFO, "fact: " + n );

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

    @Command
    public void factorial( int n )
    {
        Continuations.trampoline(
                factorial(n, Continuations.endCall(
                        result -> out( "%s%n", result ) ) ) );
    }

    @Command
    public void factorial2( int n )
    {
        Continuations.trampoline(
                factorial(
                        BigInteger.valueOf(n),
                        Continuations.endCall(
                                result -> out( "%s%n", result ) ) ) );
    }

    public static void main( String[] argv )
    {
        launch( ContinuationsCli::new, argv );
    }
}
