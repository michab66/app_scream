package mondo;

abstract class Continuation extends Counted
{
    public Continuation()
    {
        System.out.format( "Continuation{%s}%n", toString() );
    }

    abstract void runImpl( Continuation c );
    
    final void run( Continuation c )
    {
    	System.out.format( "run : %s -> %s%n", this, c );
    	runImpl(c);
    }
}
