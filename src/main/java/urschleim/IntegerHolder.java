package urschleim;

public class IntegerHolder
{
    private int value;

    public IntegerHolder( int v )
    {
        value = v;
    }
    public IntegerHolder()
    {
        this( 0 );
    }

    public IntegerHolder set( int v )
    {
        value = v;
        return this;
    }
    public int get()
    {
        return value;
    }
}
