package urschleim;

public class Holder<T>
{
    private T value;

    public Holder( T v )
    {
        value = v;
    }

    public Holder<T> set( T v )
    {
        value = v;
        return this;
    }
    public T get()
    {
        return value;
    }
}
