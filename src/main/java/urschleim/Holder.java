package urschleim;

import java.util.Objects;

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

    @Override
    public String toString()
    {
        return Objects.toString( value );
    }
}
