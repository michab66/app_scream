package urschleim;

import java.util.ArrayList;

/**
 *
 * @author MICBINZ
 *
 * @param <T>
 */
public class Property<T>
{
    T _value;

    public Property( T v )
    {
        _value = v;
    }

    public Property<T> set( T newValue )
    {
        if ( ! _value.equals( newValue ) )
        {
            var oldValue = _value;
            _value = newValue;
            fireListeners( oldValue, newValue );
        }

        return this;

    }

    private void fireListeners( T ov, T nv )
    {
        for ( var c : _listeners )
            c.accept( this, ov, nv );
    }

    public T get()
    {
        return _value;
    }

    @FunctionalInterface
    public interface PropertyConsumer<T>
    {
        void accept( Property<T> property, T old, T next );
    }

    public void addListener( PropertyConsumer<T> consumer, boolean init )
    {
        _listeners.add( consumer );

        if ( init )
            consumer.accept( this,  _value, _value );
    }

    public void removeListener( PropertyConsumer<T> consumer )
    {
        _listeners.remove( consumer );
    }

    public void dispose()
    {
        _listeners.clear();
    }

    private ArrayList<PropertyConsumer<T>> _listeners =
            new ArrayList<>();
}
