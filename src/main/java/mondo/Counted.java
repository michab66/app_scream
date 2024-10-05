package mondo;

import java.util.HashMap;
import java.util.Map;

public class Counted {
	private static Map<String, Integer> _counters = new HashMap<>();
	
    private int _id;

    public Counted() {
    	var classname = getClass().getSimpleName();
    	
    	if ( ! _counters.containsKey( classname ) )
    		_counters.put(classname, 0);

    	_id = _counters.get( classname ) + 1;
    	
    	_counters.put( classname, _id );
	}
    
    @Override
    public String toString() {
        return String.format( "%s#%s", getClass().getSimpleName(), _id );
    }

    int id()
    {
    	return _id;
    }
}
