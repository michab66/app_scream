// Experimental

package de.michab.scream.ui;

import java.util.Objects;
import java.util.function.Consumer;

import javax.swing.event.PopupMenuEvent;

/**
 *
 * @author micbinz
 */
public class PopupMenuListenerL extends Object
        implements javax.swing.event.PopupMenuListener
{
    public PopupMenuListenerL()
    {
    }

    Consumer<PopupMenuEvent> onBecomeVisible = null;


    public PopupMenuListenerL setOnBecomeVisible( Consumer<PopupMenuEvent> l )
    {
        onBecomeVisible = Objects.requireNonNull( l );
        return this;
    }

    @Override
    public void popupMenuWillBecomeVisible( PopupMenuEvent e )
    {
        if ( onBecomeVisible == null )
            return;

        onBecomeVisible.accept( e );
    }

    Consumer<PopupMenuEvent> onBecomeInvisible = null;

    public PopupMenuListenerL setOnBecomeInvisible( Consumer<PopupMenuEvent> l )
    {
        onBecomeInvisible = Objects.requireNonNull( l );
        return this;
    }

    @Override
    public void popupMenuWillBecomeInvisible( PopupMenuEvent e )
    {
        if ( onBecomeInvisible == null )
            return;

        onBecomeInvisible.accept( e );
    }

    Consumer<PopupMenuEvent> onMenuCanceled = null;

    public PopupMenuListenerL setOnmenuCanceled( Consumer<PopupMenuEvent> l )
    {
        onMenuCanceled = Objects.requireNonNull( l );
        return this;
    }

    @Override
    public void popupMenuCanceled( PopupMenuEvent e )
    {
        if ( onMenuCanceled == null )
            return;

        onMenuCanceled.accept( e );
    }
}
