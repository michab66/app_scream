/* $Id: AwtListener.java 163 2009-02-21 23:48:53Z Michael $
 *
 * Scream / JavaBinding
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2009 Michael G. Binz
 */
package de.michab.scream.binding;

import de.michab.scream.*;
import java.awt.event.*;
import java.util.logging.Level;
import java.util.logging.Logger;



/**
 * An adapter used for catching and processing AWT events in scheme.
 * Responsible for the dispatch of received events to dynamic settable
 * procedures.<p>
 * Note that this is only required for JDK1.2 compliant coding.  For better
 * versions of the jdk it is recommended to use the dynamic proxy stuff.
 *
 * @deprecated Use only for JDK1.2 compliant coding.
 */
public class  AwtListener
  implements ActionListener,
             AdjustmentListener,
             ComponentListener,
             ContainerListener,
             FocusListener,
             ItemListener,
             KeyListener,
             MouseListener,
             MouseMotionListener,
             WindowListener
{
  /**
   * The logger for this class.
   */
  private final static Logger _log =
    Logger.getLogger( AwtListener.class.getName() );



  // The table of operations.
  Operation[] operations = new Operation[ operationCount ];

  public final static int actionPerformedC = 0;
  public final static int adjustmentValueChangedC = 1;
  public final static int componentHiddenC = 2;
  public final static int componentMovedC = 3;
  public final static int componentResizedC = 4;
  public final static int componentShownC = 5;
  public final static int componentAddedC = 6;
  public final static int componentRemovedC = 7;
  public final static int focusGainedC = 8;
  public final static int focusLostC = 9;
  public final static int itemStateChangedC = 10;
  public final static int keyPressedC = 11;
  public final static int keyReleasedC = 12;
  public final static int keyTypedC = 13;
  public final static int mouseClickedC = 14;
  public final static int mouseEnteredC = 15;
  public final static int mouseExitedC = 16;
  public final static int mousePressedC = 17;
  public final static int mouseReleasedC = 18;
  public final static int mouseDraggedC = 19;
  public final static int mouseMovedC = 20;
  public final static int textValueChangedC = 21;
  public final static int windowActivatedC = 22;
  public final static int windowOpenedC = 23;
  public final static int windowIconifiedC = 24;
  public final static int windowDeiconifiedC = 25;
  public final static int windowDeactivatedC = 26;
  public final static int windowClosingC = 27;
  public final static int windowClosedC = 28;

  /**
   * This defines the number of operations held in this object.  Has to be
   * the value of the last enum plus one.
   */
  public final static int operationCount = 29;



  /**
   * Controls the behaviour for unset listeners.  If true a message is
   * written to stderr whenever an unset listener is called.
   */
  private boolean verbose = true;



  public AwtListener()
  {
      _log.warning( "Deprecated: AwtListener created." );
  }



  /**
   *
   */
  public boolean getVerbose()
  {
    return verbose;
  }



  /**
   *
   */
  public void setVerbose( boolean what )
  {
    verbose = what;
  }



  /**
   *
   */
  public void setListener( int idx, Operation op )
  {
    operations[ idx ] = op;
  }



  /**
   *
   */
  public FirstClassObject getListener( int idx )
  {
    Operation result = operations[ idx ];

    if ( result == null )
      return Cons.NIL;
    else
      return result;
  }



  /**
   *
   */
  private void call( int idx, java.lang.Object event, String methodname )
  {
    Operation op = operations[ idx ];

    if ( null != op )
    {
      try
      {
        FirstClassObject.evaluate(
          new Cons( op, new Cons( new SchemeObject( event ), Cons.NIL ) ),
          null );
      }
      catch ( RuntimeX e )
      {
        _log.log( Level.SEVERE, e.getClass().getName(), e );
      }
    }
    else if ( verbose )
      _log.info( methodname + " called." );
  }



  // ActionListener implementation

  /**
   *
   */
  public void actionPerformed( ActionEvent e )
  {
    call( actionPerformedC, e, "actionPerformed" );
  }



  // AdjustmentListener implementation.

  /**
   *
   */
  public void adjustmentValueChanged( AdjustmentEvent e )
  {
    call( adjustmentValueChangedC, e, "adjustmentValueChanged" );
  }



  // ComponentListener implementation

  /**
   *
   */
  public void componentHidden( ComponentEvent e )
  {
    call( componentHiddenC, e, "componentHidden" );
  }
  public void componentMoved( ComponentEvent e )
  {
    call( componentMovedC, e, "componentMoved" );
  }
  public void componentResized( ComponentEvent e )
  {
    call( componentResizedC, e, "componentResized" );
  }
  public void componentShown( ComponentEvent e )
  {
    call( componentShownC, e, "componentShown" );
  }



  // ContainerListener implementation

  /**
   *
   */
  public void componentAdded( ContainerEvent e )
  {
    call( componentAddedC, e, "componentAdded" );
  }
  public void componentRemoved( ContainerEvent e )
  {
    call( componentRemovedC, e, "componentRemoved" );
  }



  // FocusListener implementation.

  /**
   *
   */
  public void focusGained( FocusEvent e )
  {
    call( focusGainedC, e, "focusGained" );
  }
  public void focusLost( FocusEvent e )
  {
    call( focusLostC, e, "focusLost" );
  }



  // ItemListener implementation

  /**
   *
   */
  public void itemStateChanged( ItemEvent e )
  {
    call( itemStateChangedC, e, "itemStateChanged" );
  }



  // KeyListener implementation.

  /**
   *
   */
  public void keyPressed( KeyEvent e )
  {
    call( keyPressedC, e, "keyPressed" );
  }
  public void keyReleased( KeyEvent e )
  {
    call( keyReleasedC, e, "keyReleased" );
  }
  public void keyTyped( KeyEvent e )
  {
    call( keyTypedC, e, "keyTyped" );
  }



  // MouseListener implementation

  /**
   *
   */
  public void mouseClicked( MouseEvent e )
  {
    call( mouseClickedC, e, "mouseClicked" );
  }
  public void mouseEntered( MouseEvent e )
  {
    call( mouseEnteredC, e, "mouseEntered" );
  }
  public void mouseExited( MouseEvent e )
  {
    call( mouseExitedC, e, "mouseExited" );
  }
  public void mousePressed( MouseEvent e )
  {
    call( mousePressedC, e, "mousePressed" );
  }
  public void mouseReleased( MouseEvent e )
  {
    call( mouseReleasedC, e, "mouseReleased" );
  }



  // MouseMotionListener

  /**
   *
   */
  public void mouseDragged( MouseEvent e )
  {
    call( mouseDraggedC, e, "mouseDragged" );
  }
  public void mouseMoved( MouseEvent e )
  {
    call( mouseMovedC, e, "mouseMoved" );
  }



  // TextListener implementation

  /**
   *
   */
  public void textValueChanged( TextEvent e )
  {
    call( textValueChangedC, e, "textValueChanged" );
  }



  // WindowListener implementation
  public void windowActivated( WindowEvent e )
  {
    call( windowActivatedC, e, "windowActivated" );
  }
  public void windowClosed( WindowEvent e )
  {
    call( windowClosedC, e, "windowClosed" );
  }
  public void windowClosing( WindowEvent e )
  {
    call( windowClosingC, e, "windowClosing" );
  }
  public void windowDeactivated( WindowEvent e )
  {
    call( windowDeactivatedC, e, "windowDeactivated" );
  }
  public void windowDeiconified( WindowEvent e )
  {
    call( windowDeiconifiedC, e, "windowDeiconified" );
  }
  public void windowIconified( WindowEvent e )
  {
    call( windowIconifiedC, e, "windowIconified" );
  }
  public void windowOpened( WindowEvent e )
  {
    call( windowOpenedC, e, "windowOpened" );
  }
}
