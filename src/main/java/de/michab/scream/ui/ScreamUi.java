/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.ui;

import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.Writer;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.script.ScriptEngine;
import javax.script.ScriptException;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JLabel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;
import javax.swing.event.PopupMenuListener;

import org.smack.application.ApplicationInfo;
import org.smack.swing.SwingUtil;
import org.smack.swing.application.Action8;
import org.smack.swing.application.ApplicationProperties;
import org.smack.swing.application.SingleFrameApplication;
import org.smack.swing.beans.PersistentJavaBeanProperty;
import org.smack.swing.swingx.JXConsole;
import org.smack.swing.swingx.JXMultiSplitPane;
import org.smack.swing.swingx.multisplitpane.MultiSplitLayout;
import org.smack.swing.swingx.multisplitpane.MultiSplitLayout.Column;
import org.smack.swing.swingx.multisplitpane.MultiSplitLayout.Leaf;
import org.smack.swing.swingx.multisplitpane.MultiSplitLayout.Row;
import org.smack.swing.swingx.multisplitpane.MultiSplitLayout.Split;
import org.smack.util.JavaUtil;
import org.smack.util.ServiceManager;
import org.smack.util.StringUtil;
import org.smack.util.TimeProbe;

import de.michab.scream.Scream;
import de.michab.scream.fcos.Cons;

/**
 * scream's user interface.
 *
 * @author micbinz
 */
public class ScreamUi extends SingleFrameApplication
{
    @SuppressWarnings("unused")
    private final static Logger LOG =
            Logger.getLogger( ScreamUi.class.getName() );

    private final TimeProbe _startupTp =
            new TimeProbe( "Startup:" );
    private JToolBar _toolbar =
            new JToolBar();
    private final ScriptEngine _scream;


    /**
     * The top-level multi split pane.
     */
    private JXMultiSplitPane _msp;

    /**
     * The default layout spec for the top-level multi layout.
     * Gets overwritten in init() with the spec from the
     * previous application execution.
     */
    private Split _multiSplitModel = new Column(
            new Row(
                    new Leaf( .5, "left"),
                    new Leaf( 0, "right" ) ),
            new Leaf( .5, "bottom" ) );

    private final JTextArea _textArea = JavaUtil.make( () ->
    {
        var result = new JTextArea();

        result.setName( "compositionArea" );

        final JPopupMenu contextMenu = new JPopupMenu();

        var cut = new Action8( result::cut ).inject( getClass(), "actCut" );
        contextMenu.add( cut );

        var copy = new Action8( result::copy ).inject( getClass(), "actCopy" );
        contextMenu.add( copy );

        var paste = new Action8( result::paste ).inject( getClass(), "actPaste" );
        contextMenu.add( paste );

        contextMenu.addSeparator();

        var exec = new Action8( () -> performExec(
                result.getSelectedText() ) ).inject( getClass(), "actExecSelected" );
        contextMenu.add( exec );

        final PopupMenuListener pml = new PopupMenuListenerL().setOnBecomeVisible(
                (e) -> {
                    var selected = result.getSelectedText();

                    cut.setEnabled(
                            result.isEditable() &&
                            StringUtil.hasContent( selected ) );
                    copy.setEnabled(
                            StringUtil.hasContent( selected ) );
                    paste.setEnabled(
                            result.isEditable() &&
                            StringUtil.hasContent( SwingUtil.getClipboardText() ) );
                    exec.setEnabled( StringUtil.hasContent( selected ) );
                } );

        contextMenu.addPopupMenuListener( pml );

        result.setComponentPopupMenu(contextMenu);

        return result;
    } );

    /**
     * A persistent property for the text in the composition area.
     */
    private PersistentJavaBeanProperty<String, JTextArea> compositionText =
            new PersistentJavaBeanProperty<String, JTextArea>(
                    _textArea,
                    "StringUtil.EMPTY_STRING",
                    "text",
                    ServiceManager.getApplicationService( org.smack.util.converters.StringConverter.class ) );

    private final JXConsole _console =
            new JXConsole();
    private final JXConsole _stderr =
            new JXConsole();
    private final JXConsole _stdoin =
            new JXConsole();

    private final OutputStreamWriter _consoleWriter =
            new OutputStreamWriter( _console.getOut() );

    public ScreamUi()
    {
        _startupTp.start();
        _scream = new Scream().getScriptEngine();
        _startupTp.stop();
    }

    private void writeConsole( String message )
    {
        try
        {
            _consoleWriter.write( message + StringUtil.EOL );
            _consoleWriter.flush();
        }
        catch ( Exception e )
        {
            throw new InternalError( "Unexpected: " + e );
        }
    }

    private void performExec( String text )
    {
        try
        {
            var result = _scream.eval( text );

            Writer w = new OutputStreamWriter( _console.getOut() );

            if ( result == Cons.NIL )
                w.write( "NIL" );
            else
                w.write( result.toString() );

            w.write( StringUtil.EOL );

            w.flush();
        }
        catch ( ScriptException e )
        {
            writeConsole( e.getCause().getMessage() );
        }
        catch ( Exception e )
        {
            writeConsole( e.toString() );
        }
    }

    private Action8 commitAction = new Action8( (ev) ->
    {
        if ( ev.getSource() != _textArea )
            return;

        JTextArea src = (JTextArea)ev.getSource();

        var selected = src.getSelectedText();

        if ( StringUtil.isEmpty( selected ) )
            return;

        performExec( selected );
        src.setSelectionStart( src.getSelectionEnd() );
    });

    @Override
    protected void ready()
    {
        writeConsole( _startupTp.toString() );
    }

    @Override
    protected void initialize( String[] args )
    {
        // Check if we find a valid multisplit layout from our previous execution
        // in persistence.
        {
            var applicationProperties = ServiceManager.getApplicationService(
                    ApplicationProperties.class );

            String multiSplitLayoutString = applicationProperties.get(
                    getClass(),
                    "multiSplitLayout",
                    null );

            if ( StringUtil.hasContent( multiSplitLayoutString ) )
            {
                try {
                    _multiSplitModel =
                            MultiSplitLayout.parseModel( multiSplitLayoutString );

                }
                catch (Exception e) {
                    LOG.log( Level.INFO, "Failure parsing model.  Using default.", e );
                }
            }
        }

        // Connect the streams between interpreter and UI.
        {
            var c = _scream.getContext();
            c.setWriter( new OutputStreamWriter( _stdoin.getOut() ) );
            _scream.setContext( c );
        }

        // Connect the standard streams to the UI.
        System.setErr( new PrintStream(_stderr.getOut() ) );
        System.setOut( new PrintStream(_stdoin.getOut() ) );
    }

    @Override
    protected void startup()
    {
        _textArea.setText( compositionText.get() );

        var COMMIT_ACTION = "commit";

        InputMap im = _textArea.getInputMap();
        ActionMap am = _textArea.getActionMap();
        im.put(
                KeyStroke.getKeyStroke("control X"),
                COMMIT_ACTION);
        am.put(
                COMMIT_ACTION,
                commitAction);

        _stderr.setName( "std::err" );
        _stdoin.setName( "std::in::out" );

        JTabbedPane ioTabs = new JTabbedPane();
        ioTabs.add( _stdoin, 0 );
        ioTabs.add( _stderr, 1 );

        _msp = JavaUtil.make( () -> {

            JXMultiSplitPane msp =
                    new JXMultiSplitPane(
                            new MultiSplitLayout(
                                    _multiSplitModel ) );
            msp.add(
                    ioTabs,
                    "bottom");
            msp.add(
                    new JScrollPane( _textArea ),
                    "left");
            msp.add(
                    _console,
                    "right");

            return msp;
        } );

        var view = getMainView();

        view.setComponent(
                _msp );
        view.setToolBar(
                _toolbar );
        view.setStatusBar(
                new JLabel(
                        _scream.getFactory().getEngineName() +
                        " " +
                        _scream.getFactory().getEngineVersion()) );
        show( view );
    }

    @Override
    protected void shutdown()
    {
        super.shutdown();

        // Save text from the composition area for the next run.
        compositionText.set( _textArea.getText() );

        // Save the multiSplitLayout for the next run.
        {
            var applicationProperties = ServiceManager.getApplicationService(
                    ApplicationProperties.class );

            applicationProperties.put(
                    getClass(),
                    "multiSplitLayout",
                    _msp.getModel().toString() );
        }
    }

    public static void main( String[] argv ) throws Exception
    {
        ServiceManager.initApplicationService(
                new ApplicationInfo( ScreamUi.class ) );
        ServiceManager.getApplicationService(
                ApplicationProperties.class );
        ScreamUi.launch( ScreamUi.class, argv );
    }
}
