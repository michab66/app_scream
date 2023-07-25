/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.ui;

import java.awt.Component;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.Writer;
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
import org.smack.swing.application.Action8;
import org.smack.swing.application.ApplicationProperties;
import org.smack.swing.application.SingleFrameApplication;
import org.smack.swing.beans.PersistentJavaBeanProperty;
import org.smack.swing.swingx.JXConsole;
import org.smack.swing.swingx.JXMultiSplitPane;
import org.smack.swing.swingx.MultiSplitLayout;
import org.smack.swing.swingx.MultiSplitLayout.Divider;
import org.smack.swing.swingx.MultiSplitLayout.Leaf;
import org.smack.swing.swingx.MultiSplitLayout.Split;
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
                            StringUtil.hasContent( UiUtil.getClipboardText() ) );
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
        {
            var c = _scream.getContext();
            c.setWriter( new OutputStreamWriter( _stdoin.getOut() ) );
            _scream.setContext( c );
        }

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

        Component component = JavaUtil.make( () -> {
            // https://stackoverflow.com/questions/8660687/how-to-use-multisplitlayout-in-swingx

            Split row = JavaUtil.make( () -> {
                var left = new Leaf("left");
                left.setWeight( .5 );
                var right = new Leaf( "right" );
                right.setWeight( .5 );

                return new Split(
                        left,
                        new Divider(),
                        right );
            });

            Split column = JavaUtil.make( () -> {
                var bottom = new Leaf( "bottom" );
                bottom.setWeight( .5 );
                row.setWeight( .5 );

                var result = new Split(
                        row,
                        new Divider(),
                        bottom );
                result.setRowLayout( false );

                return result;
            });

            JXMultiSplitPane msp = new JXMultiSplitPane(
                    new MultiSplitLayout( column ) );

            msp.add( ioTabs, "bottom");

            msp.add( new JScrollPane( _textArea ), "left");

            msp.add( new JScrollPane( _textArea ), "left");
            msp.add( _console, "right");

            return msp;
        } );

        var view = getMainView();

        view.setComponent(
                component );
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
        compositionText.set( _textArea.getText() );
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
