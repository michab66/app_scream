/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.Writer;

import javax.script.ScriptEngine;
import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;

import org.smack.application.ApplicationInfo;
import org.smack.swing.application.SingleFrameApplication;
import org.smack.swing.swingx.JXConsole;
import org.smack.swing.swingx.JXMultiSplitPane;
import org.smack.swing.swingx.MultiSplitLayout;
import org.smack.swing.swingx.MultiSplitLayout.Divider;
import org.smack.swing.swingx.MultiSplitLayout.Leaf;
import org.smack.swing.swingx.MultiSplitLayout.Split;
import org.smack.util.JavaUtil;
import org.smack.util.ServiceManager;
import org.smack.util.StringUtil;

import de.michab.scream.Scream;
import de.michab.scream.fcos.Cons;

/**
 * scream's user interface.
 *
 * @author micbinz
 */
public class ScreamUi extends SingleFrameApplication
{
    private JToolBar _toolbar =
            new JToolBar();
    private ScriptEngine _scream =
            new Scream().getScriptEngine();
    private final JTextArea _textArea =
            new JTextArea();
    private final JXConsole _console =
            new JXConsole();
    private final JXConsole _stderr =
            new JXConsole();
    private final JXConsole _stdoin =
            new JXConsole();

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
        catch ( Exception e )
        {
            try
            {
                e.printStackTrace();
                _console.getOut().write( e.getMessage().getBytes() );
                _console.getOut().write( StringUtil.EOL.getBytes() );
            }
            catch ( IOException e1 )
            {
                throw new InternalError( "unexpected" );
            }
        }
    }

    private class CommitAction extends AbstractAction {
        @Override
        public void actionPerformed(ActionEvent ev) {
            if ( ev.getSource() != _textArea )
                return;

            JTextArea src = (JTextArea)ev.getSource();

            var selected = src.getSelectedText();

            if ( StringUtil.isEmpty( selected ) )
                return;

            performExec( selected );
            src.setSelectionStart( src.getSelectionEnd() );
        }
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
        System.err.print( "@console:err" + StringUtil.EOL );
        System.setOut( new PrintStream(_stdoin.getOut() ) );
        System.out.print( "@console:in-out" + StringUtil.EOL );
    }

    @Override
    protected void startup()
    {
        var COMMIT_ACTION = "commit";

        InputMap im = _textArea.getInputMap();
        ActionMap am = _textArea.getActionMap();
        im.put(
                KeyStroke.getKeyStroke("control X"),
                COMMIT_ACTION);
        am.put(
                COMMIT_ACTION,
                new CommitAction());

        _stderr.setName( "std::err" );
        _stdoin.setName( "std::in::out" );

        JTabbedPane ioTabs = new JTabbedPane();
        ioTabs.add( _stdoin, 0 );
        ioTabs.add( _stderr, 1 );

        Component component = JavaUtil.make( () -> {
            // https://stackoverflow.com/questions/8660687/how-to-use-multisplitlayout-in-swingx

            Split row = JavaUtil.make( () -> {
                var left = new Leaf("left");
                left.setWeight( .8 );
                var right = new Leaf( "right" );
                right.setWeight( .2 );

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

    public static void main( String[] argv )
    {
        ServiceManager.initApplicationService(
                new ApplicationInfo( ScreamUi.class ) );

        ScreamUi.launch( ScreamUi.class, argv );
    }
}
