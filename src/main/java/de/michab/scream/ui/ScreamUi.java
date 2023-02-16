/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;

import javax.script.ScriptEngine;
import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JButton;
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
import org.smack.swing.swingx.JXSplitPane;
import org.smack.swing.swingx.JXTextArea;
import org.smack.swing.swingx.MultiSplitLayout;
import org.smack.swing.swingx.MultiSplitLayout.Divider;
import org.smack.swing.swingx.MultiSplitLayout.Leaf;
import org.smack.swing.swingx.MultiSplitLayout.Split;
import org.smack.util.ServiceManager;
import org.smack.util.StringUtil;

import de.michab.scream.Scream;
import de.michab.scream.fcos.Cons;

public class ScreamUi extends SingleFrameApplication
{
    private JToolBar _toolbar = new JToolBar();

    private ScriptEngine _scream = new Scream().getScriptEngine();


    private JTextArea _textArea = new JTextArea();
    private JTextArea _textArea2 = new JXTextArea( "mic" );


    private JXConsole _console;

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
                _console.getOut().write( e.getMessage().getBytes() );
            }
            catch ( IOException e1 )
            {
                // TODO Auto-generated catch block
                e1.printStackTrace();
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
        }
    }

    public void handleQuit()
    {
        System.out.println( "Quit called." );
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

        JXConsole stderr = new JXConsole();
        stderr.setName( "std::err" );
        JXConsole stdoin = new JXConsole();
        stdoin.setName( "std::in::out" );

        JTabbedPane ioTabs = new JTabbedPane();
        ioTabs.add( stdoin, 0 );
        ioTabs.add( stderr, 1 );

        _console = new JXConsole();

        Component component;

        if ( false )
        {
            var spHorizontal = new JXSplitPane( 0.5 );
            spHorizontal.setLeftComponent( _textArea );
            spHorizontal.setRightComponent( _console );

            var sp = new JXSplitPane( 0.5 );
            sp.setOrientation( JXSplitPane.VERTICAL_SPLIT );
            sp.setTopComponent( ioTabs );
            sp.setBottomComponent( spHorizontal );

            component = sp;
        }
        else
        {
            Split column1 = new Split();
            column1.setRowLayout(false);

            Split row = new Split();

            Split column2 = new Split();
            column2.setRowLayout(false);

            column2.setChildren(new Leaf("middle.top"), new Divider(), new Leaf(
                "middle"), new Divider(), new Leaf("middle.bottom"));

            row.setChildren(new Leaf("left"), new Divider(), column2,
                new Divider(), new Leaf("right"));

            column1.setChildren(row, new Divider(), new Leaf("bottom"));

            _textArea.setText( "Welcome..." );
            // Once the layout is done, the code is easy
            JXMultiSplitPane msp = new JXMultiSplitPane();
            MultiSplitLayout layout = new MultiSplitLayout(column1);
            msp.setLayout(layout);
//          msp.add(new JButton("bottom"), "bottom");
            msp.add( ioTabs, "bottom");
//            msp.add(new JButton("left"), "left");
            msp.add( new JScrollPane( _textArea ), "left");
//            msp.add(new JButton("right"), "right");
            msp.add( _console, "right");
            msp.add(new JButton("middle.bottom"), "middle.bottom");
            msp.add(new JButton("middle"), "middle");
            msp.add(new JButton("middle.top"), "middle.top");

            component = msp;
        }

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
