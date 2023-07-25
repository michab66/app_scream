package de.michab.scream.ui;

import java.awt.Toolkit;
import java.awt.datatransfer.DataFlavor;

import org.smack.util.StringUtil;

public class UiUtil
{
    /**
     * @return The empty
     */
    public static String getClipboardText()
    {
        try
        {
            String result = (String)
                    Toolkit.
                    getDefaultToolkit().
                    getSystemClipboard().
                    getData(
                            DataFlavor.stringFlavor);

            if ( result == null )
                return StringUtil.EMPTY_STRING;

            return result;
        }
        catch ( Exception ignore )
        {
            return StringUtil.EMPTY_STRING;
        }
    }
}
