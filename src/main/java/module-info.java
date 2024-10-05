/*
 * Copyright © 2024 Michael G. Binz
 */
module scream
{
    requires transitive java.desktop;
    requires java.logging;
    requires java.prefs;
    requires java.compiler;
    requires framework.smack;
    requires java.scripting;
    requires framework.smack_swing;

    opens de.michab.scream.ui;
    opens de.michab.scream.util;
    opens de.michab.scream to framework.smack;
}
