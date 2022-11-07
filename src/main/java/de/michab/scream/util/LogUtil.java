/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.util;

import java.lang.reflect.Field;
import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.smack.util.StringUtil;

/**
 * From https://www.logicbig.com/tutorials/core-java-tutorial/logging/levels.html
 */
public class LogUtil {
  private static Logger log = Logger.getLogger(LogUtil.class.getName());

//  static {
//      System.setProperty("java.util.logging.SimpleFormatter.format",
//              "[%1$tF %1$tT %1$tL] [%4$-7s] %5$s %n");
//  }

  public static void main(String[] args) throws Exception {
      setLevel(Level.WARNING);
      Set<Level> levels = getAllLevels();
      int i = 1;
      for (Level level : levels) {
          log.log(level, level.getName() + " - " + (i++));
      }
  }

  /**
   * Set the log threshold.
   *
   * @param targetLevel The threshold level.
   */
  public static void setLevel(Level targetLevel) {
      setLevel( targetLevel, StringUtil.EMPTY_STRING );
  }

  /**
   * Set the log threshold for a group of classes below a certain name
   * prefix.
   *
   * @param targetLevel The threshold level.
   * @param namePrefix The logger name prefix for used when setting the
   * target level.
   */
  public static void setLevel(Level targetLevel, String namePrefix ) {
      Logger root = Logger.getLogger(namePrefix);
      root.setLevel(targetLevel);
      for (Handler handler : root.getHandlers()) {
          handler.setLevel(targetLevel);
      }
  }

  private static Set<Level> getAllLevels() throws IllegalAccessException {
      Class<Level> levelClass = Level.class;

      Set<Level> allLevels = new TreeSet<>(
              Comparator.comparingInt(Level::intValue));

      for (Field field : levelClass.getDeclaredFields()) {
          if (field.getType() == Level.class) {
              allLevels.add((Level) field.get(null));
          }
      }
      return allLevels;
  }
}
