package de.michab.scream.util;

import de.michab.scream.RuntimeX;

@FunctionalInterface
public interface SupplierX<T>
{
    T get() throws RuntimeX;
}
