package de.michab.scream.util;

@FunctionalInterface
public interface SupplierX<T,X extends Exception>
{
    T get() throws X;
}
