/*
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons.stream;

import java.util.function.Supplier;

/**
 * Throwable supplier
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 * @param <T>
 *            The output type
 * @param <E>
 *            The exception type
 */
@FunctionalInterface
public interface SupplierThrowable<T, E extends Throwable> extends Supplier<T> {

    /**
     * Performs this operation on the given argument.
     *
     * @return t the output argument
     * @throws RuntimeException
     *             On error exception
     */
    @Override
    default T get() {
        try {
            return getThrows();
        } catch (final Throwable e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Performs this operation on the given argument.
     *
     * @return t the output argument
     * @throws E
     *             On error exception
     */
    T getThrows() throws E;
}
