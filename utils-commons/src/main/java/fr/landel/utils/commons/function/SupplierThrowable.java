/*
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons.function;

import java.util.function.Supplier;

import fr.landel.utils.commons.exception.FunctionException;

/**
 * Represents a throwable supplier of results.
 *
 * <p>
 * There is no requirement that a new or distinct result be returned each time
 * the supplier is invoked.
 * </p>
 *
 * <p>
 * This is a <a href="package-summary.html">functional interface</a> whose
 * functional method is {@link #get()}.
 * </p>
 *
 * @since May 14, 2016
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
     * @return the output argument
     * @throws FunctionException
     *             On error exception
     */
    @Override
    default T get() {
        try {
            return getThrows();
        } catch (final Throwable e) {
            throw new FunctionException(e);
        }
    }

    /**
     * Performs this operation on the given argument.
     *
     * @return the output argument
     * @throws E
     *             On error exception
     */
    T getThrows() throws E;
}
