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

import fr.landel.utils.commons.exception.FunctionException;
import fr.landel.utils.commons.tuple.Quad;

/**
 * Represents a throwable supplier of quad results.
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
 * @param <A>
 *            The first type result
 * @param <B>
 *            The second type result
 * @param <C>
 *            The third type result
 * @param <D>
 *            The fourth type result
 */
@FunctionalInterface
public interface QuadSupplierThrowable<A, B, C, D, E extends Throwable> extends QuadSupplier<A, B, C, D> {

    /**
     * Performs this operation on the given argument.
     *
     * @return the output argument
     * @throws FunctionException
     *             On error exception
     */
    @Override
    default Quad<A, B, C, D> get() {
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
    Quad<A, B, C, D> getThrows() throws E;
}
