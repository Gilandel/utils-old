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

import java.util.function.BooleanSupplier;
import java.util.function.Supplier;

import fr.landel.utils.commons.exception.FunctionException;

/**
 * Represents a throwable supplier of {@code boolean}-valued results. This is
 * the {@code boolean}-producing primitive/exception specialization of
 * {@link Supplier}.
 *
 * <p>
 * There is no requirement that a new or distinct result be returned each time
 * the supplier is invoked.
 * </p>
 * 
 * <p>
 * This is a <a href="package-summary.html">functional interface</a> whose
 * functional method is {@link #getAsBoolean()}.
 * </p>
 *
 * @since May 14, 2016
 * @author Gilles
 *
 * @param <E>
 *            The exception type
 */
@FunctionalInterface
public interface BooleanSupplierThrowable<E extends Throwable> extends BooleanSupplier {

    /**
     * Performs this operation on the given argument.
     *
     * @return the output argument
     * @throws FunctionException
     *             On error exception
     */
    @Override
    default boolean getAsBoolean() {
        try {
            return getAsBooleanThrows();
        } catch (final Throwable e) {
            throw new FunctionException(e);
        }
    }

    /**
     * Performs this operation on the given argument.
     *
     * @return t the output argument
     * @throws E
     *             On error exception
     */
    boolean getAsBooleanThrows() throws E;
}
