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

import java.util.Objects;
import java.util.function.Consumer;

import fr.landel.utils.commons.exception.FunctionException;

/**
 * Represents a throwable operation that accepts a single input argument and
 * returns no result. Unlike most other functional interfaces, {@code Consumer}
 * is expected to operate via side-effects.
 *
 * <p>
 * This is a <a href="package-summary.html">functional interface</a> whose
 * functional method is {@link #accept(Object)}.
 *
 * @since May 14, 2016
 * @author Gilles
 *
 * @param <T>
 *            The input type
 * @param <E>
 *            The exception type
 */
@FunctionalInterface
public interface ConsumerThrowable<T, E extends Throwable> extends Consumer<T> {

    /**
     * Performs this operation on the given argument.
     *
     * @param t
     *            the input argument
     * @throws FunctionException
     *             On error exception
     */
    @Override
    default void accept(final T t) {
        try {
            acceptThrows(t);
        } catch (final Throwable e) {
            throw new FunctionException(e);
        }
    }

    /**
     * Performs this operation on the given argument.
     *
     * @param t
     *            the input argument
     * @throws E
     *             On error exception
     */
    void acceptThrows(T t) throws E;

    /**
     * Returns a composed {@code Consumer} that performs, in sequence, this
     * operation followed by the {@code after} operation. If performing either
     * operation throws an exception, it is relayed to the caller of the
     * composed operation. If performing this operation throws an exception, the
     * {@code after} operation will not be performed.
     *
     * @param after
     *            the operation to perform after this operation
     * @return a composed {@code Consumer} that performs in sequence this
     *         operation followed by the {@code after} operation
     * @throws NullPointerException
     *             if {@code after} is null
     * @throws E
     *             On error exception
     */
    default ConsumerThrowable<T, E> andThen(final ConsumerThrowable<T, E> after) throws E {
        Objects.requireNonNull(after);
        return (T t) -> {
            acceptThrows(t);
            after.acceptThrows(t);
        };
    }
}
