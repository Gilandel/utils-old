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
 * Represents a throwable operation that accepts four input arguments and
 * returns no result. This is the four-arity/exception specialization of
 * {@link Consumer}. Unlike most other functional interfaces,
 * {@code QuadConsumer} is expected to operate via side-effects.
 *
 * <p>
 * This is a <a href="package-summary.html">functional interface</a> whose
 * functional method is {@link #accept(Object, Object, Object, Object)}.
 *
 * @since May 14, 2016
 * @author Gilles
 *
 * @param <T>
 *            the first argument type
 * @param <U>
 *            the second argument type
 * @param <V>
 *            the third argument type
 * @param <W>
 *            the fourth argument type
 * @param <E>
 *            The exception type
 */
@FunctionalInterface
public interface QuadConsumerThrowable<T, U, V, W, E extends Throwable> extends QuadConsumer<T, U, V, W> {

    /**
     * Performs this operation on the given arguments.
     *
     * @param t
     *            the first argument
     * @param u
     *            the second argument
     * @param v
     *            the third argument
     * @param w
     *            the fourth argument
     * @throws FunctionException
     *             On error exception
     */
    @Override
    default void accept(final T t, final U u, final V v, final W w) {
        try {
            acceptThrows(t, u, v, w);
        } catch (final Throwable e) {
            throw new FunctionException(e);
        }
    }

    /**
     * Performs this operation on the given arguments.
     *
     * @param t
     *            the first argument
     * @param u
     *            the second argument
     * @param v
     *            the third argument
     * @param w
     *            the fourth argument
     * @throws E
     *             On error exception
     */
    void acceptThrows(T t, U u, V v, W w) throws E;

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
    default QuadConsumerThrowable<T, U, V, W, E> andThen(final QuadConsumerThrowable<T, U, V, W, E> after) throws E {
        Objects.requireNonNull(after);
        return (t, u, v, w) -> {
            acceptThrows(t, u, v, w);
            after.acceptThrows(t, u, v, w);
        };
    }
}
