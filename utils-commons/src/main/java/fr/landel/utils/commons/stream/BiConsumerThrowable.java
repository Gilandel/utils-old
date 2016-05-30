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

import java.util.Objects;
import java.util.function.BiConsumer;

/**
 * Throwable bi-consumer
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 * @param <T>
 *            The first input type
 * @param <U>
 *            The second input type
 * @param <E>
 *            The exception type
 */
@FunctionalInterface
public interface BiConsumerThrowable<T, U, E extends Throwable> extends BiConsumer<T, U> {

    /**
     * Performs this operation on the given arguments.
     *
     * @param t
     *            the first argument
     * @param u
     *            the second argument
     * @throws RuntimeException
     *             On error exception
     */
    @Override
    default void accept(final T t, final U u) {
        try {
            acceptThrows(t, u);
        } catch (final Throwable e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Performs this operation on the given arguments.
     *
     * @param t
     *            the first input argument
     * @param u
     *            the second input argument
     * @throws E
     *             On error exception
     */
    void acceptThrows(T t, U u) throws E;

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
    default BiConsumerThrowable<T, U, E> andThen(final BiConsumerThrowable<T, U, E> after) throws E {
        Objects.requireNonNull(after);
        return (t, u) -> {
            acceptThrows(t, u);
            after.acceptThrows(t, u);
        };
    }
}
