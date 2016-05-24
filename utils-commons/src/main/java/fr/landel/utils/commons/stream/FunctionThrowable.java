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
import java.util.function.Function;

/**
 * Throwable function
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 * @param <I>
 *            The input type
 * @param <O>
 *            The output type
 * @param <E>
 *            The exception type
 */
@FunctionalInterface
public interface FunctionThrowable<I, O, E extends Throwable> extends Function<I, O> {

    /**
     * Performs this operation on the given argument.
     *
     * @param t
     *            the input argument
     * @return The output result
     * @throws RuntimeException
     *             On error exception
     */
    @Override
    default O apply(final I t) {
        try {
            return applyThrows(t);
        } catch (final Throwable e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Performs this operation on the given argument.
     *
     * @param t
     *            the input argument
     * @return The output result
     * @throws E
     *             On error exception
     */
    O applyThrows(I t) throws E;

    /**
     * Returns a composed function that first applies the {@code before}
     * function to its input, and then applies this function to the result. If
     * evaluation of either function throws an exception, it is relayed to the
     * caller of the composed function.
     *
     * @param <V>
     *            the type of input to the {@code before} function, and to the
     *            composed function
     * @param before
     *            the function to apply before this function is applied
     * @return a composed function that first applies the {@code before}
     *         function and then applies this function
     * @throws NullPointerException
     *             if before is null
     * @throws E
     *             On error exception
     *
     * @see #andThen(Function)
     */
    default <V> FunctionThrowable<V, O, E> composeThrows(final FunctionThrowable<? super V, ? extends I, E> before) throws E {
        Objects.requireNonNull(before);
        return (V v) -> applyThrows(before.applyThrows(v));
    }

    /**
     * Returns a composed function that first applies this function to its
     * input, and then applies the {@code after} function to the result. If
     * evaluation of either function throws an exception, it is relayed to the
     * caller of the composed function.
     *
     * @param <V>
     *            the type of output of the {@code after} function, and of the
     *            composed function
     * @param after
     *            the function to apply after this function is applied
     * @return a composed function that first applies this function and then
     *         applies the {@code after} function
     * @throws NullPointerException
     *             if after is null
     * @throws E
     *             On error exception
     *
     * @see #compose(Function)
     */
    default <V> FunctionThrowable<I, V, E> andThen(final FunctionThrowable<? super O, ? extends V, E> after) throws E {
        Objects.requireNonNull(after);
        return (I t) -> after.applyThrows(applyThrows(t));
    }
}
