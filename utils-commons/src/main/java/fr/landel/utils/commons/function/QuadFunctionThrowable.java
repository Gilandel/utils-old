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
import java.util.function.Function;

import fr.landel.utils.commons.exception.FunctionException;

/**
 * Represents a throwable function that accepts four arguments and produces a
 * result. This is the four-arity/exception specialization of {@link Function}.
 *
 * <p>
 * This is a <a href="package-summary.html">functional interface</a> whose
 * functional method is {@link #apply(Object, Object, Object, Object)}.
 * </p>
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
 * @param <R>
 *            the return argument type
 * @param <E>
 *            The exception type
 */
@FunctionalInterface
public interface QuadFunctionThrowable<T, U, V, W, R, E extends Throwable> extends QuadFunction<T, U, V, W, R> {

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
     *            the fourth function argument
     * @return The output result
     * @throws FunctionException
     *             On error exception
     */
    @Override
    default R apply(final T t, final U u, final V v, final W w) {
        try {
            return applyThrows(t, u, v, w);
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
     *            The second argument
     * @param v
     *            the third argument
     * @param w
     *            the fourth function argument
     * @return The output result
     * @throws E
     *             On error exception
     */
    R applyThrows(T t, U u, V v, W w) throws E;

    /**
     * Returns a composed function that first applies this function to its
     * input, and then applies the {@code after} function to the result. If
     * evaluation of either function throws an exception, it is relayed to the
     * caller of the composed function.
     *
     * @param <O>
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
     */
    default <O> QuadFunctionThrowable<T, U, V, W, O, E> andThen(final FunctionThrowable<R, O, E> after) throws E {
        Objects.requireNonNull(after);
        return (t, u, v, w) -> after.applyThrows(applyThrows(t, u, v, w));
    }
}
