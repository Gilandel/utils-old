/*-
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
import java.util.function.BiPredicate;

/**
 * Throwable predicate with two arguments
 *
 * @since 30 mai 2016
 * @author Gilles
 *
 */
public interface BiPredicateThrowable<T, U, E extends Throwable> extends BiPredicate<T, U> {

    /**
     * Evaluates this predicate on the given arguments.
     *
     * @param t
     *            the input argument
     * @param u
     *            the second argument
     * @return {@code true} if the input argument matches the predicate,
     *         otherwise {@code false}
     */
    default boolean test(final T t, final U u) {
        try {
            return testThrows(t, u);
        } catch (final Throwable e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Evaluates this predicate on the given arguments.
     *
     * @param t
     *            the first argument
     * @param u
     *            the second argument
     * @return {@code true} if the input argument matches the predicate,
     *         otherwise {@code false}
     * @throws E
     *             On error exception
     */
    boolean testThrows(T t, U u) throws E;

    /**
     * Returns a composed predicate that represents a short-circuiting logical
     * AND of this predicate and another. When evaluating the composed
     * predicate, if this predicate is {@code false}, then the {@code other}
     * predicate is not evaluated.
     *
     * <p>
     * Any exceptions thrown during evaluation of either predicate are relayed
     * to the caller; if evaluation of this predicate throws an exception, the
     * {@code other} predicate will not be evaluated.
     *
     * @param other
     *            a predicate that will be logically-ANDed with this predicate
     * @return a composed predicate that represents the short-circuiting logical
     *         AND of this predicate and the {@code other} predicate
     * @throws NullPointerException
     *             if other is null
     * @throws E
     *             On error exception
     */
    default BiPredicateThrowable<T, U, E> and(final BiPredicateThrowable<T, U, E> other) throws E {
        Objects.requireNonNull(other);
        return (t, u) -> testThrows(t, u) && other.testThrows(t, u);
    }

    /**
     * Returns a predicate that represents the logical negation of this
     * predicate.
     *
     * @return a predicate that represents the logical negation of this
     *         predicate
     * @throws E
     *             On error exception
     */
    default BiPredicateThrowable<T, U, E> negateThrows() throws E {
        return (t, u) -> !testThrows(t, u);
    }

    /**
     * Returns a composed predicate that represents a short-circuiting logical
     * OR of this predicate and another. When evaluating the composed predicate,
     * if this predicate is {@code true}, then the {@code other} predicate is
     * not evaluated.
     *
     * <p>
     * Any exceptions thrown during evaluation of either predicate are relayed
     * to the caller; if evaluation of this predicate throws an exception, the
     * {@code other} predicate will not be evaluated.
     *
     * @param other
     *            a predicate that will be logically-ORed with this predicate
     * @return a composed predicate that represents the short-circuiting logical
     *         OR of this predicate and the {@code other} predicate
     * @throws NullPointerException
     *             if other is null
     * @throws E
     *             On error exception
     */
    default BiPredicateThrowable<T, U, E> or(final BiPredicateThrowable<T, U, E> other) throws E {
        Objects.requireNonNull(other);
        return (t, u) -> testThrows(t, u) || other.testThrows(t, u);
    }
}