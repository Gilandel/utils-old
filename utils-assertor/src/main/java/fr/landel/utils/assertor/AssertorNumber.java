/*-
 * #%L
 * utils-assertor
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.assertor;

import java.util.function.BiPredicate;

import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.commons.Comparators;

/**
 * Utility class to prepare the check of {@link Number}
 *
 * @since Aug 10, 2016
 * @author Gilles
 *
 */
public class AssertorNumber extends Constants {

    /**
     * Prepare the next step to validate if {@link Number} is equal to
     * {@code number}
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * @param step
     *            the previous step
     * @param number
     *            the number to compare
     * @param message
     *            the message if invalid
     * @param <N>
     *            The number type
     * @return the next step
     */
    protected static <N extends Number & Comparable<N>> StepAssertor<N> isEqual(final StepAssertor<N> step, final N number,
            final Message message) {

        final BiPredicate<N, Boolean> checker = (object, not) -> Comparators.compare(object, number) == 0;

        return new StepAssertor<>(step, checker, false, message, MSG.NUMBER.EQUALS, false, Pair.of(number, EnumType.getType(number)));
    }

    /**
     * Prepare the next step to validate if {@link Number} is NOT equal to
     * {@code number}
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * @param step
     *            the previous step
     * @param number
     *            the number to compare
     * @param message
     *            the message if invalid
     * @param <N>
     *            The number type
     * @return the next step
     */
    protected static <N extends Number & Comparable<N>> StepAssertor<N> isNotEqual(final StepAssertor<N> step, final N number,
            final Message message) {

        final BiPredicate<N, Boolean> checker = (object, not) -> Comparators.compare(object, number) != 0;

        return new StepAssertor<>(step, checker, false, message, MSG.NUMBER.EQUALS, true, Pair.of(number, EnumType.getType(number)));
    }

    /**
     * Prepare the next step to validate if {@link Number} is greater than
     * {@code number}
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * @param step
     *            the previous step
     * @param number
     *            the number to compare
     * @param message
     *            the message if invalid
     * @param <N>
     *            The number type
     * @return the next step
     */
    protected static <N extends Number & Comparable<N>> StepAssertor<N> isGT(final StepAssertor<N> step, final N number,
            final Message message) {

        final BiPredicate<N, Boolean> checker = (object, not) -> Comparators.compare(object, number) > 0;

        return new StepAssertor<>(step, checker, false, message, MSG.NUMBER.GT, false, Pair.of(number, EnumType.getType(number)));
    }

    /**
     * Prepare the next step to validate if {@link Number} is greater than or
     * equals to {@code number}
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * @param step
     *            the previous step
     * @param number
     *            the number to compare
     * @param message
     *            the message if invalid
     * @param <N>
     *            The number type
     * @return the next step
     */
    protected static <N extends Number & Comparable<N>> StepAssertor<N> isGTE(final StepAssertor<N> step, final N number,
            final Message message) {

        final BiPredicate<N, Boolean> checker = (object, not) -> Comparators.compare(object, number) >= 0;

        return new StepAssertor<>(step, checker, false, message, MSG.NUMBER.GTE, false, Pair.of(number, EnumType.getType(number)));
    }

    /**
     * Prepare the next step to validate if {@link Number} is lower than
     * {@code number}
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * @param step
     *            the previous step
     * @param number
     *            the number to compare
     * @param message
     *            the message if invalid
     * @param <N>
     *            The number type
     * @return the next step
     */
    protected static <N extends Number & Comparable<N>> StepAssertor<N> isLT(final StepAssertor<N> step, final N number,
            final Message message) {

        final BiPredicate<N, Boolean> checker = (object, not) -> Comparators.compare(object, number) < 0;

        return new StepAssertor<>(step, checker, false, message, MSG.NUMBER.LT, false, Pair.of(number, EnumType.getType(number)));
    }

    /**
     * Prepare the next step to validate if {@link Number} is lower than or
     * equals to {@code number}
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * @param step
     *            the previous step
     * @param number
     *            the number to compare
     * @param message
     *            the message if invalid
     * @param <N>
     *            The number type
     * @return the next step
     */
    protected static <N extends Number & Comparable<N>> StepAssertor<N> isLTE(final StepAssertor<N> step, final N number,
            final Message message) {

        final BiPredicate<N, Boolean> checker = (object, not) -> Comparators.compare(object, number) <= 0;

        return new StepAssertor<>(step, checker, false, message, MSG.NUMBER.LTE, false, Pair.of(number, EnumType.getType(number)));
    }
}
