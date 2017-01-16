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

import java.util.Locale;

/**
 * This class define methods that can be applied on the checked number. Each
 * method return a {@link PredicateStepNumber}
 *
 * @since Aug 3, 2016
 * @author Gilles
 *
 * @param <N>
 *            The type of checked object
 */
@FunctionalInterface
public interface PredicateAssertorNumber<N extends Number & Comparable<N>> extends PredicateAssertor<PredicateStepNumber<N>, N> {

    /**
     * {@inheritDoc}
     */
    @Override
    default PredicateStepNumber<N> get(final StepAssertor<N> result) {
        return () -> result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    default PredicateAssertorNumber<N> not() {
        return () -> HelperAssertor.not(this.getStep());
    }

    default PredicateStepNumber<N> isEqual(final N number) {
        return this.isEqual(number, null);
    }

    default PredicateStepNumber<N> isEqual(final N number, final CharSequence message, final Object... arguments) {
        return this.isEqual(number, null, message, arguments);
    }

    default PredicateStepNumber<N> isEqual(final N number, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorNumber.isEqual(this.getStep(), number, Message.of(locale, message, arguments));
    }

    default PredicateStepNumber<N> isNotEqual(final N number) {
        return this.isNotEqual(number, null);
    }

    default PredicateStepNumber<N> isNotEqual(final N number, final CharSequence message, final Object... arguments) {
        return this.isNotEqual(number, null, message, arguments);
    }

    default PredicateStepNumber<N> isNotEqual(final N number, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorNumber.isNotEqual(this.getStep(), number, Message.of(locale, message, arguments));
    }

    /**
     * Asserts that the given number is equal to zero. Otherwise returns false.
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * <pre>
     * Assertor.that(number).isZero().toThrow();
     * </pre>
     * 
     * @return The operator
     */
    default PredicateStepNumber<N> isZero() {
        return this.isZero(null);
    }

    /**
     * Asserts that the given number is equal to zero. Otherwise returns false.
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * <pre>
     * Assertor.that(number).isZero("not zero").toThrow();
     * </pre>
     * 
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return The operator
     */
    default PredicateStepNumber<N> isZero(final CharSequence message, final Object... arguments) {
        return this.isZero(null, message, arguments);
    }

    /**
     * Asserts that the given number is equal to zero. Otherwise returns false.
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * <pre>
     * Assertor.that(number).isZero(Locale.US, "not zero").toThrow();
     * </pre>
     * 
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return The operator
     */
    default PredicateStepNumber<N> isZero(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorNumber.isZero(this.getStep(), Message.of(locale, message, arguments));
    }

    /**
     * Asserts that the given number is positive. Positive means a number &gt;
     * 0. Also 0, negative number and {@code null} return false.
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * <pre>
     * Assertor.that(number).isPositive().toThrow();
     * </pre>
     * 
     * @return The operator
     */
    default PredicateStepNumber<N> isPositive() {
        return this.isPositive(null);
    }

    /**
     * Asserts that the given number is positive. Positive means a number &gt;
     * 0. Also 0, negative number and {@code null} return false.
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * <pre>
     * Assertor.that(number).isPositive("not positive").toThrow();
     * </pre>
     * 
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return The operator
     */
    default PredicateStepNumber<N> isPositive(final CharSequence message, final Object... arguments) {
        return this.isPositive(null, message, arguments);
    }

    /**
     * Asserts that the given number is positive. Positive means a number &gt;
     * 0. Also 0, negative number and {@code null} return false.
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * <pre>
     * Assertor.that(number).isPositive(Locale.US, "not positive").toThrow();
     * </pre>
     * 
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return The operator
     */
    default PredicateStepNumber<N> isPositive(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorNumber.isPositive(this.getStep(), Message.of(locale, message, arguments));
    }

    /**
     * Asserts that the given number is negative. Negative means a number &lt;
     * 0. Also 0, positive number and {@code null} return false.
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * <pre>
     * Assertor.that(number).isNegative().toThrow();
     * </pre>
     * 
     * @return The operator
     */
    default PredicateStepNumber<N> isNegative() {
        return this.isNegative(null);
    }

    /**
     * Asserts that the given number is negative. Negative means a number &lt;
     * 0. Also 0, positive number and {@code null} return false.
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * <pre>
     * Assertor.that(number).isNegative("not negative").toThrow();
     * </pre>
     * 
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return The operator
     */
    default PredicateStepNumber<N> isNegative(final CharSequence message, final Object... arguments) {
        return this.isNegative(null, message, arguments);
    }

    /**
     * Asserts that the given number is negative. Negative means a number &lt;
     * 0. Also 0, positive number and {@code null} return false.
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * <pre>
     * Assertor.that(number).isNegative(Locale.US, "not negative").toThrow();
     * </pre>
     * 
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return The operator
     */
    default PredicateStepNumber<N> isNegative(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorNumber.isNegative(this.getStep(), Message.of(locale, message, arguments));
    }

    default PredicateStepNumber<N> isGT(final N number) {
        return this.isGT(number, null);
    }

    default PredicateStepNumber<N> isGT(final N number, final CharSequence message, final Object... arguments) {
        return this.isGT(number, null, message, arguments);
    }

    default PredicateStepNumber<N> isGT(final N number, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorNumber.isGT(this.getStep(), number, Message.of(locale, message, arguments));
    }

    default PredicateStepNumber<N> isGTE(final N number) {
        return this.isGTE(number, null);
    }

    default PredicateStepNumber<N> isGTE(final N number, final CharSequence message, final Object... arguments) {
        return this.isGTE(number, null, message, arguments);
    }

    default PredicateStepNumber<N> isGTE(final N number, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorNumber.isGTE(this.getStep(), number, Message.of(locale, message, arguments));
    }

    default PredicateStepNumber<N> isLT(final N number) {
        return this.isLT(number, null);
    }

    default PredicateStepNumber<N> isLT(final N number, final CharSequence message, final Object... arguments) {
        return this.isLT(number, null, message, arguments);
    }

    default PredicateStepNumber<N> isLT(final N number, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorNumber.isLT(this.getStep(), number, Message.of(locale, message, arguments));
    }

    default PredicateStepNumber<N> isLTE(final N number) {
        return this.isLTE(number, null);
    }

    default PredicateStepNumber<N> isLTE(final N number, final CharSequence message, final Object... arguments) {
        return this.isLTE(number, null, message, arguments);
    }

    default PredicateStepNumber<N> isLTE(final N number, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorNumber.isLTE(this.getStep(), number, Message.of(locale, message, arguments));
    }
}