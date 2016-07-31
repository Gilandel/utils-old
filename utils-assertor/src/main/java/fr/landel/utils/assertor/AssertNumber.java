/*
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

import fr.landel.utils.commons.Comparators;

/**
 * Assertion utility class that assists in validating arguments for numbers.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 * @param <N>
 *            The type of each number <code>Byte</code>, <code>Short</code>,
 *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
 *            <code>Double</code>, <code>BigInteger</code> or
 *            <code>BigDecimal</code>. Supports new <code>Number</code>, types
 *            only if it implements <code>Comparable</code>.
 */
public class AssertNumber<N extends Number & Comparable<N>> extends AssertObject<AssertNumber<N>, N> {

    /**
     * 
     * Constructor
     *
     * @param number
     *            the number to check
     */
    protected AssertNumber(final N number) {
        super(number, getType(number));
    }

    /**
     * Assert that the first number is equal to the second one.
     * 
     * <pre>
     * Assertor.that(10).isEqual(20).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param number
     *            The second number message the exception message, use the
     *            default assertion if null
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isEqual(final N number) {
        return this.isEqual(number, this.msg(MSG.NUMBER.EQUALS, this.getParam(), this.getNextParam(1, number)));
    }

    /**
     * Assert that the first number is equal to the second one.
     * 
     * <pre>
     * Assertor.that(10).isEqual(10, "Value not allowed").toThrow();
     * Assertor.that(10).isEqual(10, "Value not allowed for %s", key).toThrow();
     * </pre>
     * 
     * @param number
     *            The second number message the exception message, use the
     *            default assertion if null
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isEqual(final N number, final CharSequence message, final Object... arguments) {
        return this.isEqual(number, (Locale) null, message, arguments);
    }

    /**
     * Assert that the first number is equal to the second one.
     * 
     * <pre>
     * Assertor.that(10).isEqual(10, Locale.US, "Value not allowed").toThrow();
     * Assertor.that(10).isEqual(10, Locale.US, "Value not allowed for %s", key).toThrow();
     * </pre>
     * 
     * @param number
     *            The second number message the exception message, use the
     *            default assertion if null
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isEqual(final N number, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(true, () -> Comparators.compare(this.get(), number) == 0, null, message, arguments, locale, number);
    }

    /**
     * Assert that the first number is not equal to the second one.
     * 
     * <pre>
     * Assertor.that(10).isNotEqual(10).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param number
     *            The second number message the exception message, use the
     *            default assertion if null
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isNotEqual(final N number) {
        return this.isNotEqual(number, this.msg(MSG.NUMBER.EQUALS + MSG.NOT, this.getParam(), this.getNextParam(1, number)));
    }

    /**
     * Assert that the first number is not equal to the second one.
     * 
     * <pre>
     * Assertor.that(10).isNotEqual(10, "Value not allowed").toThrow();
     * Assertor.that(10).isNotEqual(10, "Value not allowed for %s", key).toThrow();
     * </pre>
     * 
     * @param number
     *            The second number message the exception message, use the
     *            default assertion if null
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isNotEqual(final N number, final CharSequence message, final Object... arguments) {
        return this.isNotEqual(number, (Locale) null, message, arguments);
    }

    /**
     * Assert that the first number is not equal to the second one.
     * 
     * <pre>
     * Assertor.that(10).isNotEqual(10, Locale.US, "Value not allowed").toThrow();
     * Assertor.that(10).isNotEqual(10, Locale.US, "Value not allowed for %s", key).toThrow();
     * </pre>
     * 
     * @param number
     *            The second number message the exception message, use the
     *            default assertion if null
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isNotEqual(final N number, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(true, () -> Comparators.compare(this.get(), number) != 0, null, message, arguments, locale, number);
    }

    /**
     * Assert that the first number is greater than the second one.
     * 
     * <pre>
     * Assertor.that(10).isGT(10).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param number
     *            The second number
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isGT(final N number) {
        return this.isGT(number, this.msg(MSG.NUMBER.GT, this.getParam(), this.getNextParam(1, number)));
    }

    /**
     * Assert that the first number is greater than the second one.
     * 
     * <pre>
     * Assertor.that(10).isGT(10, "Value not allowed").toThrow();
     * Assertor.that(10).isGT(10, "Value not allowed for %s", key).toThrow();
     * </pre>
     * 
     * @param number
     *            The second number
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isGT(final N number, final CharSequence message, final Object... arguments) {
        return this.isGT(number, (Locale) null, message, arguments);
    }

    /**
     * Assert that the first number is greater than the second one.
     * 
     * <pre>
     * Assertor.that(10).isGT(10, Locale.US, "Value not allowed").toThrow();
     * Assertor.that(10).isGT(10, Locale.US, "Value not allowed for %s", key).toThrow();
     * </pre>
     * 
     * @param number
     *            The second number
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isGT(final N number, final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> Comparators.compare(this.get(), number) > 0, null, message, arguments, locale, number);
    }

    /**
     * Assert that the first number is greater than or equal to the second one.
     * 
     * <pre>
     * Assertor.that(10).isGTE(10).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param number
     *            The second number
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isGTE(final N number) {
        return this.isGTE(number, this.msg(MSG.NUMBER.GTE, this.getParam(), this.getNextParam(1, number)));
    }

    /**
     * Assert that the first number is greater than or equal to the second one.
     * 
     * <pre>
     * Assertor.that(10).isGTE(10, "Value not allowed").toThrow();
     * Assertor.that(10).isGTE(10, "Value not allowed for %s", key).toThrow();
     * </pre>
     * 
     * @param number
     *            The second number
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isGTE(final N number, final CharSequence message, final Object... arguments) {
        return this.isGTE(number, (Locale) null, message, arguments);
    }

    /**
     * Assert that the first number is greater than or equal to the second one.
     * 
     * <pre>
     * Assertor.that(10).isGTE(10, Locale.US, "Value not allowed").toThrow();
     * Assertor.that(10).isGTE(10, Locale.US, "Value not allowed for %s", key).toThrow();
     * </pre>
     * 
     * @param number
     *            The second number
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isGTE(final N number, final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> Comparators.compare(this.get(), number) >= 0, null, message, arguments, locale, number);
    }

    /**
     * Assert that the first number is lower than the second one.
     * 
     * <pre>
     * Assertor.that(10).isLT(10, "Value not allowed").toThrow();
     * Assertor.that(10).isLT(10, "Value not allowed for %s", key).toThrow();
     * </pre>
     * 
     * @param number
     *            The second number
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isLT(final N number) {
        return this.isLT(number, this.msg(MSG.NUMBER.LT, this.getParam(), this.getNextParam(1, number)));
    }

    /**
     * Assert that the first number is lower than the second one.
     * 
     * <pre>
     * Assertor.that(10).isLT(10).toThrow();
     * </pre>
     * 
     * @param number
     *            The second number
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isLT(final N number, final CharSequence message, final Object... arguments) {
        return this.isLT(number, (Locale) null, message, arguments);
    }

    /**
     * Assert that the first number is lower than the second one.
     * 
     * <pre>
     * Assertor.that(10).isLT(10, Locale.US, "Value not allowed").toThrow();
     * Assertor.that(10).isLT(10, Locale.US, "Value not allowed for %s", key).toThrow();
     * </pre>
     * 
     * @param number
     *            The second number
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isLT(final N number, final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> Comparators.compare(this.get(), number) < 0, null, message, arguments, locale, number);
    }

    /**
     * Assert that the first number is lower than or equal to the second one.
     * 
     * <pre>
     * Assertor.that(10).isLTE(10).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param number
     *            The second number
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isLTE(final N number) {
        return this.isLTE(number, this.msg(MSG.NUMBER.LTE, this.getParam(), this.getNextParam(1, number)));
    }

    /**
     * Assert that the first number is lower than or equal to the second one.
     * 
     * <pre>
     * Assertor.that(10).isLTE(10, "Value not allowed").toThrow();
     * Assertor.that(10).isLTE(10, "Value not allowed for %s", key).toThrow();
     * </pre>
     * 
     * @param number
     *            The second number
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isLTE(final N number, final CharSequence message, final Object... arguments) {
        return this.isLTE(number, (Locale) null, message, arguments);
    }

    /**
     * Assert that the first number is lower than or equal to the second one.
     * 
     * <pre>
     * Assertor.that(10).isLTE(10, Locale.US, "Value not allowed").toThrow();
     * Assertor.that(10).isLTE(10, Locale.US, "Value not allowed for %s", key).toThrow();
     * </pre>
     * 
     * @param number
     *            The second number
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isLTE(final N number, final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> Comparators.compare(this.get(), number) <= 0, null, message, arguments, locale, number);
    }
}