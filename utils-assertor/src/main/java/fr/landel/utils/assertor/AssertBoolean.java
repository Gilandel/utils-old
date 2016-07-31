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

/**
 * Assertion utility class that assists in validating arguments for booleans.
 *
 * @since 14 mai 2016
 * @author Gilles
 */
public class AssertBoolean extends AssertObject<AssertBoolean, Boolean> {

    /**
     * 
     * Constructor
     *
     * @param condition
     *            The condition to check
     */
    protected AssertBoolean(final Boolean condition) {
        super(condition, TYPE.BOOLEAN);
    }

    /**
     * Assert a boolean expression.
     * 
     * <pre>
     * Assertor.that(i &gt; 0).isTrue().toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertBoolean, Boolean> isFalse() {
        return this.isFalse(this.msg(MSG.BOOLEAN.FALSE));
    }

    /**
     * Assert a boolean expression.
     * 
     * <pre>
     * Assertor.that(i &gt; 0).isTrue().toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertBoolean, Boolean> isFalse(final CharSequence message, final Object... arguments) {
        return this.isFalse(null, message, arguments);
    }

    /**
     * Assert a boolean expression.
     * 
     * <pre>
     * Assertor.that(i &gt; 0).isTrue().toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertBoolean, Boolean> isFalse(final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> Boolean.FALSE.equals(this.get()), null, message, arguments, locale);
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code false}.
     * 
     * <pre>
     * Assertor.that(i &gt; 0).isTrue().toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertBoolean, Boolean> isTrue() {
        return this.isTrue(this.msg(MSG.BOOLEAN.TRUE));
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code false}.
     * 
     * <pre>
     * Assertor.that(i &gt; 0).isTrue().toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertBoolean, Boolean> isTrue(final CharSequence message, final Object... arguments) {
        return this.isTrue(null, message, arguments);
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code false}.
     * 
     * <pre>
     * Assertor.that(i &gt; 0).isTrue().toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertBoolean, Boolean> isTrue(final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> Boolean.TRUE.equals(this.get()), null, message, arguments, locale);
    }
}