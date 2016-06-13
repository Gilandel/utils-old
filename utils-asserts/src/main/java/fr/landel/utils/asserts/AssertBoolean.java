/*
 * #%L
 * utils-asserts
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.asserts;

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
        super(condition);
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code true}.
     * 
     * <pre>
     * AssertUtils.check(i &gt; 0).isFalse();
     * </pre>
     * 
     * @return this
     * @throws IllegalArgumentException
     *             if expression is {@code false}
     */
    public AssertBoolean isFalse() {
        return this.isFalse((CharSequence) null);
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code true}.
     * 
     * <pre>
     * AssertUtils.check(i &gt; 0).isFalse(&quot;The value must be greater than zero&quot;);
     * </pre>
     * 
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if expression is {@code false}
     */
    public AssertBoolean isFalse(final CharSequence message, final Object... arguments) {
        isFalse(this.get(), null, message, arguments);

        return this;
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code true}.
     * 
     * <pre>
     * AssertUtils.check(i &gt; 0).isFalse(exceptionToThrowOnError);
     * </pre>
     * 
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if expression is {@code false}
     */
    public <E extends Throwable> AssertBoolean isFalse(final E exception) throws E {
        isFalse(this.get(), exception, null);

        return this;
    }

    protected static <E extends Throwable> void isFalse(final Boolean expression, final E exception, final CharSequence message,
            final Object... arguments) throws E {
        if (expression == null || expression) {
            manageExceptions("this expression must be false", exception, message, new Object[] {expression}, arguments);
        }
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code false}.
     * 
     * <pre>
     * AssertUtils.check(i &gt; 0).isTrue();
     * </pre>
     * 
     * @return this
     * @throws IllegalArgumentException
     *             if expression is {@code false}
     */
    public AssertBoolean isTrue() {
        return this.isTrue((CharSequence) null);
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code false}.
     * 
     * <pre>
     * AssertUtils.check(i &gt; 0).isTrue(&quot;The value must be greater than zero&quot;);
     * </pre>
     * 
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if expression is {@code false}
     */
    public AssertBoolean isTrue(final CharSequence message, final Object... arguments) {
        isTrue(this.get(), null, message, arguments);

        return this;
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code false}.
     * 
     * <pre>
     * AssertUtils.check(i &gt; 0).isTrue(exceptionToThrowOnError);
     * </pre>
     * 
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if expression is {@code false}
     */
    public <E extends Throwable> AssertBoolean isTrue(final E exception) throws E {
        isTrue(this.get(), exception, null);

        return this;
    }

    protected static <E extends Throwable> void isTrue(final Boolean expression, final E exception, final CharSequence message,
            final Object... arguments) throws E {
        if (expression == null || !expression) {
            manageExceptions("this expression must be true", exception, message, new Object[] {expression}, arguments);
        }
    }
}