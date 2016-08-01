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
package fr.landel.utils.assertor.expect;

import java.util.Objects;

import fr.landel.utils.commons.function.AssertConsumer;
import fr.landel.utils.commons.function.TriFunction;

/**
 * Assertion utility class that assists in validating thrown exception.
 *
 * @author Gilles
 */
public final class Expect {

    private Expect() {
    }

    /**
     * Check that the consumed code raise the specified exception.
     * 
     * <pre>
     * Expect.exception(() -&gt; {
     *     // throw new IllegalArgumentException("parameter cannot be null");
     *     getMyType(null);
     * }, IllegalArgumentException.class);
     * </pre>
     * 
     * @param consumer
     *            The consumer (required, not null)
     * @param expectedException
     *            The expected exception type (required, not null)
     * @param <T>
     *            The generic expected exception type
     */
    public static <T extends Throwable> void exception(final AssertConsumer<Throwable> consumer, final Class<T> expectedException) {
        exception(consumer, expectedException, null, null);
    }

    /**
     * Check that the consumed code raise the specified exception, also check
     * the message.
     * 
     * <pre>
     * Expect.exception(() -&gt; {
     *     // throw new IllegalArgumentException("parameter cannot be null");
     *     getMyType(null);
     * }, IllegalArgumentException.class, "parameter cannot be null");
     * </pre>
     * 
     * @param consumer
     *            The consumer (required, not null)
     * @param expectedException
     *            The expected exception type (required, not null)
     * @param expectedMessage
     *            The expected exception message
     * @param <T>
     *            The generic expected exception type
     */
    public static <T extends Throwable> void exception(final AssertConsumer<Throwable> consumer, final Class<T> expectedException,
            final String expectedMessage) {
        exception(consumer, expectedException, expectedMessage, null);
    }

    /**
     * Check that the consumed code raise the specified exception and allow to
     * change the thrown exception.
     * 
     * <pre>
     * // Obviously, you can save this in a static variable to share it
     * TriFunction&lt;Boolean, String, String&gt; junitError = (catched, expected, actual) -&gt; {
     *     if (catched) {
     *         return new ComparisonFailure("The exception message don't match.", expected, actual);
     *     } else {
     *         return new AssertionError("The expected exception never came up");
     *     }
     * };
     * 
     * Expect.exception(() -&gt; {
     *     // throw new IllegalArgumentException("parameter cannot be null");
     *     getMyType(null);
     * }, IllegalArgumentException.class, junitError);
     * 
     * // ComparisonFailure come from: org.junit.ComparisonFailure
     * </pre>
     * 
     * @param consumer
     *            The consumer (required, not null)
     * @param expectedException
     *            The expected exception type (required, not null)
     * @param exceptionFunction
     *            The exception function (three parameters are injected: (first:
     *            if it's the expected exception), (second: the expected
     *            message) and (third: the actual message), the return has to be
     *            a {@link Throwable}). If the exceptions don't match, the
     *            {@link String} parameters are {@code null}}.
     * @param <T>
     *            The generic expected exception type
     * @param <E>
     *            The exception thrown if the expected exception isn't raised
     * @throws E
     *             Exception provided
     */
    public static <T extends Throwable, E extends Throwable> void exception(final AssertConsumer<Throwable> consumer,
            final Class<T> expectedException, final TriFunction<Boolean, String, String, E> exceptionFunction) throws E {
        exception(consumer, expectedException, null, exceptionFunction);
    }

    /**
     * Check that the consumed code raise the specified exception, also check
     * the message and allow to change the thrown exception.
     * 
     * <pre>
     * // Obviously, you can save this in a static variable to share it
     * TriFunction&lt;Boolean, String, String&gt; junitError = (catched, expected, actual) -&gt; {
     *     if (catched) {
     *         return new ComparisonFailure("The exception message don't match.", expected, actual);
     *     } else {
     *         return new AssertionError("The expected exception never came up");
     *     }
     * };
     * 
     * Expect.exception(() -&gt; {
     *     // throw new IllegalArgumentException("parameter cannot be null");
     *     getMyType(null);
     * }, IllegalArgumentException.class, "parameter cannot be null", junitError);
     * 
     * // ComparisonFailure come from: org.junit.ComparisonFailure
     * </pre>
     * 
     * @param consumer
     *            The consumer (required, not null)
     * @param expectedException
     *            The expected exception type (required, not null)
     * @param expectedMessage
     *            The expected exception message
     * @param exceptionFunction
     *            The exception function (three parameters are injected: (first:
     *            if it's the expected exception), (second: the expected
     *            message) and (third: the actual message), the return has to be
     *            a {@link Throwable}). If the exceptions don't match, the
     *            {@link String} parameters are {@code null}}.
     * @param <T>
     *            The generic expected exception type
     * @param <E>
     *            The exception thrown if the expected exception isn't raised
     * @throws E
     *             Provided exception
     */
    public static <T extends Throwable, E extends Throwable> void exception(final AssertConsumer<Throwable> consumer,
            final Class<T> expectedException, final String expectedMessage, final TriFunction<Boolean, String, String, E> exceptionFunction)
            throws E {
        Objects.requireNonNull(consumer, "Consumer cannot be null");
        Objects.requireNonNull(expectedException, "Expected exception cannot be null");

        Throwable e = null;
        try {
            consumer.assertException();
        } catch (Throwable e1) {
            e = e1;
        }
        boolean exceptionDontMatch = e == null || !expectedException.isAssignableFrom(e.getClass());
        if (exceptionDontMatch || (expectedMessage != null && !expectedMessage.equals(e.getMessage()))) {
            if (exceptionFunction != null) {
                throw exceptionFunction.apply(!exceptionDontMatch, expectedMessage, e.getMessage());
            } else if (exceptionDontMatch) {
                throw new ExpectException("The expected exception never came up");
            } else {
                throw new ExpectException(
                        new StringBuilder("The exception message isn't as expected.\nExpected (first line) and result (second line):\n")
                                .append(expectedMessage).append("\n").append(e.getMessage()).toString());
            }
        }
    }
}