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
 * Assertion utility class that assists in validating thrown exception.
 *
 * @author Gilles
 */
public final class Expect {

    private Expect() {
    }

    /**
     * Check that the consumed code raise the specified exception
     * 
     * @param consumer
     *            The consumer
     * @param expectedException
     *            The expected exception type
     * @param <T>
     *            The generic expected exception type
     */
    public static <T extends Throwable> void exception(final ConsumerAssert<Throwable> consumer, final Class<T> expectedException) {
        exception(consumer, expectedException, null, null);
    }

    /**
     * Check that the consumed code raise the specified exception
     * 
     * @param consumer
     *            The consumer
     * @param expectedException
     *            The expected exception type
     * @param expectedMessage
     *            The expected exception message
     * @param <T>
     *            The generic expected exception type
     */
    public static <T extends Throwable> void exception(final ConsumerAssert<Throwable> consumer, final Class<T> expectedException,
            final String expectedMessage) {
        exception(consumer, expectedException, expectedMessage, null);
    }

    /**
     * Check that the consumed code raise the specified exception
     * 
     * @param consumer
     *            The consumer
     * @param expectedException
     *            The expected exception type
     * @param exception
     *            The exception to throw
     * @param <T>
     *            The generic expected exception type
     * @param <E>
     *            The exception thrown if the expected exception isn't raised
     * @throws E
     *             Exception provided
     */
    public static <T extends Throwable, E extends Throwable> void exception(final ConsumerAssert<Throwable> consumer,
            final Class<T> expectedException, final E exception) throws E {
        exception(consumer, expectedException, null, exception);
    }

    /**
     * Check that the consumed code raise the specified exception
     * 
     * @param consumer
     *            The consumer
     * @param expectedException
     *            The expected exception type
     * @param expectedMessage
     *            The expected exception message
     * @param exception
     *            The exception to throw
     * @param <T>
     *            The generic expected exception type
     * @param <E>
     *            The exception thrown if the expected exception isn't raised
     * @throws E
     *             Provided exception
     */
    public static <T extends Throwable, E extends Throwable> void exception(final ConsumerAssert<Throwable> consumer,
            final Class<T> expectedException, final String expectedMessage, final E exception) throws E {
        Throwable e = null;
        try {
            consumer.assertException();
        } catch (Throwable e1) {
            e = e1;
        }
        boolean exceptionDontMatch = e == null || !expectedException.isAssignableFrom(e.getClass());
        if (exceptionDontMatch || (expectedMessage != null && !expectedMessage.equals(e.getMessage()))) {
            if (exception != null) {
                throw exception;
            } else if (exceptionDontMatch) {
                throw new RuntimeException("The expected exception never comes up");
            } else {
                throw new RuntimeException(
                        "The exception message isn't as expected.\nExpected: " + expectedMessage + "\nMessage: " + e.getMessage());
            }
        }
    }
}