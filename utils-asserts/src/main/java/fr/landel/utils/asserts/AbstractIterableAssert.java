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

import java.util.Arrays;
import java.util.Iterator;

import org.apache.commons.collections4.IterableUtils;

import fr.landel.utils.commons.CollectionUtils2;

/**
 * Assertion utility class that assists in validating arguments for iterables.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public abstract class AbstractIterableAssert extends AbstractArrayAssert {

    /**
     * Assert that an iterable has elements; that is, it must not be
     * {@code null} and must have at least one element.
     * 
     * <pre>
     * AssertUtils.isNotEmpty(iterable, &quot;Iterable must have elements&quot;);
     * </pre>
     * 
     * @param iterable
     *            the iterable to check
     * @throws IllegalArgumentException
     *             if the iterable is {@code null} or has no elements
     */
    public static void isNotEmpty(final Iterable<?> iterable) {
        isNotEmpty(iterable, (String) null);
    }

    /**
     * Assert that a iterable has elements; that is, it must not be {@code null}
     * and must have at least one element.
     * 
     * <pre>
     * AssertUtils.isNotEmpty(iterable, &quot;Iterable must have elements&quot;);
     * </pre>
     * 
     * @param iterable
     *            the iterable to check
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the iterable is {@code null} or has no elements
     */
    public static void isNotEmpty(final Iterable<?> iterable, final String message, final Object... arguments) {
        isNotEmpty(iterable, null, message, arguments);
    }

    /**
     * Assert that a iterable has elements; that is, it must not be {@code null}
     * and must have at least one element.
     * 
     * <pre>
     * AssertUtils.isNotEmpty(iterable, exceptionToThrowOnError);
     * </pre>
     * 
     * @param iterable
     *            the iterable to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the iterable is {@code null} or has no elements
     */
    public static <E extends Throwable> void isNotEmpty(final Iterable<?> iterable, final E exception) throws E {
        isNotEmpty(iterable, exception, null);
    }

    private static <E extends Throwable> void isNotEmpty(final Iterable<?> iterable, final E exception, final String message,
            final Object... arguments) throws E {
        if (IterableUtils.isEmpty(iterable)) {
            manageExceptions("this iterable must not be empty: it must contain at least 1 element", exception, message,
                    new Object[] {iterable}, arguments);
        }
    }

    public static <T> void contains(final Iterable<T> iterable, final T object) {
        contains(iterable, object, null);
    }

    public static <T> void contains(final Iterable<T> iterable, final T object, final String message, final Object... arguments) {
        contains(iterable, object, null, message, arguments);
    }

    public static <T, E extends Throwable> void contains(final Iterable<T> iterable, final T object, final E exception) throws E {
        contains(iterable, object, exception, null);
    }

    private static <T, E extends Throwable> void contains(final Iterable<T> iterable, final T object, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(iterable);
        isNotNull(object);

        boolean found = false;

        Iterator<T> iterator = iterable.iterator();
        while (iterator.hasNext() && !found) {
            if (object.equals(iterator.next())) {
                found = true;
            }
        }

        if (!found) {
            manageExceptions("The iterable must contain the object", exception, message, new Object[] {iterable, object}, arguments);
        }
    }

    public static <T> void contains(final Iterable<T> iterable, final Iterable<T> objects) {
        contains(iterable, objects, null);
    }

    public static <T> void contains(final Iterable<T> iterable, final Iterable<T> objects, final String message,
            final Object... arguments) {
        contains(iterable, objects, null, message, arguments);
    }

    public static <T, E extends Throwable> void contains(final Iterable<T> iterable, final Iterable<T> objects, final E exception)
            throws E {
        contains(iterable, objects, exception, null);
    }

    private static <T, E extends Throwable> void contains(final Iterable<T> iterable, final Iterable<T> objects, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(iterable);
        isNotEmpty(objects);

        T[] array = CollectionUtils2.toArray(iterable);

        for (T object : objects) {
            if (Arrays.binarySearch(array, object) < 0) {
                manageExceptions("The iterable must contain all objects", exception, message, new Object[] {array, objects}, arguments);
            }
        }
    }

    public static <T> void doesNotContain(final Iterable<T> iterable, final T object) {
        doesNotContain(iterable, object, null);
    }

    public static <T> void doesNotContain(final Iterable<T> iterable, final T object, final String message, final Object... arguments) {
        doesNotContain(iterable, object, null, message, arguments);
    }

    public static <T, E extends Throwable> void doesNotContain(final Iterable<T> iterable, final T object, final E exception) throws E {
        doesNotContain(iterable, object, exception, null);
    }

    private static <T, E extends Throwable> void doesNotContain(final Iterable<T> iterable, final T object, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(iterable);
        isNotNull(object);

        boolean found = false;

        Iterator<T> iterator = iterable.iterator();
        while (iterator.hasNext() && !found) {
            if (object.equals(iterator.next())) {
                found = true;
            }
        }

        if (found) {
            manageExceptions("The iterable must not contain the object", exception, message, new Object[] {iterable, object}, arguments);
        }
    }

    public static <T> void doesNotContain(final Iterable<T> iterable, final Iterable<T> objects) {
        doesNotContain(iterable, objects, null);
    }

    public static <T> void doesNotContain(final Iterable<T> iterable, final Iterable<T> objects, final String message,
            final Object... arguments) {
        doesNotContain(iterable, objects, null, message, arguments);
    }

    public static <T, E extends Throwable> void doesNotContain(final Iterable<T> iterable, final Iterable<T> objects, final E exception)
            throws E {
        doesNotContain(iterable, objects, exception, null);
    }

    private static <T, E extends Throwable> void doesNotContain(final Iterable<T> iterable, final Iterable<T> objects, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(iterable);
        isNotEmpty(objects);

        T[] array = CollectionUtils2.toArray(iterable);

        for (T object : objects) {
            if (Arrays.binarySearch(array, object) > -1) {
                manageExceptions("The iterable must not contain any object", exception, message, new Object[] {array, objects}, arguments);
            }
        }
    }
}
