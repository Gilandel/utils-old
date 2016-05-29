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
package fr.landel.utils.commons.asserts;

import java.util.Arrays;
import java.util.Comparator;

import org.apache.commons.lang3.ArrayUtils;

/**
 * Assertion utility class that assists in validating arguments for arrays.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public abstract class AbstractArrayAssert extends AbstractDateAssert {

    /**
     * Assert that an array has elements; that is, it must not be {@code null}
     * and must have at least one element.
     * 
     * <pre>
     * AssertUtils.isNotEmpty(array);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @throws IllegalArgumentException
     *             if the object array is {@code null} or has no elements
     */
    public static void isNotEmpty(Object[] array) {
        isNotEmpty(array, (String) null);
    }

    /**
     * Assert that an array has elements; that is, it must not be {@code null}
     * and must have at least one element.
     * 
     * <pre>
     * AssertUtils.isNotEmpty(array, &quot;The array must have elements&quot;);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the object array is {@code null} or has no elements
     */
    public static void isNotEmpty(Object[] array, final String message, final Object... arguments) {
        isNotEmpty(array, null, message, arguments);
    }

    /**
     * Assert that an array has elements; that is, it must not be {@code null}
     * and must have at least one element.
     * 
     * <pre>
     * AssertUtils.isNotEmpty(array, exceptionToThrowOnError);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the object array is {@code null} or has no elements
     */
    public static <E extends Throwable> void isNotEmpty(Object[] array, final E exception) throws E {
        isNotEmpty(array, exception, null);
    }

    private static <E extends Throwable> void isNotEmpty(Object[] array, final E exception, final String message, final Object... arguments)
            throws E {
        if (ArrayUtils.isEmpty(array)) {
            manageExceptions("this array must not be empty: it must contain at least 1 element", exception, message, new Object[] {array},
                    arguments);
        }
    }

    /**
     * Assert that an array has no null elements.
     * 
     * <pre>
     * AssertUtils.hasNoNullElements(array);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @throws IllegalArgumentException
     *             if the object array contains a {@code null} element
     */
    public static void hasNoNullElements(Object[] array) {
        hasNoNullElements(array, (String) null);
    }

    /**
     * Assert that an array has no null elements.
     * 
     * <pre>
     * AssertUtils.hasNoNullElements(array, &quot;The array must have non-null elements&quot;);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the object array contains a {@code null} element
     */
    public static void hasNoNullElements(Object[] array, final String message, final Object... arguments) {
        hasNoNullElements(array, null, message, arguments);
    }

    /**
     * Assert that an array has no null elements.
     * 
     * <pre>
     * AssertUtils.hasNoNullElements(array, exceptionToThrowOnError);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if object array contains a {@code null} element. The standard
     *             exception is appended as suppressed.
     */
    public static <E extends Throwable> void hasNoNullElements(Object[] array, final E exception) throws E {
        hasNoNullElements(array, exception, null);
    }

    private static <E extends Throwable> void hasNoNullElements(Object[] array, final E exception, final String message,
            final Object... arguments) throws E {

        isNotNull(array);

        for (Object element : array) {
            if (element == null) {
                manageExceptions("this array must not contain any null elements", exception, message, new Object[] {array}, arguments);
            }
        }
    }

    public static <T> void contains(T[] array, final T object) {
        contains(array, object, (Comparator<T>) null);
    }

    public static <T> void contains(T[] array, final T object, final String message, final Object... arguments) {
        contains(array, object, (Comparator<T>) null, null, message, arguments);
    }

    public static <T, E extends Throwable> void contains(T[] array, final T object, final E exception) throws E {
        contains(array, object, (Comparator<T>) null, exception, null);
    }

    public static <T> void contains(T[] array, final T object, final Comparator<T> comparator) {
        contains(array, object, comparator, null);
    }

    public static <T> void contains(T[] array, final T object, final Comparator<T> comparator, final String message,
            final Object... arguments) {
        contains(array, object, comparator, null, message, arguments);
    }

    public static <T, E extends Throwable> void contains(T[] array, final T object, final Comparator<T> comparator, final E exception)
            throws E {
        contains(array, object, comparator, exception, null);
    }

    private static <T, E extends Throwable> void contains(T[] array, final T object, final Comparator<T> comparator, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(array);
        isNotNull(object);

        T[] inputArray = Arrays.copyOf(array, array.length);

        Arrays.sort(inputArray, comparator);

        if (Arrays.binarySearch(inputArray, object, comparator) < 0) {
            manageExceptions("The array must contain the object", exception, message, new Object[] {array, object}, arguments);
        }
    }

    public static <T> void contains(T[] array, T[] objects) {
        contains(array, objects, (Comparator<T>) null, null);
    }

    public static <T> void contains(T[] array, T[] objects, final String message, final Object... arguments) {
        contains(array, objects, (Comparator<T>) null, null, message, arguments);
    }

    public static <T, E extends Throwable> void contains(T[] array, T[] objects, final E exception) throws E {
        contains(array, objects, (Comparator<T>) null, exception, null);
    }

    public static <T> void contains(T[] array, T[] objects, final Comparator<T> comparator) {
        contains(array, objects, comparator, null);
    }

    public static <T> void contains(T[] array, T[] objects, final Comparator<T> comparator, final String message,
            final Object... arguments) {
        contains(array, objects, comparator, null, message, arguments);
    }

    public static <T, E extends Throwable> void contains(T[] array, T[] objects, final Comparator<T> comparator, final E exception)
            throws E {
        contains(array, objects, comparator, exception, null);
    }

    private static <T, E extends Throwable> void contains(T[] array, T[] objects, final Comparator<T> comparator, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(array);
        isNotEmpty(objects);

        T[] inputArray = Arrays.copyOf(array, array.length);
        T[] inputObjects = Arrays.copyOf(objects, objects.length);

        Arrays.sort(inputArray, comparator);
        Arrays.sort(inputObjects, comparator);

        for (T object : inputObjects) {
            if (Arrays.binarySearch(inputArray, object, comparator) < 0) {
                manageExceptions("The array must contain all objects", exception, message, new Object[] {array, objects}, arguments);
            }
        }
    }

    public static <T> void doesNotContain(T[] array, final T object) {
        doesNotContain(array, object, (Comparator<T>) null, null);
    }

    public static <T> void doesNotContain(T[] array, final T object, final String message, final Object... arguments) {
        doesNotContain(array, object, (Comparator<T>) null, null, message, arguments);
    }

    public static <T, E extends Throwable> void doesNotContain(T[] array, final T object, final E exception) throws E {
        doesNotContain(array, object, (Comparator<T>) null, exception, null);
    }

    public static <T> void doesNotContain(T[] array, final T object, final Comparator<T> comparator) {
        doesNotContain(array, object, comparator, null);
    }

    public static <T> void doesNotContain(T[] array, final T object, final Comparator<T> comparator, final String message,
            final Object... arguments) {
        doesNotContain(array, object, comparator, null, message, arguments);
    }

    public static <T, E extends Throwable> void doesNotContain(T[] array, final T object, final Comparator<T> comparator, final E exception)
            throws E {
        doesNotContain(array, object, comparator, exception, null);
    }

    private static <T, E extends Throwable> void doesNotContain(T[] array, final T object, final Comparator<T> comparator,
            final E exception, final String message, final Object... arguments) throws E {

        isNotEmpty(array);
        isNotNull(object);

        T[] inputArray = Arrays.copyOf(array, array.length);

        Arrays.sort(inputArray, comparator);

        if (Arrays.binarySearch(inputArray, object, comparator) > -1) {
            manageExceptions("The array contains the object", exception, message, new Object[] {array, object}, arguments);
        }
    }

    public static <T> void doesNotContain(T[] array, T[] objects) {
        doesNotContain(array, objects, (Comparator<T>) null, null);
    }

    public static <T> void doesNotContain(T[] array, T[] objects, final String message, final Object... arguments) {
        doesNotContain(array, objects, (Comparator<T>) null, null, message, arguments);
    }

    public static <T, E extends Throwable> void doesNotContain(T[] array, T[] objects, final E exception) throws E {
        doesNotContain(array, objects, (Comparator<T>) null, exception, null);
    }

    public static <T> void doesNotContain(T[] array, T[] objects, final Comparator<T> comparator) {
        doesNotContain(array, objects, comparator, null);
    }

    public static <T> void doesNotContain(T[] array, T[] objects, final Comparator<T> comparator, final String message,
            final Object... arguments) {
        doesNotContain(array, objects, comparator, null, message, arguments);
    }

    public static <T, E extends Throwable> void doesNotContain(T[] array, T[] objects, final Comparator<T> comparator, final E exception)
            throws E {
        doesNotContain(array, objects, comparator, exception, null);
    }

    private static <T, E extends Throwable> void doesNotContain(T[] array, T[] objects, final Comparator<T> comparator, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(array);
        isNotEmpty(objects);

        T[] inputArray = Arrays.copyOf(array, array.length);
        T[] inputObjects = Arrays.copyOf(objects, objects.length);

        Arrays.sort(inputArray, comparator);
        Arrays.sort(inputObjects, comparator);

        for (T object : inputObjects) {
            if (Arrays.binarySearch(inputArray, object, comparator) > -1) {
                manageExceptions("The array must not contain any object", exception, message, new Object[] {array, objects}, arguments);
            }
        }
    }
}