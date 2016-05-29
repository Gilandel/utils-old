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
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.collections4.IterableUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.ArrayUtils;

import fr.landel.utils.commons.CollectionUtils2;

/**
 * Assertion utility class that assists in validating arguments for arrays and
 * iterables.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public abstract class AbstractIterableAssert extends AbstractDateAssert {

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

    /**
     * Assert that a Map has entries; that is, it must not be {@code null} and
     * must have at least one entry.
     * 
     * <pre>
     * AssertUtils.notEmpty(map);
     * </pre>
     * 
     * @param map
     *            the map to check
     * @throws IllegalArgumentException
     *             if the map is {@code null} or has no entries
     */
    public static void isNotEmpty(final Map<?, ?> map) {
        isNotEmpty(map, (String) null);
    }

    /**
     * Assert that a Map has entries; that is, it must not be {@code null} and
     * must have at least one entry.
     * 
     * <pre>
     * AssertUtils.isNotEmpty(map, &quot;Map must have entries&quot;);
     * </pre>
     * 
     * @param map
     *            the map to check
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the map is {@code null} or has no entries
     */
    public static void isNotEmpty(final Map<?, ?> map, final String message, final Object... arguments) {
        isNotEmpty(map, null, message, arguments);
    }

    /**
     * Assert that a Map has entries; that is, it must not be {@code null} and
     * must have at least one entry.
     * 
     * <pre>
     * AssertUtils.isNotEmpty(map, exceptionToThrowOnError);
     * </pre>
     * 
     * @param map
     *            the map to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the map is {@code null} or has no entries
     */
    public static <E extends Throwable> void isNotEmpty(final Map<?, ?> map, final E exception) throws E {
        isNotEmpty(map, exception, null);
    }

    private static <E extends Throwable> void isNotEmpty(final Map<?, ?> map, final E exception, final String message,
            final Object... arguments) throws E {
        if (MapUtils.isEmpty(map)) {
            manageExceptions("this map must not be empty; it must contain at least one entry", exception, message, new Object[] {map},
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

    public static <K, V> void contains(final Map<K, V> map, final K key) {
        contains(map, key, (IllegalArgumentException) null);
    }

    public static <K, V> void contains(final Map<K, V> map, final K key, final String message, final Object... arguments) {
        contains(map, key, (IllegalArgumentException) null, message, arguments);
    }

    public static <K, V, E extends Throwable> void contains(final Map<K, V> map, final K key, final E exception) throws E {
        contains(map, key, exception, null);
    }

    private static <K, V, E extends Throwable> void contains(final Map<K, V> map, final K key, final E exception, final String message,
            final Object... arguments) throws E {

        isNotEmpty(map);

        if (!map.containsKey(key)) {
            manageExceptions("The map must contain the key", exception, message, new Object[] {map, key}, arguments);
        }
    }

    public static <K, V> void contains(final Map<K, V> map, final Iterable<K> keys) {
        contains(map, keys, null);
    }

    public static <K, V> void contains(final Map<K, V> map, final Iterable<K> keys, final String message, final Object... arguments) {
        contains(map, keys, null, message, arguments);
    }

    public static <K, V, E extends Throwable> void contains(final Map<K, V> map, final Iterable<K> keys, final E exception) throws E {
        contains(map, keys, exception, null);
    }

    private static <K, V, E extends Throwable> void contains(final Map<K, V> map, final Iterable<K> keys, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(map);
        isNotEmpty(keys);

        for (K key : keys) {
            if (!map.containsKey(key)) {
                manageExceptions("The map must contain all keys", exception, message, new Object[] {map, keys}, arguments);
            }
        }
    }

    public static <K, V> void contains(final Map<K, V> map, final K key, final V value) {
        contains(map, key, value, (IllegalArgumentException) null, null);
    }

    public static <K, V> void contains(final Map<K, V> map, final K key, final V value, final String message, final Object... arguments) {
        contains(map, key, value, null, message, arguments);
    }

    public static <K, V, E extends Throwable> void contains(final Map<K, V> map, final K key, final V value, final E exception) throws E {
        contains(map, key, value, (IllegalArgumentException) null, null);
    }

    private static <K, V, E extends Throwable> void contains(final Map<K, V> map, final K key, final V value, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(map);

        if (!map.containsKey(key) || (map.get(key) != null && !map.get(key).equals(value)) || value != null) {
            manageExceptions("The map must contain the key and the value", exception, message, new Object[] {map, key, value}, arguments);
        }
    }

    public static <K, V> void contains(final Map<K, V> map, final Map<K, V> objects) {
        contains(map, objects, null);
    }

    public static <K, V> void contains(final Map<K, V> map, final Map<K, V> objects, final String message, final Object... arguments) {
        contains(map, objects, null, message, arguments);
    }

    public static <K, V, E extends Throwable> void contains(final Map<K, V> map, final Map<K, V> objects, final E exception) throws E {
        contains(map, objects, exception, null);
    }

    private static <K, V, E extends Throwable> void contains(final Map<K, V> map, final Map<K, V> objects, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(map);
        isNotEmpty(objects);

        for (Entry<K, V> entry : objects.entrySet()) {
            if (!map.containsKey(entry.getKey()) || (map.get(entry.getKey()) != null && !map.get(entry.getKey()).equals(entry.getValue()))
                    || entry.getValue() != null) {
                manageExceptions("The map must contain all the keys and the values", exception, message, new Object[] {map, objects},
                        arguments);
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

    public static <K, V> void doesNotContain(final Map<K, V> map, final K key) {
        doesNotContain(map, key, (IllegalArgumentException) null);
    }

    public static <K, V> void doesNotContain(final Map<K, V> map, final K key, final String message, final Object... arguments) {
        doesNotContain(map, key, (IllegalArgumentException) null, message, arguments);
    }

    public static <K, V, T, E extends Throwable> void doesNotContain(final Map<K, V> map, final K key, final E exception) throws E {
        doesNotContain(map, key, exception, null);
    }

    private static <K, V, T, E extends Throwable> void doesNotContain(final Map<K, V> map, final K key, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(map);

        if (map.containsKey(key)) {
            manageExceptions("The map must not contain the key", exception, message, new Object[] {map, key}, arguments);
        }
    }

    public static <K, V> void doesNotContain(final Map<K, V> map, final Iterable<K> keys) {
        doesNotContain(map, keys, null);
    }

    public static <K, V> void doesNotContain(final Map<K, V> map, final Iterable<K> keys, final String message, final Object... arguments) {
        doesNotContain(map, keys, null, message, arguments);
    }

    public static <K, V, E extends Throwable> void doesNotContain(final Map<K, V> map, final Iterable<K> keys, final E exception) throws E {
        doesNotContain(map, keys, exception, null);
    }

    private static <K, V, E extends Throwable> void doesNotContain(final Map<K, V> map, final Iterable<K> keys, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(map);
        isNotEmpty(keys);

        for (K key : keys) {
            if (map.containsKey(key)) {
                manageExceptions("The map must not contain any key", exception, message, new Object[] {map, keys}, arguments);
            }
        }
    }

    public static <K, V> void doesNotContain(final Map<K, V> map, final K key, final V value) {
        doesNotContain(map, key, value, null);
    }

    public static <K, V> void doesNotContain(final Map<K, V> map, final K key, final V value, final String message,
            final Object... arguments) {
        doesNotContain(map, key, value, null, message, arguments);
    }

    public static <K, V, T, E extends Throwable> void doesNotContain(final Map<K, V> map, final K key, final V value, final E exception)
            throws E {
        doesNotContain(map, key, value, exception, null);
    }

    private static <K, V, T, E extends Throwable> void doesNotContain(final Map<K, V> map, final K key, final V value, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(map);

        if (map.containsKey(key) && ((map.get(key) == null && value == null) || map.get(key).equals(value))) {
            manageExceptions("The map must not contain the key/value pair", exception, message, new Object[] {map, key, value}, arguments);
        }
    }

    public static <K, V> void doesNotContain(final Map<K, V> map, final Map<K, V> objects) {
        doesNotContain(map, objects, null);
    }

    public static <K, V> void doesNotContain(final Map<K, V> map, final Map<K, V> objects, final String message,
            final Object... arguments) {
        doesNotContain(map, objects, null, message, arguments);
    }

    public static <K, V, E extends Throwable> void doesNotContain(final Map<K, V> map, final Map<K, V> objects, final E exception)
            throws E {
        doesNotContain(map, objects, exception, null);
    }

    private static <K, V, E extends Throwable> void doesNotContain(final Map<K, V> map, final Map<K, V> objects, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(map);
        isNotEmpty(objects);

        for (Entry<K, V> entry : objects.entrySet()) {
            if (map.containsKey(entry.getKey()) || ((map.get(entry.getKey()) == null && entry.getValue() == null)
                    || map.get(entry.getKey()).equals(entry.getValue()))) {
                manageExceptions("The map must not contain any key/value pair", exception, message, new Object[] {map, objects}, arguments);
            }
        }
    }
}
