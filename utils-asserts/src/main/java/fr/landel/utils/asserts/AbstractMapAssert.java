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

import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.collections4.MapUtils;

/**
 * Assertion utility class that assists in validating arguments for iterables.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public abstract class AbstractMapAssert extends AbstractIterableAssert {

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
