/*-
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
 * Assertion utility class that assists in validating arguments for maps.
 *
 * @since 5 juin 2016
 * @author Gilles
 *
 * @param <K>
 *            the type of the map key
 * @param <V>
 *            the type of the map value
 */
public class AssertMap<K, V> extends AssertObject<AssertMap<K, V>, Map<K, V>> {

    /**
     * 
     * Constructor
     *
     * @param map
     *            The map to check
     */
    protected AssertMap(final Map<K, V> map) {
        super(map);
    }

    /**
     * Assert that a Map has the size; that is, it must not be {@code null} and
     * must have the expected size.
     * 
     * @param size
     *            The expected size
     * @return this
     * @throws IllegalArgumentException
     *             if the map is {@code null} or has size not matching
     */
    public AssertMap<K, V> hasSize(final int size) {
        return this.hasSize(size, (String) null);
    }

    /**
     * Assert that a Map has the size; that is, it must not be {@code null} and
     * must have the expected size.
     * 
     * @param size
     *            The expected size
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if the map is {@code null} or has size not matching
     */
    public AssertMap<K, V> hasSize(final int size, final String message, final Object... arguments) {
        hasSize(this.get(), size, null, message, arguments);

        return this;
    }

    /**
     * Assert that a Map has the size; that is, it must not be {@code null} and
     * must have the expected size.
     * 
     * @param size
     *            The expected size
     * @param exception
     *            The specific exception
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the map is {@code null} or has the expected size
     */
    public <E extends Throwable> AssertMap<K, V> hasSize(final int size, final E exception) throws E {
        hasSize(this.get(), size, exception, null);

        return this;
    }

    private static <K, V, E extends Throwable> void hasSize(final Map<K, V> map, final int size, final E exception, final String message,
            final Object... arguments) throws E {

        AssertNumber.isGTE(size, 0, null, "The size parameter has to be greater than or equal to 0");

        if (map == null || map.size() != size) {
            manageExceptions("this map hasn't the expected size", exception, message, new Object[] {map}, arguments);
        }
    }

    /**
     * Assert that a Map has entries; that is, it must not be {@code null} and
     * must have no entry.
     * 
     * <pre>
     * AssertUtils.check(map).isEmpty();
     * </pre>
     * 
     * @return this
     * @throws IllegalArgumentException
     *             if the map is {@code null} or has entries
     */
    public AssertMap<K, V> isEmpty() {
        return this.isEmpty((String) null);
    }

    /**
     * Assert that a Map has entries; that is, it must not be {@code null} and
     * must have no entry.
     * 
     * <pre>
     * AssertUtils.check(map).isEmpty(&quot;Map must have entries&quot;);
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
     *             if the map is {@code null} or has entries
     */
    public AssertMap<K, V> isEmpty(final String message, final Object... arguments) {
        isEmpty(this.get(), null, message, arguments);

        return this.getThis();
    }

    /**
     * Assert that a Map has entries; that is, it must not be {@code null} and
     * must have no entry.
     * 
     * <pre>
     * AssertUtils.check(map).isEmpty(exceptionToThrowOnError);
     * </pre>
     * 
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the map is {@code null} or has entries
     */
    public <E extends Throwable> AssertMap<K, V> isEmpty(final E exception) throws E {
        isEmpty(this.get(), exception, null);

        return this.getThis();
    }

    protected static <K, V, E extends Throwable> void isEmpty(final Map<K, V> map, final E exception, final String message,
            final Object... arguments) throws E {
        if (map == null || !MapUtils.isEmpty(map)) {
            manageExceptions("this map must be empty and not null", exception, message, new Object[] {map}, arguments);
        }
    }

    /**
     * Assert that a Map has entries; that is, it must not be {@code null} and
     * must have at least one entry.
     * 
     * <pre>
     * AssertUtils.check(map).isNotEmpty();
     * </pre>
     * 
     * @return this
     * @throws IllegalArgumentException
     *             if the map is {@code null} or has no entries
     */
    public AssertMap<K, V> isNotEmpty() {
        return this.isNotEmpty((String) null);
    }

    /**
     * Assert that a Map has entries; that is, it must not be {@code null} and
     * must have at least one entry.
     * 
     * <pre>
     * AssertUtils.check(map).isNotEmpty(&quot;Map must have entries&quot;);
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
     *             if the map is {@code null} or has no entries
     */
    public AssertMap<K, V> isNotEmpty(final String message, final Object... arguments) {
        isNotEmpty(this.get(), null, message, arguments);

        return this.getThis();
    }

    /**
     * Assert that a Map has entries; that is, it must not be {@code null} and
     * must have at least one entry.
     * 
     * <pre>
     * AssertUtils.check(map).isNotEmpty(exceptionToThrowOnError);
     * </pre>
     * 
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the map is {@code null} or has no entries
     */
    public <E extends Throwable> AssertMap<K, V> isNotEmpty(final E exception) throws E {
        isNotEmpty(this.get(), exception, null);

        return this.getThis();
    }

    protected static <K, V, E extends Throwable> void isNotEmpty(final Map<K, V> map, final E exception, final String message,
            final Object... arguments) throws E {
        if (MapUtils.isEmpty(map)) {
            manageExceptions("this map must not be empty; it must contain at least one entry", exception, message, new Object[] {map},
                    arguments);
        }
    }

    public AssertMap<K, V> contains(final K key) {
        return this.contains(key, (IllegalArgumentException) null);
    }

    public AssertMap<K, V> contains(final K key, final String message, final Object... arguments) {
        contains(this.get(), key, (IllegalArgumentException) null, message, arguments);

        return this.getThis();
    }

    public <E extends Throwable> AssertMap<K, V> contains(final K key, final E exception) throws E {
        contains(this.get(), key, exception, null);

        return this.getThis();
    }

    protected static <K, V, E extends Throwable> void contains(final Map<K, V> map, final K key, final E exception, final String message,
            final Object... arguments) throws E {

        isNotEmpty(map, exception, message, arguments);

        if (!map.containsKey(key)) {
            manageExceptions("The map must contain the key", exception, message, new Object[] {map, key}, arguments);
        }
    }

    public AssertMap<K, V> contains(final Iterable<K> keys) {
        return this.contains(keys, null);
    }

    public AssertMap<K, V> contains(final Iterable<K> keys, final String message, final Object... arguments) {
        contains(this.get(), keys, null, message, arguments);

        return this.getThis();
    }

    public <E extends Throwable> AssertMap<K, V> contains(final Iterable<K> keys, final E exception) throws E {
        contains(this.get(), keys, exception, null);

        return this.getThis();
    }

    protected static <K, V, E extends Throwable> void contains(final Map<K, V> map, final Iterable<K> keys, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(map, exception, message, arguments);
        // TODO isNotEmpty(keys);

        for (K key : keys) {
            if (!map.containsKey(key)) {
                manageExceptions("The map must contain all keys", exception, message, new Object[] {map, keys}, arguments);
            }
        }
    }

    public AssertMap<K, V> contains(final K key, final V value) {
        return this.contains(key, value, (IllegalArgumentException) null);
    }

    public AssertMap<K, V> contains(final K key, final V value, final String message, final Object... arguments) {
        contains(this.get(), key, value, null, message, arguments);

        return this.getThis();
    }

    public <E extends Throwable> AssertMap<K, V> contains(final K key, final V value, final E exception) throws E {
        contains(this.get(), key, value, (IllegalArgumentException) null, null);

        return this.getThis();
    }

    protected static <K, V, E extends Throwable> void contains(final Map<K, V> map, final K key, final V value, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(map, exception, message, arguments);

        if (!map.containsKey(key) || !map.get(key).equals(value)) {
            manageExceptions("The map must contain the key and the value", exception, message, new Object[] {map, key, value}, arguments);
        }
    }

    public AssertMap<K, V> contains(final Map<K, V> objects) {
        return this.contains(objects, null);
    }

    public AssertMap<K, V> contains(final Map<K, V> objects, final String message, final Object... arguments) {
        contains(this.get(), objects, null, message, arguments);

        return this.getThis();
    }

    public <E extends Throwable> AssertMap<K, V> contains(final Map<K, V> objects, final E exception) throws E {
        contains(this.get(), objects, exception, null);

        return this.getThis();
    }

    protected static <K, V, E extends Throwable> void contains(final Map<K, V> map, final Map<K, V> objects, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(map, exception, message, arguments);
        new AssertMap<>(objects).isNotEmpty();

        for (Entry<K, V> entry : objects.entrySet()) {
            if (!map.containsKey(entry.getKey()) || !map.get(entry.getKey()).equals(entry.getValue())) {
                manageExceptions("The map must contain all the keys and the values", exception, message, new Object[] {map, objects},
                        arguments);
            }
        }
    }

    public AssertMap<K, V> doesNotContain(final K key) {
        return this.doesNotContain(key, (IllegalArgumentException) null);
    }

    public AssertMap<K, V> doesNotContain(final K key, final String message, final Object... arguments) {
        doesNotContain(this.get(), key, (IllegalArgumentException) null, message, arguments);

        return this.getThis();
    }

    public <E extends Throwable> AssertMap<K, V> doesNotContain(final K key, final E exception) throws E {
        doesNotContain(this.get(), key, exception, null);

        return this.getThis();
    }

    protected static <K, V, E extends Throwable> void doesNotContain(final Map<K, V> map, final K key, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(map, exception, message, arguments);

        if (map.containsKey(key)) {
            manageExceptions("The map must not contain the key", exception, message, new Object[] {map, key}, arguments);
        }
    }

    public AssertMap<K, V> doesNotContain(final Iterable<K> keys) {
        return this.doesNotContain(keys, null);
    }

    public AssertMap<K, V> doesNotContain(final Iterable<K> keys, final String message, final Object... arguments) {
        doesNotContain(this.get(), keys, null, message, arguments);

        return this.getThis();
    }

    public <E extends Throwable> AssertMap<K, V> doesNotContain(final Iterable<K> keys, final E exception) throws E {
        doesNotContain(this.get(), keys, exception, null);

        return this.getThis();
    }

    protected static <K, V, E extends Throwable> void doesNotContain(final Map<K, V> map, final Iterable<K> keys, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(map, exception, message, arguments);
        // TODO isNotEmpty(keys);

        for (K key : keys) {
            if (map.containsKey(key)) {
                manageExceptions("The map must not contain any key", exception, message, new Object[] {map, keys}, arguments);
            }
        }
    }

    public AssertMap<K, V> doesNotContain(final K key, final V value) {
        return this.doesNotContain(key, value, null);
    }

    public AssertMap<K, V> doesNotContain(final K key, final V value, final String message, final Object... arguments) {
        doesNotContain(this.get(), key, value, null, message, arguments);

        return this.getThis();
    }

    public <E extends Throwable> AssertMap<K, V> doesNotContain(final K key, final V value, final E exception) throws E {
        doesNotContain(this.get(), key, value, exception, null);

        return this.getThis();
    }

    protected static <K, V, E extends Throwable> void doesNotContain(final Map<K, V> map, final K key, final V value, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(map, exception, message, arguments);

        if (map.containsKey(key) && map.get(key).equals(value)) {
            manageExceptions("The map must not contain the key/value pair", exception, message, new Object[] {map, key, value}, arguments);
        }
    }

    public AssertMap<K, V> doesNotContain(final Map<K, V> objects) {
        return this.doesNotContain(objects, null);
    }

    public AssertMap<K, V> doesNotContain(final Map<K, V> objects, final String message, final Object... arguments) {
        doesNotContain(this.get(), objects, null, message, arguments);

        return this.getThis();
    }

    public <E extends Throwable> AssertMap<K, V> doesNotContain(final Map<K, V> objects, final E exception) throws E {
        doesNotContain(this.get(), objects, exception, null);

        return this.getThis();
    }

    protected static <K, V, E extends Throwable> void doesNotContain(final Map<K, V> map, final Map<K, V> objects, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(map, exception, message, arguments);
        isNotEmpty(objects, exception, message, arguments);

        for (Entry<K, V> entry : objects.entrySet()) {
            if (map.containsKey(entry.getKey()) && map.get(entry.getKey()).equals(entry.getValue())) {
                manageExceptions("The map must not contain any key/value pair", exception, message, new Object[] {map, objects}, arguments);
            }
        }
    }
}