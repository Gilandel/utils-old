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

import org.apache.commons.collections4.IterableUtils;
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
     * Asserts that a Map has the size; that is, it must not be {@code null} and
     * must have the expected size.
     * 
     * <pre>
     * Assertor.that(map).hasSize(5).toThrow();
     * </pre>
     * 
     * @param size
     *            The expected size
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> hasSize(final int size) {
        boolean condition = true;
        final StringBuilder message = new StringBuilder();

        if (size < 0) {
            condition = false;
            message.append("the size parameter has to be greater than or equal to 0");
        } else if (this.get() == null) {
            condition = false;
            message.append("the map cannot be null");
        } else if (this.get().size() != size) {
            condition = false;
            message.append("the map hasn't the expected size");
        }
        return this.combine(condition, message, size);
    }

    /**
     * Asserts that a Map has not the size; that is, it must not be {@code null}
     * and must have the expected size.
     * 
     * <pre>
     * Assertor.that(map).hasNotSize(5).toThrow();
     * </pre>
     * 
     * @param size
     *            The expected size
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> hasNotSize(final int size) {
        boolean condition = true;
        final StringBuilder message = new StringBuilder();

        if (size < 0) {
            condition = false;
            message.append("the size parameter has to be greater than or equal to 0");
        } else if (this.get() == null) {
            condition = false;
            message.append("the map cannot be null");
        } else if (this.get().size() == size) {
            condition = false;
            message.append("the map hasn't the expected size");
        }
        return this.combine(condition, message, size);
    }

    /**
     * Asserts that a Map has entries; that is, it must not be {@code null} and
     * must have no entry.
     * 
     * <pre>
     * Assertor.that(map).isEmpty().toThrow();
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> isEmpty() {
        return this.combine(MapUtils.isEmpty(this.get()), "the map must be empty or null");
    }

    /**
     * Asserts that a Map has entries; that is, it must not be {@code null} and
     * must have at least one entry.
     * 
     * <pre>
     * Assertor.that(map).isNotEmpty().toThrow();
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> isNotEmpty() {
        return this.combine(this.get() != null && !MapUtils.isEmpty(this.get()), "the map must be not empty and not null");
    }

    /**
     * Asserts that a Map contains the specified entry.
     * 
     * <pre>
     * Assertor.that(map).contains(key).toThrow();
     * </pre>
     * 
     * @param key
     *            the map key to find
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> contains(final K key) {
        final StringBuilder message = new StringBuilder();
        boolean condition = this.isEmpty(message);

        if (condition && !this.get().containsKey(key)) {
            condition = false;
            message.append("the map must contain the key");
        }

        return this.combine(condition, message, key);
    }

    /**
     * Asserts that a Map contains the specified entry and the values match.
     * 
     * <pre>
     * Assertor.that(map).contains(key, value).toThrow();
     * </pre>
     * 
     * @param key
     *            the map key to find
     * @param value
     *            The map value to check
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> contains(final K key, final V value) {
        final StringBuilder message = new StringBuilder();
        boolean condition = this.isEmpty(message);

        if (condition && !this.containsValue(key, value)) {
            condition = false;
            message.append("the map must contain the key and the value");
        }

        return this.combine(condition, message, key, value);
    }

    /**
     * Asserts that a Map contains all keys.
     * 
     * <pre>
     * Assertor.that(map).contains(keys).toThrow();
     * </pre>
     * 
     * @param keys
     *            the map keys to find
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> contains(final Iterable<K> keys) {
        final StringBuilder message = new StringBuilder();
        boolean condition = this.isEmpty(message);

        if (condition) {
            if (IterableUtils.isEmpty(keys)) {
                condition = false;
                message.append("the iterable must be not empty or not null");
            } else {
                for (K key : keys) {
                    if (!this.get().containsKey(key)) {
                        condition = false;
                        message.append("the map must contain all keys");
                        break;
                    }
                }
            }
        }

        return this.combine(condition, message, keys);

    }

    /**
     * Asserts that a Map contains all specified map entries.
     * 
     * <pre>
     * Assertor.that(map).contains(map2).toThrow();
     * </pre>
     * 
     * @param objects
     *            the map objects to find
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> contains(final Map<K, V> objects) {
        final StringBuilder message = new StringBuilder();
        boolean condition = this.isEmpty(message);

        if (condition) {
            if (MapUtils.isEmpty(objects)) {
                condition = false;
                message.append("the iterable must be not empty or not null");
            } else {
                for (Entry<K, V> entry : objects.entrySet()) {
                    if (!this.containsValue(entry.getKey(), entry.getValue())) {
                        condition = false;
                        message.append("the map must contain all the keys and the values");
                        break;
                    }
                }
            }
        }

        return this.combine(condition, message, objects);
    }

    /**
     * Asserts that a Map doesn't contain the specified entry.
     * 
     * <pre>
     * Assertor.that(map).doesNotContain(key).toThrow();
     * </pre>
     * 
     * @param key
     *            the map key to find
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> doesNotContain(final K key) {
        final StringBuilder message = new StringBuilder();
        boolean condition = this.isEmpty(message);

        if (condition && this.get().containsKey(key)) {
            condition = false;
            message.append("the map must not contain the key");
        }

        return this.combine(condition, message, key);
    }

    /**
     * Asserts that a Map contains the specified key/value pair.
     * 
     * <pre>
     * Assertor.that(map).doesNotContain(key, value).toThrow();
     * </pre>
     * 
     * @param key
     *            the map key to find
     * @param value
     *            The map value to check
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> doesNotContain(final K key, final V value) {
        final StringBuilder message = new StringBuilder();
        boolean condition = this.isEmpty(message);

        if (condition && this.containsValue(key, value)) {
            condition = false;
            message.append("the map must not contain the key/value pair");
        }

        return this.combine(condition, message, key, value);
    }

    /**
     * Asserts that a Map doesn't contain any keys.
     * 
     * <pre>
     * Assertor.that(map).doesNotContain(keys).toThrow();
     * </pre>
     * 
     * @param keys
     *            the map keys to find
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> doesNotContain(final Iterable<K> keys) {
        final StringBuilder message = new StringBuilder();
        boolean condition = this.isEmpty(message);

        if (condition) {
            if (IterableUtils.isEmpty(keys)) {
                condition = false;
                message.append("the iterable must be not empty or not null");
            } else {
                for (K key : keys) {
                    if (this.get().containsKey(key)) {
                        condition = false;
                        message.append("the map must not contain any keys");
                        break;
                    }
                }
            }
        }

        return this.combine(condition, message, keys);
    }

    /**
     * Asserts that a Map doesn't contain any specified map entries.
     * 
     * <pre>
     * Assertor.that(map).doesNotContain(map2).toThrow();
     * </pre>
     * 
     * @param objects
     *            the map objects to find
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> doesNotContain(final Map<K, V> objects) {
        final StringBuilder message = new StringBuilder();
        boolean condition = this.isEmpty(message);

        if (condition) {
            if (MapUtils.isEmpty(objects)) {
                condition = false;
                message.append("the iterable must be not empty or not null");
            } else {
                for (Entry<K, V> entry : objects.entrySet()) {
                    if (this.containsValue(entry.getKey(), entry.getValue())) {
                        condition = false;
                        message.append("the map must not contain any key/value pair");
                        break;
                    }
                }
            }
        }

        return this.combine(condition, message, objects);
    }

    private boolean isEmpty(final StringBuilder message) {
        boolean condition = true;

        if (MapUtils.isEmpty(this.get())) {
            condition = false;
            message.append("the map must be not empty or not null");
        }

        return condition;
    }

    private boolean containsValue(final K key, final V value) {
        if (this.get().containsKey(key)) {
            V val = this.get().get(key);
            if (val != null && val.equals(value)) {
                return true;
            } else if (val == null && value == null) {
                return true;
            }
        }
        return false;
    }
}