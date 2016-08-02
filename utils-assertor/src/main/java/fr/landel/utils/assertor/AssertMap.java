/*-
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
        super(map, TYPE.MAP);
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
        return this.hasSize(size, this.msg(MSG.MAP.SIZE, this.getParam(), this.getNextParam(1, TYPE.NUMBER_INTEGER)));
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
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> hasSize(final int size, final CharSequence message, final Object... arguments) {
        return this.hasSize(size, null, message, arguments);
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
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> hasSize(final int size, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(size >= 0 && this.get() != null, () -> this.get().size() == size, () -> this.msg(MSG.MAP.SIZE, true), message,
                arguments, locale, size);
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
        return this.isEmpty(this.msg(MSG.MAP.EMPTY, this.getParam()));
    }

    /**
     * Asserts that a Map has entries; that is, it must not be {@code null} and
     * must have no entry.
     * 
     * <pre>
     * Assertor.that(map).isEmpty().toThrow();
     * </pre>
     * 
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> isEmpty(final CharSequence message, final Object... arguments) {
        return this.isEmpty(null, message, arguments);
    }

    /**
     * Asserts that a Map has entries; that is, it must not be {@code null} and
     * must have no entry.
     * 
     * <pre>
     * Assertor.that(map).isEmpty().toThrow();
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
    public Operator<AssertMap<K, V>, Map<K, V>> isEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> MapUtils.isEmpty(this.get()), null, message, arguments, locale);
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
        return this.isNotEmpty(this.msg(MSG.MAP.EMPTY + MSG.NOT, this.getParam()));
    }

    /**
     * Asserts that a Map has entries; that is, it must not be {@code null} and
     * must have at least one entry.
     * 
     * <pre>
     * Assertor.that(map).isNotEmpty().toThrow();
     * </pre>
     * 
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> isNotEmpty(final CharSequence message, final Object... arguments) {
        return this.isNotEmpty(null, message, arguments);
    }

    /**
     * Asserts that a Map has entries; that is, it must not be {@code null} and
     * must have at least one entry.
     * 
     * <pre>
     * Assertor.that(map).isNotEmpty().toThrow();
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
    public Operator<AssertMap<K, V>, Map<K, V>> isNotEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> MapUtils.isNotEmpty(this.get()), null, message, arguments, locale);
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
        return this.contains(key, this.msg(MSG.MAP.CONTAINS_KEY, this.getParam(), this.getNextParam(1, TYPE.UNKNOWN)));
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
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> contains(final K key, final CharSequence message, final Object... arguments) {
        return this.contains(key, (Locale) null, message, arguments);
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
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> contains(final K key, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(MapUtils.isNotEmpty(this.get()), () -> this.get().containsKey(key), () -> this.msg(MSG.MAP.CONTAINS_KEY, true),
                message, arguments, locale, key);
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
        return this.contains(key, value,
                this.msg(MSG.MAP.CONTAINS_PAIR, this.getParam(), this.getNextParam(1, TYPE.UNKNOWN), this.getNextParam(2, TYPE.UNKNOWN)));
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
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> contains(final K key, final V value, final CharSequence message,
            final Object... arguments) {
        return this.contains(key, value, null, message, arguments);
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
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> contains(final K key, final V value, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(MapUtils.isNotEmpty(this.get()), () -> this.containsValue(key, value),
                () -> this.msg(MSG.MAP.CONTAINS_PAIR, true), message, arguments, locale, key, value);
    }

    /**
     * Asserts that a Map contains all keys.
     * 
     * <pre>
     * Assertor.that(map).containsAll(keys).toThrow();
     * </pre>
     * 
     * @param keys
     *            the map keys to find (required, not {@code null} and not
     *            empty)
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> containsAll(final Iterable<K> keys) {
        return this.containsAll(keys, this.msg(MSG.MAP.CONTAINS_KEYS_ALL, this.getParam(), this.getNextParam(1, TYPE.ITERABLE)));
    }

    /**
     * Asserts that a Map contains all keys.
     * 
     * <pre>
     * Assertor.that(map).containsAll(keys).toThrow();
     * </pre>
     * 
     * @param keys
     *            the map keys to find (required, not {@code null} and not
     *            empty)
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> containsAll(final Iterable<K> keys, final CharSequence message, final Object... arguments) {
        return this.containsAll(keys, null, message, arguments);
    }

    /**
     * Asserts that a Map contains all keys.
     * 
     * <pre>
     * Assertor.that(map).containsAll(keys).toThrow();
     * </pre>
     * 
     * @param keys
     *            the map keys to find (required, not {@code null} and not
     *            empty)
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> containsAll(final Iterable<K> keys, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.contains(keys, true, locale, message, arguments);
    }

    /**
     * Asserts that a Map contains ny keys.
     * 
     * <pre>
     * Assertor.that(map).containsAny(keys).toThrow();
     * </pre>
     * 
     * @param keys
     *            the map keys to find (required, not {@code null} and not
     *            empty)
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> containsAny(final Iterable<K> keys) {
        return this.containsAny(keys, this.msg(MSG.MAP.CONTAINS_KEYS_ALL, this.getParam(), this.getNextParam(1, TYPE.ITERABLE)));
    }

    /**
     * Asserts that a Map contains any keys.
     * 
     * <pre>
     * Assertor.that(map).containsAny(keys).toThrow();
     * </pre>
     * 
     * @param keys
     *            the map keys to find (required, not {@code null} and not
     *            empty)
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> containsAny(final Iterable<K> keys, final CharSequence message, final Object... arguments) {
        return this.containsAny(keys, null, message, arguments);
    }

    /**
     * Asserts that a Map contains any keys.
     * 
     * <pre>
     * Assertor.that(map).containsAny(keys).toThrow();
     * </pre>
     * 
     * @param keys
     *            the map keys to find (required, not {@code null} and not
     *            empty)
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> containsAny(final Iterable<K> keys, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.contains(keys, false, locale, message, arguments);
    }

    /**
     * Asserts that a Map contains all specified map entries.
     * 
     * <pre>
     * // if 'map' contains all elements from 'map2'
     * Assertor.that(map).containsAll(map2).toThrow();
     * 
     * // not: if 'map' contains a number of elements from 'map2' lower than the
     * // size of 'map2'
     * Assertor.that(map).not().containsAll(map2).toThrow();
     * </pre>
     * 
     * @param objects
     *            the map objects to find (required, not {@code null} and not
     *            empty)
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> containsAll(final Map<K, V> objects) {
        return this.containsAll(objects, this.msg(MSG.MAP.CONTAINS_MAP_ALL));
    }

    /**
     * Asserts that a Map contains all specified map entries.
     * 
     * <pre>
     * // if 'map' contains all elements from 'map2'
     * Assertor.that(map).containsAll(map2).toThrow();
     * 
     * // not: if 'map' contains a number of elements from 'map2' lower than the
     * // size of 'map2'
     * Assertor.that(map).not().containsAll(map2).toThrow();
     * </pre>
     * 
     * @param objects
     *            the map objects to find (required, not {@code null} and not
     *            empty)
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> containsAll(final Map<K, V> objects, final CharSequence message,
            final Object... arguments) {
        return this.containsAll(objects, null, message, arguments);
    }

    /**
     * Asserts that a Map contains all specified map entries.
     * 
     * <pre>
     * // if 'map' contains all elements from 'map2'
     * Assertor.that(map).containsAll(map2).toThrow();
     * 
     * // reverse, if 'map' contains a number of elements from 'map2' lower than
     * // the size of 'map2'
     * Assertor.that(map).not().containsAll(map2).toThrow();
     * </pre>
     * 
     * @param objects
     *            the map objects to find (required, not {@code null} and not
     *            empty)
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> containsAll(final Map<K, V> objects, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.contains(objects, true, locale, message, arguments);
    }

    /**
     * Asserts that a Map contains any specified map entry but not all.
     * 
     * <pre>
     * // if 'map' contains at least one element from 'map2'
     * Assertor.that(map).containsAny(map2).toThrow();
     * 
     * // reverse, if 'map' contains no element from 'map2'
     * Assertor.that(map).not().containsAny(map2).toThrow();
     * </pre>
     * 
     * @param objects
     *            the map objects to find (required, not {@code null} and not
     *            empty)
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> containsAny(final Map<K, V> objects) {
        return this.containsAny(objects, this.msg(MSG.MAP.CONTAINS_MAP_ANY));
    }

    /**
     * Asserts that a Map contains any specified map entry but not all.
     * 
     * <pre>
     * // if 'map' contains at least one element from 'map2'
     * Assertor.that(map).containsAny(map2, "No matching role found").toThrow();
     * 
     * // reverse, if 'map' contains no element from 'map2'
     * Assertor.that(map).not().containsAny(map2, "Some articles are already in your cart").toThrow();
     * </pre>
     * 
     * @param objects
     *            the map objects to find (required, not {@code null} and not
     *            empty)
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> containsAny(final Map<K, V> objects, final CharSequence message,
            final Object... arguments) {
        return this.containsAny(objects, null, message, arguments);
    }

    /**
     * Asserts that a Map contains any specified map entry but not all.
     * 
     * <pre>
     * // if 'map' contains at least one element from 'map2'
     * Assertor.that(map).containsAny(map2, Locale.US, "No matching role found (%.2fms)", 2.326).toThrow();
     * // on error, message -&gt; No matching role found (2.33ms)
     * 
     * // reverse, if 'map' contains no element from 'map2'
     * Assertor.that(map).not().containsAny(map2, Locale.US, "Some articles are already in your cart (%.2fms)", 2.326).toThrow();
     * </pre>
     * 
     * @param objects
     *            the map objects to find (required, not {@code null} and not
     *            empty)
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertMap<K, V>, Map<K, V>> containsAny(final Map<K, V> objects, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.contains(objects, false, locale, message, arguments);
    }

    private Operator<AssertMap<K, V>, Map<K, V>> contains(final Iterable<K> keys, final boolean all, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return this.combine(MapUtils.isNotEmpty(this.get()) && !IterableUtils.isEmpty(keys), () -> {
            int found = 0;
            for (K key : keys) {
                if (this.get().containsKey(key)) {
                    found++;
                }
            }
            if (all ^ this.isNot()) {
                return found == IterableUtils.size(keys);
            } else {
                return found > 0;
            }
        }, () -> {
            if (all) {
                return this.msg(MSG.MAP.CONTAINS_KEYS_ALL, true);
            } else {
                return this.msg(MSG.MAP.CONTAINS_KEYS_ANY, true);
            }
        }, message, arguments, locale, keys);
    }

    private Operator<AssertMap<K, V>, Map<K, V>> contains(final Map<K, V> objects, final boolean all, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return this.combine(MapUtils.isNotEmpty(this.get()) && MapUtils.isNotEmpty(objects), () -> {
            int found = 0;
            for (Entry<K, V> entry : objects.entrySet()) {
                if (this.containsValue(entry.getKey(), entry.getValue())) {
                    found++;
                }
            }
            if (all ^ this.isNot()) {
                return found == objects.size();
            } else {
                return found > 0;
            }
        }, () -> {
            if (all) {
                return this.msg(MSG.MAP.CONTAINS_MAP_ALL, true);
            } else {
                return this.msg(MSG.MAP.CONTAINS_MAP_ANY, true);
            }
        }, message, arguments, locale, objects);
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
