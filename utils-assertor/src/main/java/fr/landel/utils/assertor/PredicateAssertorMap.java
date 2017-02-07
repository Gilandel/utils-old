/*-
 * #%L
 * utils-assertor
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
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

/**
 * This class define methods that can be applied on the checked {@link Map}
 * object. To provide a result, it's also provide a chain builder by returning a
 * {@link PredicateStepMap}. The chain looks like:
 * 
 * <pre>
 * {@link PredicateAssertorMap} &gt; {@link PredicateStepMap} &gt; {@link PredicateAssertorMap} &gt; {@link PredicateStepMap}...
 * </pre>
 * 
 * This chain always starts with a {@link PredicateAssertorMap} and ends with
 * {@link PredicateStepMap}.
 *
 * @since Aug 3, 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateAssertorMap<K, V> extends PredicateAssertor<PredicateStepMap<K, V>, Map<K, V>> {

    /**
     * {@inheritDoc}
     */
    @Override
    default PredicateStepMap<K, V> get(final StepAssertor<Map<K, V>> result) {
        return () -> result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    default PredicateAssertorMap<K, V> not() {
        return () -> HelperAssertor.not(getStep());
    }

    default PredicateStepMap<K, V> hasSize(final int size) {
        return this.hasSize(size, null);
    }

    default PredicateStepMap<K, V> hasSize(final int size, final CharSequence message, final Object... arguments) {
        return this.hasSize(size, null, message, arguments);
    }

    default PredicateStepMap<K, V> hasSize(final int size, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorMap.hasSize(this.getStep(), size, Message.of(locale, message, arguments));
    }

    default PredicateStepMap<K, V> isEmpty() {
        return this.isEmpty(null);
    }

    default PredicateStepMap<K, V> isEmpty(final CharSequence message, final Object... arguments) {
        return this.isEmpty(null, message, arguments);
    }

    default PredicateStepMap<K, V> isEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorMap.isEmpty(this.getStep(), Message.of(locale, message, arguments));
    }

    default PredicateStepMap<K, V> isNotEmpty() {
        return this.isNotEmpty(null);
    }

    default PredicateStepMap<K, V> isNotEmpty(final CharSequence message, final Object... arguments) {
        return this.isNotEmpty(null, message, arguments);
    }

    default PredicateStepMap<K, V> isNotEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorMap.isNotEmpty(this.getStep(), Message.of(locale, message, arguments));
    }

    default PredicateStepMap<K, V> contains(final K key) {
        return this.contains(key, (CharSequence) null);
    }

    default PredicateStepMap<K, V> contains(final K key, final CharSequence message, final Object... arguments) {
        return this.contains(key, (Locale) null, message, arguments);
    }

    default PredicateStepMap<K, V> contains(final K key, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorMap.contains(this.getStep(), key, Message.of(locale, message, arguments));
    }

    default PredicateStepMap<K, V> contains(final K key, final V value) {
        return this.contains(key, value, null);
    }

    default PredicateStepMap<K, V> contains(final K key, final V value, final CharSequence message, final Object... arguments) {
        return this.contains(key, value, null, message, arguments);
    }

    default PredicateStepMap<K, V> contains(final K key, final V value, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorMap.contains(this.getStep(), key, value, Message.of(locale, message, arguments));
    }

    default PredicateStepMap<K, V> containsAll(final Iterable<K> keys) {
        return this.containsAll(keys, null);
    }

    default PredicateStepMap<K, V> containsAll(final Iterable<K> keys, final CharSequence message, final Object... arguments) {
        return this.containsAll(keys, null, message, arguments);
    }

    default PredicateStepMap<K, V> containsAll(final Iterable<K> keys, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorMap.containsAll(this.getStep(), keys, Message.of(locale, message, arguments));
    }

    default PredicateStepMap<K, V> containsAll(final Map<K, V> map) {
        return this.containsAll(map, null);
    }

    default PredicateStepMap<K, V> containsAll(final Map<K, V> map, final CharSequence message, final Object... arguments) {
        return this.containsAll(map, null, message, arguments);
    }

    default PredicateStepMap<K, V> containsAll(final Map<K, V> map, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorMap.containsAll(this.getStep(), map, Message.of(locale, message, arguments));
    }

    default PredicateStepMap<K, V> containsAny(final Iterable<K> keys) {
        return this.containsAny(keys, null);
    }

    default PredicateStepMap<K, V> containsAny(final Iterable<K> keys, final CharSequence message, final Object... arguments) {
        return this.containsAny(keys, null, message, arguments);
    }

    default PredicateStepMap<K, V> containsAny(final Iterable<K> keys, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorMap.containsAny(this.getStep(), keys, Message.of(locale, message, arguments));
    }

    default PredicateStepMap<K, V> containsAny(final Map<K, V> map) {
        return this.containsAny(map, null);
    }

    default PredicateStepMap<K, V> containsAny(final Map<K, V> map, final CharSequence message, final Object... arguments) {
        return this.containsAny(map, null, message, arguments);
    }

    default PredicateStepMap<K, V> containsAny(final Map<K, V> map, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorMap.containsAny(this.getStep(), map, Message.of(locale, message, arguments));
    }
}
