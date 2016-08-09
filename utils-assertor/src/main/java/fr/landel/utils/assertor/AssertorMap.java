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
import java.util.function.BiFunction;
import java.util.function.Function;

import org.apache.commons.collections4.IterableUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.TriFunction;

public class AssertorMap extends Constants {

    protected static <K, V, E extends Throwable> AssertorResult<Map<K, V>> hasSize(final AssertorResult<Map<K, V>> result, final int size,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<Map<K, V>, Boolean> precondition = (map) -> size >= 0 && map != null;

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage
                .getDefaultMessage(result, MSG.MAP.SIZE, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Map<K, V>, Boolean, Boolean, E> checker = (map, not) -> map.size() == size;

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.MAP.SIZE, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(size, EnumType.NUMBER_INTEGER));
    }

    protected static <K, V, E extends Throwable> AssertorResult<Map<K, V>> isEmpty(final AssertorResult<Map<K, V>> result,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<Map<K, V>, Boolean, Boolean, E> checker = (map, not) -> MapUtils.isEmpty(map);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.MAP.EMPTY, not, objectIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false);
    }

    protected static <K, V, E extends Throwable> AssertorResult<Map<K, V>> isNotEmpty(final AssertorResult<Map<K, V>> result,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<Map<K, V>, Boolean, Boolean, E> checker = (map, not) -> MapUtils.isNotEmpty(map);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.MAP.EMPTY, !not, objectIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false);
    }

    protected static <K, V, E extends Throwable> AssertorResult<Map<K, V>> containsAll(final AssertorResult<Map<K, V>> result,
            final Map<K, V> map, final Locale locale, final CharSequence message, final Object[] arguments) {

        return contains(result, map, MSG.MAP.CONTAINS_MAP_ALL, true, locale, message, arguments);
    }

    protected static <K, V, E extends Throwable> AssertorResult<Map<K, V>> containsAll(final AssertorResult<Map<K, V>> result,
            final Iterable<K> keys, final Locale locale, final CharSequence message, final Object[] arguments) {

        return contains(result, keys, MSG.MAP.CONTAINS_KEYS_ALL, true, locale, message, arguments);
    }

    protected static <K, V, E extends Throwable> AssertorResult<Map<K, V>> containsAny(final AssertorResult<Map<K, V>> result,
            final Map<K, V> map, final Locale locale, final CharSequence message, final Object[] arguments) {

        return contains(result, map, MSG.MAP.CONTAINS_MAP_ANY, false, locale, message, arguments);
    }

    protected static <K, V, E extends Throwable> AssertorResult<Map<K, V>> containsAny(final AssertorResult<Map<K, V>> result,
            final Iterable<K> keys, final Locale locale, final CharSequence message, final Object[] arguments) {

        return contains(result, keys, MSG.MAP.CONTAINS_KEYS_ANY, false, locale, message, arguments);
    }

    protected static <K, V, E extends Throwable> AssertorResult<Map<K, V>> contains(final AssertorResult<Map<K, V>> result,
            final Iterable<K> keys, final CharSequence key, final boolean all, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final Function<Map<K, V>, Boolean> precondition = (map) -> MapUtils.isNotEmpty(map) && !IterableUtils.isEmpty(keys);

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage
                .getDefaultMessage(result, key, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Map<K, V>, Boolean, Boolean, E> checker = (map, not) -> AssertorMap.contains(map, keys, all, not);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, key, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, true,
                Pair.of(keys, EnumType.ITERABLE));
    }

    protected static <K, V, E extends Throwable> AssertorResult<Map<K, V>> contains(final AssertorResult<Map<K, V>> result,
            final Map<K, V> map, final CharSequence key, final boolean all, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final Function<Map<K, V>, Boolean> precondition = (map1) -> MapUtils.isNotEmpty(map1) && MapUtils.isNotEmpty(map);

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage
                .getDefaultMessage(result, key, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Map<K, V>, Boolean, Boolean, E> checker = (map1, not) -> AssertorMap.contains(map1, map, all, not);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, key, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, true, Pair.of(map, EnumType.MAP));
    }

    protected static <K, V, E extends Throwable> AssertorResult<Map<K, V>> contains(final AssertorResult<Map<K, V>> result, final K key,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<Map<K, V>, Boolean> precondition = (map) -> MapUtils.isNotEmpty(map);

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage
                .getDefaultMessage(result, MSG.MAP.CONTAINS_KEY, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Map<K, V>, Boolean, Boolean, E> checker = (map, not) -> map.containsKey(key);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.MAP.CONTAINS_KEY, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(key, EnumType.getType(key)));
    }

    protected static <K, V, E extends Throwable> AssertorResult<Map<K, V>> contains(final AssertorResult<Map<K, V>> result, final K key,
            final V value, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<Map<K, V>, Boolean> precondition = (map) -> MapUtils.isNotEmpty(map);

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage
                .getDefaultMessage(result, MSG.MAP.CONTAINS_PAIR, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Map<K, V>, Boolean, Boolean, E> checker = (map, not) -> AssertorMap.contains(map, key, value);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.MAP.CONTAINS_PAIR, not, objectIndex, paramIndex, paramIndex + 1);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(key, EnumType.getType(key)), Pair.of(value, EnumType.getType(value)));
    }

    private static <K, V> boolean contains(final Map<K, V> map, final Iterable<K> keys, final boolean all, final boolean not) {
        int found = 0;
        for (K key : keys) {
            if (map.containsKey(key)) {
                found++;
            }
        }

        return HelperAssertor.isValid(all, not, found, IterableUtils.size(keys));
    }

    private static <K, V> boolean contains(final Map<K, V> map, final Map<K, V> objects, final boolean all, final boolean not) {
        int found = 0;
        for (Entry<K, V> entry : objects.entrySet()) {
            if (AssertorMap.contains(map, entry.getKey(), entry.getValue())) {
                found++;
            }
        }

        return HelperAssertor.isValid(all, not, found, objects.size());
    }

    private static <K, V> boolean contains(final Map<K, V> map, final K key, final V value) {
        if (map.containsKey(key)) {
            V val = map.get(key);
            if (val != null && val.equals(value)) {
                return true;
            } else if (val == null && value == null) {
                return true;
            }
        }
        return false;
    }
}
