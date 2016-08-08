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
import java.util.function.Function;
import java.util.function.Supplier;

import org.apache.commons.collections4.IterableUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.QuadFunction;
import fr.landel.utils.commons.function.TriFunction;

public class AssertorMap extends AssertorConstants {

    protected static <K, V, E extends Throwable> Supplier<AssertorResult<Map<K, V>>> hasSize(final Supplier<AssertorResult<Map<K, V>>> step,
            final int size, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<Map<K, V>, Boolean> precondition = (map) -> size >= 0 && map != null;

        final TriFunction<AssertorResult<Map<K, V>>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.MAP.SIZE, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Map<K, V>, Boolean, Boolean, E> checker = (map, not) -> map.size() == size;

        final QuadFunction<AssertorResult<Map<K, V>>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex,
                paramIndex, not) -> AssertorHelper.getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.MAP.SIZE, not,
                        objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(size, EnumType.NUMBER_INTEGER));
    }

    protected static <K, V, E extends Throwable> Supplier<AssertorResult<Map<K, V>>> isEmpty(final Supplier<AssertorResult<Map<K, V>>> step,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<Map<K, V>, Boolean, Boolean, E> checker = (map, not) -> MapUtils.isEmpty(map);

        final QuadFunction<AssertorResult<Map<K, V>>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex,
                paramIndex, not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.MAP.EMPTY, not, objectIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false);
    }

    protected static <K, V, E extends Throwable> Supplier<AssertorResult<Map<K, V>>> isNotEmpty(
            final Supplier<AssertorResult<Map<K, V>>> step, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<Map<K, V>, Boolean, Boolean, E> checker = (map, not) -> MapUtils.isNotEmpty(map);

        final QuadFunction<AssertorResult<Map<K, V>>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex,
                paramIndex, not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.MAP.EMPTY, !not, objectIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false);
    }

    protected static <K, V, E extends Throwable> Supplier<AssertorResult<Map<K, V>>> containsAll(
            final Supplier<AssertorResult<Map<K, V>>> step, final Map<K, V> map, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        return contains(step, map, MSG.MAP.CONTAINS_MAP_ALL, true, locale, message, arguments);
    }

    protected static <K, V, E extends Throwable> Supplier<AssertorResult<Map<K, V>>> containsAll(
            final Supplier<AssertorResult<Map<K, V>>> step, final Iterable<K> keys, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        return contains(step, keys, MSG.MAP.CONTAINS_KEYS_ALL, true, locale, message, arguments);
    }

    protected static <K, V, E extends Throwable> Supplier<AssertorResult<Map<K, V>>> containsAny(
            final Supplier<AssertorResult<Map<K, V>>> step, final Map<K, V> map, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        return contains(step, map, MSG.MAP.CONTAINS_MAP_ANY, false, locale, message, arguments);
    }

    protected static <K, V, E extends Throwable> Supplier<AssertorResult<Map<K, V>>> containsAny(
            final Supplier<AssertorResult<Map<K, V>>> step, final Iterable<K> keys, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        return contains(step, keys, MSG.MAP.CONTAINS_KEYS_ANY, false, locale, message, arguments);
    }

    protected static <K, V, E extends Throwable> Supplier<AssertorResult<Map<K, V>>> contains(
            final Supplier<AssertorResult<Map<K, V>>> step, final Iterable<K> keys, final CharSequence key, final boolean all,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<Map<K, V>, Boolean> precondition = (map) -> MapUtils.isNotEmpty(map) && !IterableUtils.isEmpty(keys);

        final TriFunction<AssertorResult<Map<K, V>>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, key, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Map<K, V>, Boolean, Boolean, E> checker = (map, not) -> AssertorMap.contains(map, keys, all, not);

        final QuadFunction<AssertorResult<Map<K, V>>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex,
                paramIndex, not) -> AssertorHelper.getMessage(result, locale, message, arguments, key, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, true,
                Pair.of(keys, EnumType.ITERABLE));
    }

    protected static <K, V, E extends Throwable> Supplier<AssertorResult<Map<K, V>>> contains(
            final Supplier<AssertorResult<Map<K, V>>> step, final Map<K, V> map, final CharSequence key, final boolean all,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<Map<K, V>, Boolean> precondition = (map1) -> MapUtils.isNotEmpty(map1) && MapUtils.isNotEmpty(map);

        final TriFunction<AssertorResult<Map<K, V>>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, key, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Map<K, V>, Boolean, Boolean, E> checker = (map1, not) -> AssertorMap.contains(map1, map, all, not);

        final QuadFunction<AssertorResult<Map<K, V>>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex,
                paramIndex, not) -> AssertorHelper.getMessage(result, locale, message, arguments, key, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, true, Pair.of(map, EnumType.MAP));
    }

    protected static <K, V, E extends Throwable> Supplier<AssertorResult<Map<K, V>>> contains(
            final Supplier<AssertorResult<Map<K, V>>> step, final K key, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final Function<Map<K, V>, Boolean> precondition = (map) -> MapUtils.isNotEmpty(map);

        final TriFunction<AssertorResult<Map<K, V>>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.MAP.CONTAINS_KEY, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Map<K, V>, Boolean, Boolean, E> checker = (map, not) -> map.containsKey(key);

        final QuadFunction<AssertorResult<Map<K, V>>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex,
                paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.MAP.CONTAINS_KEY, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(key, EnumType.getType(key)));
    }

    protected static <K, V, E extends Throwable> Supplier<AssertorResult<Map<K, V>>> contains(
            final Supplier<AssertorResult<Map<K, V>>> step, final K key, final V value, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final Function<Map<K, V>, Boolean> precondition = (map) -> MapUtils.isNotEmpty(map);

        final TriFunction<AssertorResult<Map<K, V>>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.MAP.CONTAINS_PAIR, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Map<K, V>, Boolean, Boolean, E> checker = (map, not) -> AssertorMap.contains(map, key, value);

        final QuadFunction<AssertorResult<Map<K, V>>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex,
                paramIndex, not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.MAP.CONTAINS_PAIR, not, objectIndex,
                        paramIndex, paramIndex + 1);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(key, EnumType.getType(key)), Pair.of(value, EnumType.getType(value)));
    }

    private static <K, V> boolean contains(final Map<K, V> map, final Iterable<K> keys, final boolean all, final boolean not) {
        int found = 0;
        for (K key : keys) {
            if (map.containsKey(key)) {
                found++;
            }
        }
        if (all ^ not) { // ALL or NOT ANY
            return found == IterableUtils.size(keys);
        } else if (all && not) { // NOT ALL
            return found > 0 && found < IterableUtils.size(keys);
        } else { // ANY
            return found > 0;
        }
    }

    private static <K, V> boolean contains(final Map<K, V> map, final Map<K, V> objects, final boolean all, final boolean not) {
        int found = 0;
        for (Entry<K, V> entry : objects.entrySet()) {
            if (AssertorMap.contains(map, entry.getKey(), entry.getValue())) {
                found++;
            }
        }

        if (not && all) { // NOT ALL
            return found > 0 && found < objects.size();
        } else if (!not && all) { // ALL
            return found == objects.size();
        } else if (not && !all) { // NOT ANY
            return found == 0;
        } else { // ANY
            return found > 0;
        }
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
