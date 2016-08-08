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
import java.util.function.Function;
import java.util.function.Supplier;

import org.apache.commons.collections4.IterableUtils;
import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.QuadFunction;
import fr.landel.utils.commons.function.TriFunction;

public class AssertorIterable extends AssertorConstants {

    protected static <T, E extends Throwable> Supplier<AssertorResult<Iterable<T>>> hasSize(
            final Supplier<AssertorResult<Iterable<T>>> step, final int size, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final Function<Iterable<T>, Boolean> precondition = (iterable) -> size >= 0 && iterable != null;

        final TriFunction<AssertorResult<Iterable<T>>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.ITERABLE.SIZE, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Iterable<T>, Boolean, Boolean, E> checker = (iterable, not) -> IterableUtils.size(iterable) == size;

        final QuadFunction<AssertorResult<Iterable<T>>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex,
                paramIndex, not) -> AssertorHelper.getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.ITERABLE.SIZE,
                        not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(size, EnumType.NUMBER_INTEGER));
    }

    protected static <T, E extends Throwable> Supplier<AssertorResult<Iterable<T>>> isEmpty(
            final Supplier<AssertorResult<Iterable<T>>> step, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<Iterable<T>, Boolean, Boolean, E> checker = (map, not) -> IterableUtils.isEmpty(map);

        final QuadFunction<AssertorResult<Iterable<T>>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex,
                paramIndex, not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.ITERABLE.EMPTY, not, objectIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false);
    }

    protected static <T, E extends Throwable> Supplier<AssertorResult<Iterable<T>>> isNotEmpty(
            final Supplier<AssertorResult<Iterable<T>>> step, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<Iterable<T>, Boolean, Boolean, E> checker = (iterable, not) -> !IterableUtils.isEmpty(iterable);

        final QuadFunction<AssertorResult<Iterable<T>>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex,
                paramIndex, not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.ITERABLE.EMPTY, !not, objectIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false);
    }

    protected static <T, E extends Throwable> Supplier<AssertorResult<Iterable<T>>> containsAll(
            final Supplier<AssertorResult<Iterable<T>>> step, final Iterable<T> values, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        return contains(step, values, MSG.ITERABLE.CONTAINS_ALL, true, locale, message, arguments);
    }

    protected static <T, E extends Throwable> Supplier<AssertorResult<Iterable<T>>> containsAny(
            final Supplier<AssertorResult<Iterable<T>>> step, final Iterable<T> values, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        return contains(step, values, MSG.ITERABLE.CONTAINS_ANY, false, locale, message, arguments);
    }

    protected static <T, E extends Throwable> Supplier<AssertorResult<Iterable<T>>> contains(
            final Supplier<AssertorResult<Iterable<T>>> step, final Iterable<T> iterable, final CharSequence key, final boolean all,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<Iterable<T>, Boolean> precondition = (iterable1) -> !IterableUtils.isEmpty(iterable1)
                && !IterableUtils.isEmpty(iterable);

        final TriFunction<AssertorResult<Iterable<T>>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, key, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Iterable<T>, Boolean, Boolean, E> checker = (iterable1, not) -> AssertorIterable.has(iterable1, iterable,
                all, not);

        final QuadFunction<AssertorResult<Iterable<T>>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex,
                paramIndex, not) -> AssertorHelper.getMessage(result, locale, message, arguments, key, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, true,
                Pair.of(iterable, EnumType.ITERABLE));
    }

    protected static <T, E extends Throwable> Supplier<AssertorResult<Iterable<T>>> contains(
            final Supplier<AssertorResult<Iterable<T>>> step, final T value, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final Function<Iterable<T>, Boolean> precondition = (iterable) -> !IterableUtils.isEmpty(iterable);

        final TriFunction<AssertorResult<Iterable<T>>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.ITERABLE.CONTAINS_OBJECT, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Iterable<T>, Boolean, Boolean, E> checker = (iterable, not) -> AssertorIterable.has(iterable, value);

        final QuadFunction<AssertorResult<Iterable<T>>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex,
                paramIndex, not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.ITERABLE.CONTAINS_OBJECT, not,
                        objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(value, EnumType.getType(value)));
    }

    private static <T> boolean has(final Iterable<T> iterable, final T object) {
        boolean found = false;
        if (object != null) {
            for (T objectRef : iterable) {
                if (object.equals(objectRef)) {
                    found = true;
                    break;
                }
            }
        } else {
            for (T objectRef : iterable) {
                if (objectRef == null) {
                    found = true;
                    break;
                }
            }
        }
        return found;
    }

    private static <T> boolean has(final Iterable<T> iterable1, final Iterable<T> iterable2, final boolean all, final boolean not) {
        int found = 0;
        for (T objectRef : iterable2) {
            if (AssertorIterable.has(iterable1, objectRef)) {
                found++;
            }
        }

        if (not && all) { // NOT ALL
            return found > 0 && found < IterableUtils.size(iterable2);
        } else if (!not && all) { // ALL
            return found == IterableUtils.size(iterable2);
        } else if (not && !all) { // NOT ANY
            return found == 0;
        } else { // ANY
            return found > 0;
        }
    }
}
