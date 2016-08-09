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
import java.util.function.BiFunction;
import java.util.function.Function;

import org.apache.commons.collections4.IterableUtils;
import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.TriFunction;

public class AssertorIterable extends Constants {

    protected static <T, E extends Throwable> AssertorResult<Iterable<T>> hasSize(final AssertorResult<Iterable<T>> result, final int size,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<Iterable<T>, Boolean> precondition = (iterable) -> size >= 0 && iterable != null;

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage
                .getDefaultMessage(result, MSG.ITERABLE.SIZE, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Iterable<T>, Boolean, Boolean, E> checker = (iterable, not) -> IterableUtils.size(iterable) == size;

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.ITERABLE.SIZE, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(size, EnumType.NUMBER_INTEGER));
    }

    protected static <T, E extends Throwable> AssertorResult<Iterable<T>> isEmpty(final AssertorResult<Iterable<T>> result,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<Iterable<T>, Boolean, Boolean, E> checker = (map, not) -> IterableUtils.isEmpty(map);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.ITERABLE.EMPTY, not, objectIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false);
    }

    protected static <T, E extends Throwable> AssertorResult<Iterable<T>> isNotEmpty(final AssertorResult<Iterable<T>> result,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<Iterable<T>, Boolean, Boolean, E> checker = (iterable, not) -> !IterableUtils.isEmpty(iterable);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.ITERABLE.EMPTY, !not, objectIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false);
    }

    protected static <T, E extends Throwable> AssertorResult<Iterable<T>> containsAll(final AssertorResult<Iterable<T>> result,
            final Iterable<T> values, final Locale locale, final CharSequence message, final Object[] arguments) {

        return contains(result, values, MSG.ITERABLE.CONTAINS_ALL, true, locale, message, arguments);
    }

    protected static <T, E extends Throwable> AssertorResult<Iterable<T>> containsAny(final AssertorResult<Iterable<T>> result,
            final Iterable<T> values, final Locale locale, final CharSequence message, final Object[] arguments) {

        return contains(result, values, MSG.ITERABLE.CONTAINS_ANY, false, locale, message, arguments);
    }

    protected static <T, E extends Throwable> AssertorResult<Iterable<T>> contains(final AssertorResult<Iterable<T>> result,
            final Iterable<T> iterable, final CharSequence key, final boolean all, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final Function<Iterable<T>, Boolean> precondition = (iterable1) -> !IterableUtils.isEmpty(iterable1)
                && !IterableUtils.isEmpty(iterable);

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage
                .getDefaultMessage(result, key, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Iterable<T>, Boolean, Boolean, E> checker = (iterable1, not) -> AssertorIterable.has(iterable1, iterable,
                all, not);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, key, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, true,
                Pair.of(iterable, EnumType.ITERABLE));
    }

    protected static <T, E extends Throwable> AssertorResult<Iterable<T>> contains(final AssertorResult<Iterable<T>> result, final T value,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<Iterable<T>, Boolean> precondition = (iterable) -> !IterableUtils.isEmpty(iterable);

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage
                .getDefaultMessage(result, MSG.ITERABLE.CONTAINS_OBJECT, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Iterable<T>, Boolean, Boolean, E> checker = (iterable, not) -> AssertorIterable.has(iterable, value);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.ITERABLE.CONTAINS_OBJECT, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
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

        return HelperAssertor.isValid(all, not, found, IterableUtils.size(iterable2));
    }
}
