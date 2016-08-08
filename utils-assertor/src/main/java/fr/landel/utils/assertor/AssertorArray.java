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

import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.commons.ArrayUtils;
import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.QuadFunction;
import fr.landel.utils.commons.function.TriFunction;

/**
 * (Description)
 *
 * @since 5 août 2016
 * @author Gilles
 *
 */
public class AssertorArray extends AssertorConstants {

    protected static <T, E extends Throwable> Supplier<AssertorResult<T[]>> hasLength(final Supplier<AssertorResult<T[]>> step,
            final int length, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T[], Boolean> precondition = (object) -> length >= 0 && object != null;

        final TriFunction<AssertorResult<T[]>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.ARRAY.LENGTH, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T[], Boolean, Boolean, E> checker = (object, not) -> object.length == length;

        final QuadFunction<AssertorResult<T[]>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.ARRAY.LENGTH, not,
                        objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(length, EnumType.NUMBER_INTEGER));
    }

    protected static <T, E extends Throwable> Supplier<AssertorResult<T[]>> isEmpty(final Supplier<AssertorResult<T[]>> step,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T[], Boolean, Boolean, E> checker = (object, not) -> ArrayUtils.isEmpty(object);

        final QuadFunction<AssertorResult<T[]>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.ARRAY.EMPTY, not, objectIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false);
    }

    protected static <T, E extends Throwable> Supplier<AssertorResult<T[]>> isNotEmpty(final Supplier<AssertorResult<T[]>> step,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T[], Boolean, Boolean, E> checker = (object, not) -> ArrayUtils.isNotEmpty(object);

        final QuadFunction<AssertorResult<T[]>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.ARRAY.EMPTY, !not, objectIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false);
    }

    protected static <T, E extends Throwable> Supplier<AssertorResult<T[]>> contains(final Supplier<AssertorResult<T[]>> step,
            final T object, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T[], Boolean> precondition = (object1) -> object1 != null;

        final TriFunction<AssertorResult<T[]>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.ARRAY.CONTAINS_OBJECT, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T[], Boolean, Boolean, E> checker = (object1, not) -> AssertorArray.has(object1, object);

        final QuadFunction<AssertorResult<T[]>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.ARRAY.CONTAINS_OBJECT, not, objectIndex,
                        paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(object, EnumType.getType(object)));
    }

    private static <T, E extends Throwable> Supplier<AssertorResult<T[]>> contains(final Supplier<AssertorResult<T[]>> step,
            final T[] array, final CharSequence key, final boolean all, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final Function<T[], Boolean> precondition = (object) -> array != null && object != null;

        final TriFunction<AssertorResult<T[]>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, key, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T[], Boolean, Boolean, E> checker = (object, not) -> AssertorArray.has(object, array, all, not);

        final QuadFunction<AssertorResult<T[]>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, key, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, true,
                Pair.of(array, EnumType.ARRAY));
    }

    protected static <T, E extends Throwable> Supplier<AssertorResult<T[]>> containsAll(final Supplier<AssertorResult<T[]>> step,
            final T[] array, final Locale locale, final CharSequence message, final Object[] arguments) {

        return AssertorArray.contains(step, array, MSG.ARRAY.CONTAINS_ALL, true, locale, message, arguments);
    }

    protected static <T, E extends Throwable> Supplier<AssertorResult<T[]>> containsAny(final Supplier<AssertorResult<T[]>> step,
            final T[] array, final Locale locale, final CharSequence message, final Object[] arguments) {

        return AssertorArray.contains(step, array, MSG.ARRAY.CONTAINS_ANY, false, locale, message, arguments);
    }

    private static <T> boolean has(final T[] array, final T object) {
        boolean found = false;
        if (object != null) {
            for (T objectArray : array) {
                if (object.equals(objectArray)) {
                    found = true;
                    break;
                }
            }
        } else {
            for (T objectArray : array) {
                if (objectArray == null) {
                    found = true;
                    break;
                }
            }
        }
        return found;
    }

    private static <T> boolean has(final T[] array1, final T[] array2, final boolean all, final boolean not) {
        int found = 0;
        for (T objectArray : array2) {
            if (AssertorArray.has(array1, objectArray)) {
                found++;
            }
        }

        if (not && all) { // NOT ALL
            return found > 0 && found < array2.length;
        } else if (!not && all) { // ALL
            return found == array2.length;
        } else if (not && !all) { // NOT ANY
            return found == 0;
        } else { // ANY
            return found > 0;
        }
    }
}
