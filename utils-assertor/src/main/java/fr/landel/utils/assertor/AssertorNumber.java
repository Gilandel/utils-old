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
import java.util.function.Supplier;

import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.commons.Comparators;
import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.QuadFunction;

public class AssertorNumber extends AssertorConstants {

    protected static <N extends Number & Comparable<N>, E extends Throwable> Supplier<AssertorResult<N>> isEqual(
            final Supplier<AssertorResult<N>> step, final N number, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final BiFunctionThrowable<N, Boolean, Boolean, E> checker = (object, not) -> Comparators.compare(object, number) == 0;

        final QuadFunction<AssertorResult<N>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.NUMBER.EQUALS, not,
                        objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false, Pair.of(number, EnumType.getType(number)));
    }

    protected static <N extends Number & Comparable<N>, E extends Throwable> Supplier<AssertorResult<N>> isNotEqual(
            final Supplier<AssertorResult<N>> step, final N number, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final BiFunctionThrowable<N, Boolean, Boolean, E> checker = (object, not) -> Comparators.compare(object, number) != 0;

        final QuadFunction<AssertorResult<N>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.NUMBER.EQUALS, !not,
                        objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false, Pair.of(number, EnumType.getType(number)));
    }

    protected static <N extends Number & Comparable<N>, E extends Throwable> Supplier<AssertorResult<N>> isGT(
            final Supplier<AssertorResult<N>> step, final N number, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final BiFunctionThrowable<N, Boolean, Boolean, E> checker = (object, not) -> Comparators.compare(object, number) > 0;

        final QuadFunction<AssertorResult<N>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.NUMBER.GT, not, objectIndex,
                        paramIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false, Pair.of(number, EnumType.getType(number)));
    }

    protected static <N extends Number & Comparable<N>, E extends Throwable> Supplier<AssertorResult<N>> isGTE(
            final Supplier<AssertorResult<N>> step, final N number, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final BiFunctionThrowable<N, Boolean, Boolean, E> checker = (object, not) -> Comparators.compare(object, number) >= 0;

        final QuadFunction<AssertorResult<N>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.NUMBER.GTE, not, objectIndex,
                        paramIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false, Pair.of(number, EnumType.getType(number)));
    }

    protected static <N extends Number & Comparable<N>, E extends Throwable> Supplier<AssertorResult<N>> isLT(
            final Supplier<AssertorResult<N>> step, final N number, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final BiFunctionThrowable<N, Boolean, Boolean, E> checker = (object, not) -> Comparators.compare(object, number) < 0;

        final QuadFunction<AssertorResult<N>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.NUMBER.LT, not, objectIndex,
                        paramIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false, Pair.of(number, EnumType.getType(number)));
    }

    protected static <N extends Number & Comparable<N>, E extends Throwable> Supplier<AssertorResult<N>> isLTE(
            final Supplier<AssertorResult<N>> step, final N number, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final BiFunctionThrowable<N, Boolean, Boolean, E> checker = (object, not) -> Comparators.compare(object, number) <= 0;

        final QuadFunction<AssertorResult<N>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.NUMBER.LTE, not, objectIndex,
                        paramIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false, Pair.of(number, EnumType.getType(number)));
    }
}
