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

import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.commons.Comparators;
import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.TriFunction;

public class AssertorNumber extends Constants {

    protected static <N extends Number & Comparable<N>, E extends Throwable> AssertorResult<N> isEqual(final AssertorResult<N> result,
            final N number, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<N, Boolean, Boolean, E> checker = (object, not) -> Comparators.compare(object, number) == 0;

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.NUMBER.EQUALS, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false, Pair.of(number, EnumType.getType(number)));
    }

    protected static <N extends Number & Comparable<N>, E extends Throwable> AssertorResult<N> isNotEqual(final AssertorResult<N> result,
            final N number, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<N, Boolean, Boolean, E> checker = (object, not) -> Comparators.compare(object, number) != 0;

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.NUMBER.EQUALS, !not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false, Pair.of(number, EnumType.getType(number)));
    }

    protected static <N extends Number & Comparable<N>, E extends Throwable> AssertorResult<N> isGT(final AssertorResult<N> result,
            final N number, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<N, Boolean, Boolean, E> checker = (object, not) -> Comparators.compare(object, number) > 0;

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.NUMBER.GT, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false, Pair.of(number, EnumType.getType(number)));
    }

    protected static <N extends Number & Comparable<N>, E extends Throwable> AssertorResult<N> isGTE(final AssertorResult<N> result,
            final N number, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<N, Boolean, Boolean, E> checker = (object, not) -> Comparators.compare(object, number) >= 0;

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.NUMBER.GTE, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false, Pair.of(number, EnumType.getType(number)));
    }

    protected static <N extends Number & Comparable<N>, E extends Throwable> AssertorResult<N> isLT(final AssertorResult<N> result,
            final N number, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<N, Boolean, Boolean, E> checker = (object, not) -> Comparators.compare(object, number) < 0;

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.NUMBER.LT, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false, Pair.of(number, EnumType.getType(number)));
    }

    protected static <N extends Number & Comparable<N>, E extends Throwable> AssertorResult<N> isLTE(final AssertorResult<N> result,
            final N number, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<N, Boolean, Boolean, E> checker = (object, not) -> Comparators.compare(object, number) <= 0;

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.NUMBER.LTE, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false, Pair.of(number, EnumType.getType(number)));
    }
}
