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

import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.function.Function;
import java.util.function.Supplier;

import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.commons.Comparators;
import fr.landel.utils.commons.DateUtils;
import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.QuadFunction;
import fr.landel.utils.commons.function.TriFunction;

/**
 * (Description)
 *
 * @since 6 août 2016
 * @author Gilles
 *
 */
public class AssertorDate extends AssertorConstants {

    protected static <T extends Comparable<T>, E extends Throwable> Supplier<AssertorResult<T>> isEqual(
            final Supplier<AssertorResult<T>> step, final T date, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        return AssertorDate.isAround(step, date, -1, 0, locale, message, arguments);
    }

    protected static <T extends Comparable<T>, E extends Throwable> Supplier<AssertorResult<T>> isNotEqual(
            final Supplier<AssertorResult<T>> step, final T date, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        return AssertorDate.isNotAround(step, date, -1, 0, locale, message, arguments);
    }

    protected static <T extends Comparable<T>, E extends Throwable> Supplier<AssertorResult<T>> isAround(
            final Supplier<AssertorResult<T>> step, final T date, final int calendarField, final int calendarAmount, final Locale locale,
            final CharSequence message, final Object[] arguments) {

        return AssertorDate.isAround(step, date, calendarField, calendarAmount, false, locale, message, arguments);
    }

    protected static <T extends Comparable<T>, E extends Throwable> Supplier<AssertorResult<T>> isNotAround(
            final Supplier<AssertorResult<T>> step, final T date, final int calendarField, final int calendarAmount, final Locale locale,
            final CharSequence message, final Object[] arguments) {

        return AssertorDate.isAround(step, date, calendarField, calendarAmount, true, locale, message, arguments);
    }

    protected static <T extends Comparable<T>, E extends Throwable> Supplier<AssertorResult<T>> isAround(
            final Supplier<AssertorResult<T>> step, final T date, final int calendarField, final int calendarAmount, final boolean reverse,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (date1) -> {
            final boolean prerequisites;
            final boolean calendarFieldOk = calendarField == -1 || (CALENDAR_FIELDS.containsKey(calendarField) && calendarAmount != 0);

            if (calendarField != -1) {
                prerequisites = date1 != null && date != null;
            } else {
                prerequisites = true;
            }

            return prerequisites && calendarFieldOk;
        };

        final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex, paramIndex) -> {
            return AssertorHelper.msg(result, MSG.DATE.AROUND, true, false, objectIndex, paramIndex);
        };

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (date1, not) -> {
            boolean result = false;
            final int compare = Comparators.compare(date1, date);
            if (compare == 0) {
                result = true;
            } else if (calendarField != -1) {
                result = AssertorDate.isAround(date1, date, calendarField, calendarAmount, compare);
            }
            return reverse ^ result;
        };

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> {
            if (calendarField == -1) {
                return AssertorHelper.getMessage(result, locale, message, arguments, MSG.DATE.EQUALS, not, objectIndex, paramIndex);
            } else {
                return AssertorHelper.getMessage(result, locale, message, arguments, MSG.DATE.AROUND, not, objectIndex, paramIndex,
                        paramIndex + 1, paramIndex + 2);
            }
        };

        if (calendarField == -1) {
            return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                    Pair.of(date, EnumType.DATE));
        } else {
            return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                    Pair.of(date, EnumType.DATE), Pair.of(calendarField, EnumType.CALENDAR_FIELD),
                    Pair.of(calendarAmount, EnumType.NUMBER_INTEGER));
        }
    }

    private static <T> boolean isAround(final T date1, final T date, final int calendarField, final int calendarAmount, final int compare) {

        Calendar calendar1;
        Calendar calendar2;
        final Class<?> clazz = date1.getClass();

        if (Date.class.isAssignableFrom(clazz)) {
            calendar1 = DateUtils.getCalendar((Date) date1);
            calendar2 = DateUtils.getCalendar((Date) date);
        } else if (Calendar.class.isAssignableFrom(clazz)) {
            calendar1 = (Calendar) ((Calendar) date1).clone();
            calendar2 = (Calendar) ((Calendar) date).clone();
        } else { // Future support of LocalTime...
            return false;
        }

        if (compare < 0) {
            calendar1.add(calendarField, calendarAmount);
            return !calendar1.before(calendar2);
        } else if (compare > 0) {
            calendar2.add(calendarField, calendarAmount);
            return !calendar2.before(calendar1);
        }
        return true;
    }

    protected static <T extends Comparable<T>, E extends Throwable> Supplier<AssertorResult<T>> isAfter(
            final Supplier<AssertorResult<T>> step, final T date, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (date1, not) -> Comparators.compare(date1, date) > 0;

        return AssertorDate.is(step, date, MSG.DATE.AFTER, checker, locale, message, arguments);
    }

    protected static <T extends Comparable<T>, E extends Throwable> Supplier<AssertorResult<T>> isAfterOrEquals(
            final Supplier<AssertorResult<T>> step, final T date, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (date1, not) -> Comparators.compare(date1, date) >= 0;

        return AssertorDate.is(step, date, MSG.DATE.AFTER_OR_EQUALS, checker, locale, message, arguments);
    }

    private static <T extends Comparable<T>, E extends Throwable> Supplier<AssertorResult<T>> is(final Supplier<AssertorResult<T>> step,
            final T date, final CharSequence key, final BiFunctionThrowable<T, Boolean, Boolean, E> checker, final Locale locale,
            final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (date1) -> date1 != null && date != null;

        final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, key, true, false, objectIndex, paramIndex);

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, key, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(date, EnumType.DATE));
    }

    protected static <T extends Comparable<T>, E extends Throwable> Supplier<AssertorResult<T>> isBefore(
            final Supplier<AssertorResult<T>> step, final T date, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (date1, not) -> Comparators.compare(date1, date) < 0;

        return AssertorDate.is(step, date, MSG.DATE.BEFORE, checker, locale, message, arguments);
    }

    protected static <T extends Comparable<T>, E extends Throwable> Supplier<AssertorResult<T>> isBeforeOrEquals(
            final Supplier<AssertorResult<T>> step, final T date, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (date1, not) -> Comparators.compare(date1, date) <= 0;

        return AssertorDate.is(step, date, MSG.DATE.BEFORE_OR_EQUALS, checker, locale, message, arguments);
    }

}
