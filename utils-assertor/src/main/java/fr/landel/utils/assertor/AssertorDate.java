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
import java.util.function.BiFunction;
import java.util.function.Function;

import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.commons.Comparators;
import fr.landel.utils.commons.DateUtils;
import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.TriFunction;

/**
 * (Description)
 *
 * @since 6 août 2016
 * @author Gilles
 *
 */
public class AssertorDate extends Constants {

    protected static <T extends Comparable<T>, E extends Throwable> AssertorResult<T> isEqual(final AssertorResult<T> result, final T date,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        return AssertorDate.isAround(result, date, -1, 0, locale, message, arguments);
    }

    protected static <T extends Comparable<T>, E extends Throwable> AssertorResult<T> isNotEqual(final AssertorResult<T> result,
            final T date, final Locale locale, final CharSequence message, final Object[] arguments) {

        return AssertorDate.isNotAround(result, date, -1, 0, locale, message, arguments);
    }

    protected static <T extends Comparable<T>, E extends Throwable> AssertorResult<T> isAround(final AssertorResult<T> result, final T date,
            final int calendarField, final int calendarAmount, final Locale locale, final CharSequence message, final Object[] arguments) {

        return AssertorDate.isAround(result, date, calendarField, calendarAmount, false, locale, message, arguments);
    }

    protected static <T extends Comparable<T>, E extends Throwable> AssertorResult<T> isNotAround(final AssertorResult<T> result,
            final T date, final int calendarField, final int calendarAmount, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        return AssertorDate.isAround(result, date, calendarField, calendarAmount, true, locale, message, arguments);
    }

    protected static <T extends Comparable<T>, E extends Throwable> AssertorResult<T> isAround(final AssertorResult<T> result, final T date,
            final int calendarField, final int calendarAmount, final boolean reverse, final Locale locale, final CharSequence message,
            final Object[] arguments) {

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

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> {
            return HelperMessage.getDefaultMessage(result, MSG.DATE.AROUND, true, false, objectIndex, paramIndex);
        };

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (date1, not) -> {
            boolean around = false;
            final int compare = Comparators.compare(date1, date);
            if (compare == 0) {
                around = true;
            } else if (calendarField != -1) {
                around = AssertorDate.isAround(date1, date, calendarField, calendarAmount, compare);
            }
            return reverse ^ around;
        };

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> {
            if (calendarField == -1) {
                return HelperMessage.getMessage(result, locale, message, arguments, MSG.DATE.EQUALS, not, objectIndex, paramIndex);
            } else {
                return HelperMessage.getMessage(result, locale, message, arguments, MSG.DATE.AROUND, not, objectIndex, paramIndex,
                        paramIndex + 1, paramIndex + 2);
            }
        };

        if (calendarField == -1) {
            return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                    Pair.of(date, EnumType.DATE));
        } else {
            return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
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
        return true; // normally not used (equals)
    }

    protected static <T extends Comparable<T>, E extends Throwable> AssertorResult<T> isAfter(final AssertorResult<T> result, final T date,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (date1, not) -> Comparators.compare(date1, date) > 0;

        return AssertorDate.is(result, date, MSG.DATE.AFTER, checker, locale, message, arguments);
    }

    protected static <T extends Comparable<T>, E extends Throwable> AssertorResult<T> isAfterOrEquals(final AssertorResult<T> result,
            final T date, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (date1, not) -> Comparators.compare(date1, date) >= 0;

        return AssertorDate.is(result, date, MSG.DATE.AFTER_OR_EQUALS, checker, locale, message, arguments);
    }

    private static <T extends Comparable<T>, E extends Throwable> AssertorResult<T> is(final AssertorResult<T> result, final T date,
            final CharSequence key, final BiFunctionThrowable<T, Boolean, Boolean, E> checker, final Locale locale,
            final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (date1) -> date1 != null && date != null;

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage
                .getDefaultMessage(result, key, true, false, objectIndex, paramIndex);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, key, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(date, EnumType.DATE));
    }

    protected static <T extends Comparable<T>, E extends Throwable> AssertorResult<T> isBefore(final AssertorResult<T> result, final T date,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (date1, not) -> Comparators.compare(date1, date) < 0;

        return AssertorDate.is(result, date, MSG.DATE.BEFORE, checker, locale, message, arguments);
    }

    protected static <T extends Comparable<T>, E extends Throwable> AssertorResult<T> isBeforeOrEquals(final AssertorResult<T> result,
            final T date, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (date1, not) -> Comparators.compare(date1, date) <= 0;

        return AssertorDate.is(result, date, MSG.DATE.BEFORE_OR_EQUALS, checker, locale, message, arguments);
    }

}
