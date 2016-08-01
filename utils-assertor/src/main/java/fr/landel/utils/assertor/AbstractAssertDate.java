/*
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

import org.apache.commons.lang3.ArrayUtils;

import fr.landel.utils.commons.Comparators;
import fr.landel.utils.commons.DateUtils;

/**
 * Assertion utility class that assists in validating arguments for dates.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 * @param <A>
 *            The Assertor type
 * @param <T>
 *            The date type
 */
public abstract class AbstractAssertDate<A extends AbstractAssertDate<A, T>, T extends Comparable<T>> extends AssertObject<A, T> {

    /**
     * 
     * Constructor
     *
     * @param object
     *            the date or calendar input object
     */
    protected AbstractAssertDate(final T object) {
        super(object, TYPE.DATE);
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

    /**
     * Check if the first date is around the second one.
     * 
     * @param date
     *            the second date
     * @param calendarField
     *            the calendar field
     * @param calendarAmount
     *            the calendar amount
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    protected Operator<A, T> isAround(final T date, final int calendarField, final int calendarAmount, final Locale locale,
            final CharSequence message, final Object... arguments) {

        final boolean calendarFieldOk = calendarField == -1 || (CALENDAR_FIELDS.containsKey(calendarField) && calendarAmount != 0);
        boolean prerequisites = true;

        Object[] parameters = new Object[] {date};
        if (calendarField != -1) {
            if (CALENDAR_FIELDS.containsKey(calendarField)) {
                parameters = ArrayUtils.add(parameters, new Object[] {CALENDAR_FIELDS.get(calendarField), calendarAmount});
            } else {
                parameters = ArrayUtils.add(parameters, new Object[] {calendarField, calendarAmount});
            }
            prerequisites = this.get() != null && date != null;
        }

        return this.combine(prerequisites && calendarFieldOk, () -> {
            boolean result = false;
            final int compare = Comparators.compare(this.get(), date);
            if (compare == 0) {
                result = true;
            } else if (calendarField != -1) {
                result = AbstractAssertDate.isAround(this.get(), date, calendarField, calendarAmount, compare);
            }
            return result;
        }, () -> {
            // no prerequisite for equals (calendarField == -1) only for around
            return this.msg(MSG.DATE.AROUND, true);
        }, message, arguments, locale, parameters);
    }

    /**
     * Check if the first date is NOT around the second one.
     * 
     * @param date
     *            the second date
     * @param calendarField
     *            the calendar field
     * @param calendarAmount
     *            the calendar amount
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    protected Operator<A, T> isNotAround(final T date, final int calendarField, final int calendarAmount, final Locale locale,
            final CharSequence message, final Object... arguments) {

        final boolean calendarFieldOk = calendarField == -1 || (CALENDAR_FIELDS.containsKey(calendarField) && calendarAmount != 0);
        boolean prerequisites = true;

        Object[] parameters = new Object[] {date};
        if (calendarField != -1) {
            if (CALENDAR_FIELDS.containsKey(calendarField)) {
                parameters = ArrayUtils.add(parameters, new Object[] {CALENDAR_FIELDS.get(calendarField), calendarAmount});
            } else {
                parameters = ArrayUtils.add(parameters, new Object[] {calendarField, calendarAmount});
            }
            prerequisites = this.get() != null && date != null;
        }

        return this.combine(prerequisites && calendarFieldOk, () -> {
            boolean result = true;
            final int compare = Comparators.compare(this.get(), date);
            if (compare == 0) {
                result = false;
            } else if (calendarField != -1) {
                result = !AbstractAssertDate.isAround(this.get(), date, calendarField, calendarAmount, compare);
            }
            return result;
        }, () -> {
            // no prerequisite for equals (calendarField == -1) only for around
            return this.msg(MSG.DATE.AROUND + MSG.NOT, true);
        }, message, arguments, locale, parameters);
    }

    /**
     * Check if the first date is equal to the second one.
     * 
     * @param date
     *            the second date
     * @return the operator
     */
    public Operator<A, T> isEqual(final T date) {
        return this.isEqual(date, this.msg(MSG.DATE.EQUALS, this.getParam(), this.getNextParam(1, TYPE.DATE)));
    }

    /**
     * Check if the first date is equal to the second one.
     * 
     * @param date
     *            the second date
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<A, T> isEqual(final T date, final CharSequence message, final Object... arguments) {
        return this.isEqual(date, null, message, arguments);
    }

    /**
     * Check if the first date is equal to the second one.
     * 
     * @param date
     *            the second date
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<A, T> isEqual(final T date, final Locale locale, final CharSequence message, final Object... arguments) {
        return this.isAround(date, -1, -1, locale, message, arguments);
    }

    /**
     * Check if the first date is NOT equal to the second one.
     * 
     * @param date
     *            the second date
     * @return the operator
     */
    public Operator<A, T> isNotEqual(final T date) {
        return this.isNotEqual(date, this.msg(MSG.DATE.EQUALS + MSG.NOT, this.getParam(), this.getNextParam(1, TYPE.DATE)));
    }

    /**
     * Check if the first date is NOT equal to the second one.
     * 
     * @param date
     *            the second date
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<A, T> isNotEqual(final T date, final CharSequence message, final Object... arguments) {
        return this.isNotEqual(date, null, message, arguments);
    }

    /**
     * Check if the first date is NOT equal to the second one.
     * 
     * @param date
     *            the second date
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<A, T> isNotEqual(final T date, final Locale locale, final CharSequence message, final Object... arguments) {
        return this.isNotAround(date, -1, -1, locale, message, arguments);
    }

    /**
     * Check if the first date is after the second one.
     * 
     * @param date
     *            the second date
     * @return the operator
     */
    protected Operator<A, T> isAfter(final T date) {
        return this.isAfter(date, this.msg(MSG.DATE.AFTER, this.getParam(), this.getNextParam(1, TYPE.DATE)));
    }

    /**
     * Check if the first date is after the second one.
     * 
     * @param date
     *            the second date
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    protected Operator<A, T> isAfter(final T date, final CharSequence message, final Object... arguments) {
        return this.isAfter(date, null, message, arguments);
    }

    /**
     * Check if the first date is after the second one.
     * 
     * @param date
     *            the second date
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    protected Operator<A, T> isAfter(final T date, final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(this.get() != null && date != null, () -> Comparators.compare(this.get(), date) > 0,
                () -> this.msg(MSG.DATE.AFTER), message, arguments, locale, date);
    }

    /**
     * Check if the first date is after or equal to the second one.
     * 
     * @param date
     *            the second date
     * @return the operator
     */
    protected Operator<A, T> isAfterOrEquals(final T date) {
        return this.isAfterOrEquals(date, this.msg(MSG.DATE.AFTER_OR_EQUALS, this.getParam(), this.getNextParam(1, TYPE.DATE)));
    }

    /**
     * Check if the first date is after or equal to the second one.
     * 
     * @param date
     *            the second date
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    protected Operator<A, T> isAfterOrEquals(final T date, final CharSequence message, final Object... arguments) {
        return this.isAfterOrEquals(date, null, message, arguments);
    }

    /**
     * Check if the first date is after or equal to the second one.
     * 
     * @param date
     *            the second date
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    protected Operator<A, T> isAfterOrEquals(final T date, final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(this.get() != null && date != null, () -> Comparators.compare(this.get(), date) >= 0,
                () -> this.msg(MSG.DATE.AFTER_OR_EQUALS), message, arguments, locale, date);
    }

    /**
     * Check if the first date is before the second one.
     * 
     * @param date
     *            the second date
     * @return the operator
     */
    protected Operator<A, T> isBefore(final T date) {
        return this.isBefore(date, this.msg(MSG.DATE.BEFORE, this.getParam(), this.getNextParam(1, TYPE.DATE)));
    }

    /**
     * Check if the first date is before the second one.
     * 
     * @param date
     *            the second date
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    protected Operator<A, T> isBefore(final T date, final CharSequence message, final Object... arguments) {
        return this.isBefore(date, null, message, arguments);
    }

    /**
     * Check if the first date is before the second one.
     * 
     * @param date
     *            the second date
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    protected Operator<A, T> isBefore(final T date, final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(this.get() != null && date != null, () -> Comparators.compare(this.get(), date) < 0,
                () -> this.msg(MSG.DATE.BEFORE), message, arguments, locale, date);
    }

    /**
     * Check if the first date is before or equal to the second one.
     * 
     * @param date
     *            the second date
     * @return the operator
     */
    protected Operator<A, T> isBeforeOrEquals(final T date) {
        return this.isBeforeOrEquals(date, this.msg(MSG.DATE.BEFORE_OR_EQUALS, this.getParam(), this.getNextParam(1, TYPE.DATE)));
    }

    /**
     * Check if the first date is before or equal to the second one.
     * 
     * @param date
     *            the second date
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    protected Operator<A, T> isBeforeOrEquals(final T date, final CharSequence message, final Object... arguments) {
        return this.isBeforeOrEquals(date, null, message, arguments);
    }

    /**
     * Check if the first date is before or equal to the second one.
     * 
     * @param date
     *            the second date
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    protected Operator<A, T> isBeforeOrEquals(final T date, final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(this.get() != null && date != null, () -> Comparators.compare(this.get(), date) <= 0,
                () -> this.msg(MSG.DATE.BEFORE_OR_EQUALS, true), message, arguments, locale, date);
    }
}