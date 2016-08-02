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

import java.util.Date;
import java.util.Locale;

/**
 * Assertion utility class that assists in validating arguments for dates.
 * 
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public class AssertDate extends AbstractAssertDate<AssertDate, Date> {

    /**
     * 
     * Constructor
     *
     * @param object
     *            The date to check
     */
    protected AssertDate(final Date object) {
        super(object);
    }

    /**
     * Assert that the first date is equal to the second one.
     * 
     * <pre>
     * Assertor.that(date1).isEqual(date2).toThrow();
     * Assertor.that(date1).isEqual(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @return the operator
     */
    public Operator<AssertDate, Date> isEqual(final Date date) {
        return super.isEqual(date);
    }

    /**
     * Assert that the first date is equal to the second one.
     * 
     * <pre>
     * Assertor.that(date1).isEqual(date2).toThrow();
     * Assertor.that(date1).isEqual(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertDate, Date> isEqual(final Date date, final CharSequence message, final Object... arguments) {
        return super.isEqual(date, message, arguments);
    }

    /**
     * Assert that the first date is equal to the second one.
     * 
     * <pre>
     * Assertor.that(date1).isEqual(date2).toThrow();
     * Assertor.that(date1).isEqual(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertDate, Date> isEqual(final Date date, final Locale locale, final CharSequence message, final Object... arguments) {
        return super.isEqual(date, locale, message, arguments);
    }

    /**
     * Assert that the first date is not equal to the second one.
     * 
     * <pre>
     * Assertor.that(date1).isNotEqual(date2).toThrow();
     * Assertor.that(date1).isNotEqual(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @return the operator
     */
    public Operator<AssertDate, Date> isNotEqual(final Date date) {
        return super.isNotEqual(date);
    }

    /**
     * Assert that the first date is not equal to the second one.
     * 
     * <pre>
     * Assertor.that(date1).isNotEqual(date2).toThrow();
     * Assertor.that(date1).isNotEqual(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertDate, Date> isNotEqual(final Date date, final CharSequence message, final Object... arguments) {
        return super.isNotEqual(date, message, arguments);
    }

    /**
     * Assert that the first date is not equal to the second one.
     * 
     * <pre>
     * Assertor.that(date1).isNotEqual(date2).toThrow();
     * Assertor.that(date1).isNotEqual(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertDate, Date> isNotEqual(final Date date, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return super.isNotEqual(date, locale, message, arguments);
    }

    /**
     * Assert that the first date is around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * Assertor.that(date1).isAround(date2, Calendar.MINUTE, 5).toThrow();
     * Assertor.that(date1).isAround(date2, Calendar.MINUTE, 5).isOK();
     * // Check that the first date is 5 minutes around the second one.
     * </pre>
     * 
     * @see #CALENDAR_FIELDS
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @return the operator
     */
    public Operator<AssertDate, Date> isAround(final Date date, final int calendarField, final int calendarAmount) {
        // second param is defined as char sequence, because it will be
        // converted as string from supported calendar fields
        return this.isAround(date, calendarField, calendarAmount, this.msg(MSG.DATE.AROUND, this.getParam(),
                this.getNextParam(1, TYPE.DATE), this.getNextParam(2, TYPE.CHAR_SEQUENCE), this.getNextParam(3, TYPE.NUMBER_INTEGER)));
    }

    /**
     * Assert that the first date is around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * Assertor.that(date1).isAround(date2, Calendar.MINUTE, 5).toThrow();
     * Assertor.that(date1).isAround(date2, Calendar.MINUTE, 5).isOK();
     * // Check that the first date is 5 minutes around the second one.
     * </pre>
     * 
     * @see #CALENDAR_FIELDS
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertDate, Date> isAround(final Date date, final int calendarField, final int calendarAmount,
            final CharSequence message, final Object... arguments) {
        return this.isAround(date, calendarField, calendarAmount, null, message, arguments);
    }

    /**
     * Assert that the first date is around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * Assertor.that(date1).isAround(date2, Calendar.MINUTE, 5).toThrow();
     * Assertor.that(date1).isAround(date2, Calendar.MINUTE, 5).isOK();
     * // Check that the first date is 5 minutes around the second one.
     * </pre>
     * 
     * @see #CALENDAR_FIELDS
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertDate, Date> isAround(final Date date, final int calendarField, final int calendarAmount, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return super.isAround(date, calendarField, calendarAmount, locale, message, arguments);
    }

    /**
     * Assert that the first date is not around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * Assertor.that(date1).isNotAround(date2, Calendar.MINUTE, 5).toThrow();
     * Assertor.that(date1).isNotAround(date2, Calendar.MINUTE, 5).isOK();
     * // Check that the first date is 5 minutes not around the second one.
     * </pre>
     * 
     * @see #CALENDAR_FIELDS
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @return the operator
     */
    public Operator<AssertDate, Date> isNotAround(final Date date, final int calendarField, final int calendarAmount) {
        // second param is defined as char sequence, because it will be
        // converted as string from supported calendar fields
        return this.isNotAround(date, calendarField, calendarAmount, this.msg(MSG.DATE.AROUND + MSG.NOT, this.getParam(),
                this.getNextParam(1, TYPE.DATE), this.getNextParam(2, TYPE.CHAR_SEQUENCE), this.getNextParam(3, TYPE.NUMBER_INTEGER)));
    }

    /**
     * Assert that the first date is not around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * Assertor.that(date1).isNotAround(date2, Calendar.MINUTE, 5).toThrow();
     * Assertor.that(date1).isNotAround(date2, Calendar.MINUTE, 5).isOK();
     * // Check that the first date is 5 minutes not around the second one.
     * </pre>
     * 
     * @see #CALENDAR_FIELDS
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertDate, Date> isNotAround(final Date date, final int calendarField, final int calendarAmount,
            final CharSequence message, final Object... arguments) {
        return this.isNotAround(date, calendarField, calendarAmount, null, message, arguments);
    }

    /**
     * Assert that the first date is not around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * Assertor.that(date1).isNotAround(date2, Calendar.MINUTE, 5).toThrow();
     * Assertor.that(date1).isNotAround(date2, Calendar.MINUTE, 5).isOK();
     * // Check that the first date is 5 minutes not around the second one.
     * </pre>
     * 
     * @see #CALENDAR_FIELDS
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertDate, Date> isNotAround(final Date date, final int calendarField, final int calendarAmount, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return super.isNotAround(date, calendarField, calendarAmount, locale, message, arguments);
    }

    /**
     * Assert that the first date is after than the second one.
     * 
     * <pre>
     * Assertor.that(date1).isAfter(date2).toThrow();
     * Assertor.that(date1).isAfter(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @return the operator
     */
    public Operator<AssertDate, Date> isAfter(final Date date) {
        return super.isAfter(date);
    }

    /**
     * Assert that the first date is after than the second one.
     * 
     * <pre>
     * Assertor.that(date1).isAfter(date2).toThrow();
     * Assertor.that(date1).isAfter(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertDate, Date> isAfter(final Date date, final CharSequence message, final Object... arguments) {
        return super.isAfter(date, message, arguments);
    }

    /**
     * Assert that the first date is after than the second one.
     * 
     * <pre>
     * Assertor.that(date1).isAfter(date2).toThrow();
     * Assertor.that(date1).isAfter(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertDate, Date> isAfter(final Date date, final Locale locale, final CharSequence message, final Object... arguments) {
        return super.isAfter(date, locale, message, arguments);
    }

    /**
     * Assert that the first date is after than or equal to the second one.
     * 
     * <pre>
     * Assertor.that(date1).isAfterOrEquals(date2).toThrow();
     * Assertor.that(date1).isAfterOrEquals(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @return the operator
     */
    public Operator<AssertDate, Date> isAfterOrEquals(final Date date) {
        return super.isAfterOrEquals(date);
    }

    /**
     * Assert that the first date is after than or equal to the second one.
     * 
     * <pre>
     * Assertor.that(date1).isAfterOrEquals(date2).toThrow();
     * Assertor.that(date1).isAfterOrEquals(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertDate, Date> isAfterOrEquals(final Date date, final CharSequence message, final Object... arguments) {
        return super.isAfterOrEquals(date, message, arguments);
    }

    /**
     * Assert that the first date is after than or equal to the second one.
     * 
     * <pre>
     * Assertor.that(date1).isAfterOrEquals(date2).toThrow();
     * Assertor.that(date1).isAfterOrEquals(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertDate, Date> isAfterOrEquals(final Date date, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return super.isAfterOrEquals(date, locale, message, arguments);
    }

    /**
     * Assert that the first date is before than the second one.
     * 
     * <pre>
     * Assertor.that(date1).isBefore(date2).toThrow();
     * Assertor.that(date1).isBefore(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @return the operator
     */
    public Operator<AssertDate, Date> isBefore(final Date date) {
        return super.isBefore(date);
    }

    /**
     * Assert that the first date is before than the second one.
     * 
     * <pre>
     * Assertor.that(date1).isBefore(date2).toThrow();
     * Assertor.that(date1).isBefore(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertDate, Date> isBefore(final Date date, final CharSequence message, final Object... arguments) {
        return super.isBefore(date, message, arguments);
    }

    /**
     * Assert that the first date is before than the second one.
     * 
     * <pre>
     * Assertor.that(date1).isBefore(date2).toThrow();
     * Assertor.that(date1).isBefore(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertDate, Date> isBefore(final Date date, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return super.isBefore(date, locale, message, arguments);
    }

    /**
     * Assert that the first date is before than or equal to the second one.
     * 
     * <pre>
     * Assertor.that(date1).isBeforeOrEquals(date2).toThrow();
     * Assertor.that(date1).isBeforeOrEquals(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @return the operator
     */
    public Operator<AssertDate, Date> isBeforeOrEquals(final Date date) {
        return super.isBeforeOrEquals(date);
    }

    /**
     * Assert that the first date is before than or equal to the second one.
     * 
     * <pre>
     * Assertor.that(date1).isBeforeOrEquals(date2).toThrow();
     * Assertor.that(date1).isBeforeOrEquals(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertDate, Date> isBeforeOrEquals(final Date date, final CharSequence message, final Object... arguments) {
        return super.isBeforeOrEquals(date, message, arguments);
    }

    /**
     * Assert that the first date is before than or equal to the second one.
     * 
     * <pre>
     * Assertor.that(date1).isBeforeOrEquals(date2).toThrow();
     * Assertor.that(date1).isBeforeOrEquals(date2).isOK();
     * </pre>
     * 
     * @param date
     *            The second date
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertDate, Date> isBeforeOrEquals(final Date date, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return super.isBeforeOrEquals(date, locale, message, arguments);
    }
}
