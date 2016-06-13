/*
 * #%L
 * utils-asserts
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.asserts;

import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import fr.landel.utils.commons.DateUtils;

/**
 * Assertion utility class that assists in validating arguments for dates.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public class AbstractAssertDate<T extends AbstractAssertDate<T, O>, O extends Comparable<O>> extends AssertObject<T, O> {

    /**
     * Supported {@link Calendar} field
     * 
     * @see Calendar#ERA
     * @see Calendar#YEAR
     * @see Calendar#MONTH
     * @see Calendar#WEEK_OF_YEAR
     * @see Calendar#WEEK_OF_MONTH
     * @see Calendar#DATE
     * @see Calendar#DAY_OF_MONTH
     * @see Calendar#DAY_OF_YEAR
     * @see Calendar#DAY_OF_WEEK
     * @see Calendar#DAY_OF_WEEK_IN_MONTH
     * @see Calendar#HOUR
     * @see Calendar#HOUR_OF_DAY
     * @see Calendar#MINUTE
     * @see Calendar#SECOND
     * @see Calendar#MILLISECOND
     */
    public static final List<Integer> SUPPORTED_FIELDS = Arrays.asList(Calendar.ERA, Calendar.YEAR, Calendar.MONTH, Calendar.WEEK_OF_YEAR,
            Calendar.WEEK_OF_MONTH, Calendar.DATE, Calendar.DAY_OF_MONTH, Calendar.DAY_OF_YEAR, Calendar.DAY_OF_WEEK,
            Calendar.DAY_OF_WEEK_IN_MONTH, Calendar.HOUR, Calendar.HOUR_OF_DAY, Calendar.MINUTE, Calendar.SECOND, Calendar.MILLISECOND);

    /**
     * 
     * Constructor
     *
     * @param object
     *            the date or calendar input object
     */
    protected AbstractAssertDate(final O object) {
        super(object);
    }

    /**
     * Check if the first date is around the second one.
     * 
     * @param date1
     *            the first date
     * @param date2
     *            the second date
     * @param calendarField
     *            the calendar field
     * @param calendarAmount
     *            the calendar amount
     * @param exception
     *            the exception to throw on error
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     *            IllegalArgumentException if at least one date is {@code null}
     *            and if date are not around.
     * @param <T>
     *            the type of date ({@link Date} or {@link Calendar})
     * @param <E>
     *            the type of exception
     * @throws E
     *             if at least one date is {@code null} and if date are not
     *             closed. The standard exception is appended as suppressed.
     */
    protected static <T extends Comparable<T>, E extends Throwable> void isAround(final T date1, final T date2, final int calendarField,
            final int calendarAmount, final E exception, final CharSequence message, final Object... arguments) throws E {

        final int compare = AbstractAssertDate.compareDate(date1, date2, exception, null, null, false);

        if (compare != 0) {
            if (date1 != null && date2 != null && calendarField != -1) {
                if (!SUPPORTED_FIELDS.contains(calendarField)) {
                    AssertObject.manageExceptions("calendarField isn't correct see Calendar fields' list.", exception, message,
                            new Object[] {date1, date2, calendarField, calendarAmount}, arguments);
                } else if (calendarAmount == 0) {
                    AssertObject.manageExceptions("calendarAmount cannot be equal to 0.", exception, message,
                            new Object[] {date1, date2, calendarField, calendarAmount}, arguments);
                }

                if (!isAround(date1, date2, calendarField, calendarAmount, compare)) {
                    AssertObject.manageExceptions("date1 is not around to date2.", exception, message,
                            new Object[] {date1, date2, calendarField, calendarAmount}, arguments);
                }
            } else {
                AssertObject.manageExceptions("date1 is not equal to date2.", exception, message, new Object[] {date1, date2}, arguments);
            }
        }
    }

    private static <T> boolean isAround(final T date1, final T date2, final int calendarField, final int calendarAmount,
            final int compare) {

        Calendar calendar1;
        Calendar calendar2;

        if (Date.class.isAssignableFrom(date1.getClass())) {
            calendar1 = DateUtils.getCalendar((Date) date1);
            calendar2 = DateUtils.getCalendar((Date) date2);
        } else if (Calendar.class.isAssignableFrom(date1.getClass())) {
            calendar1 = (Calendar) ((Calendar) date1).clone();
            calendar2 = (Calendar) ((Calendar) date2).clone();
        } else {
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
     * Check if the first date is NOT around the second one.
     * 
     * @param date1
     *            the first date
     * @param date2
     *            the second date
     * @param calendarField
     *            the calendar field
     * @param calendarAmount
     *            the calendar amount
     * @param exception
     *            the exception to throw on error
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     *            IllegalArgumentException if at least one date is {@code null}
     *            and if dates are around.
     * @param <T>
     *            the type of date ({@link Date} or {@link Calendar})
     * @param <E>
     *            the type of exception
     * @throws E
     *             if at least one date is {@code null} and if dates are closed.
     *             The standard exception is appended as suppressed.
     */
    protected static <T extends Comparable<T>, E extends Throwable> void isNotAround(final T date1, final T date2, final int calendarField,
            final int calendarAmount, final E exception, final CharSequence message, final Object... arguments) throws E {

        final int compare = AbstractAssertDate.compareDate(date1, date2, exception, null, null, false);

        if (compare != 0 && date1 != null && date2 != null && calendarField != -1) {
            if (!SUPPORTED_FIELDS.contains(calendarField)) {
                AssertObject.manageExceptions("calendarField isn't correct see Calendar fields' list.", exception, message,
                        new Object[] {date1, date2, calendarField, calendarAmount}, arguments);
            } else if (calendarAmount == 0) {
                AssertObject.manageExceptions("calendarAmount cannot be equal to 0.", exception, message,
                        new Object[] {date1, date2, calendarField, calendarAmount}, arguments);
            }

            if (AbstractAssertDate.isAround(date1, date2, calendarField, calendarAmount, compare)) {
                AssertObject.manageExceptions("date1 is around to date2.", exception, message,
                        new Object[] {date1, date2, calendarField, calendarAmount}, arguments);
            }
        } else if (compare == 0) {
            AssertObject.manageExceptions("date1 is equal to date2.", exception, message, new Object[] {date1, date2}, arguments);
        }
    }

    /**
     * Check if the first date is NOT equal to the second one.
     * 
     * @param date1
     *            the first date
     * @param date2
     *            the second date
     * @param exception
     *            the exception to throw on error
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     *            IllegalArgumentException if at least one date is {@code null}
     *            and if dates are not equal.
     * @param <T>
     *            the type of date ({@link Date} or {@link Calendar})
     * @param <E>
     *            the type of exception
     * @throws E
     *             if at least one date is {@code null} and if dates are equal.
     *             The standard exception is appended as suppressed.
     */
    protected static <T extends Comparable<T>, E extends Throwable> void isNotEqual(final T date1, final T date2, final E exception,
            final CharSequence message, final Object... arguments) throws E {
        AbstractAssertDate.isNotAround(date1, date2, -1, -1, exception, message, arguments);
    }

    /**
     * Check if the first date is after the second one.
     * 
     * @param date1
     *            the first date
     * @param date2
     *            the second date
     * @param exception
     *            the exception to throw on error
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     *            IllegalArgumentException if at least one date is {@code null}
     *            and if date is not after the other one.
     * @param <T>
     *            the type of date ({@link Date} or {@link Calendar})
     * @param <E>
     *            the type of exception
     * @throws E
     *             if at least one date is {@code null} and if dates is NOT
     *             after. The standard exception is appended as suppressed.
     */
    protected static <T extends Comparable<T>, E extends Throwable> void isAfter(final T date1, final T date2, final E exception,
            final CharSequence message, final Object... arguments) throws E {
        if (AbstractAssertDate.compareDate(date1, date2, exception, null, null, true) <= 0) {
            AssertObject.manageExceptions("date1 is not after than date2.", exception, message, new Object[] {date1, date2}, arguments);
        }
    }

    /**
     * Check if the first date is after or equal to the second one.
     * 
     * @param date1
     *            the first date
     * @param date2
     *            the second date
     * @param exception
     *            the exception to throw on error
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     *            IllegalArgumentException if at least one date is {@code null}
     *            and if date is not after and not equal the other one.
     * @param <T>
     *            the type of date ({@link Date} or {@link Calendar})
     * @param <E>
     *            the type of exception
     * @throws E
     *             if at least one date is {@code null} and if dates is NOT
     *             after and NOT equal. The standard exception is appended as
     *             suppressed.
     */
    protected static <T extends Comparable<T>, E extends Throwable> void isAfterOrEqual(final T date1, final T date2, final E exception,
            final CharSequence message, final Object... arguments) throws E {
        if (AbstractAssertDate.compareDate(date1, date2, exception, null, null, true) < 0) {
            AssertObject.manageExceptions("date1 is not after than or equal to date2.", exception, message, new Object[] {date1, date2},
                    arguments);
        }
    }

    /**
     * Check if the first date is before the second one.
     * 
     * @param date1
     *            the first date
     * @param date2
     *            the second date
     * @param exception
     *            the exception to throw on error
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     *            IllegalArgumentException if at least one date is {@code null}
     *            and if date is not before the other one.
     * @param <T>
     *            the type of date ({@link Date} or {@link Calendar})
     * @param <E>
     *            the type of exception
     * @throws E
     *             if at least one date is {@code null} and if dates is NOT
     *             before. The standard exception is appended as suppressed.
     */
    protected static <T extends Comparable<T>, E extends Throwable> void isBefore(final T date1, final T date2, final E exception,
            final CharSequence message, final Object... arguments) throws E {
        if (AbstractAssertDate.compareDate(date1, date2, exception, null, null, true) >= 0) {
            AssertObject.manageExceptions("date1 is not before than or equal to date2.", exception, message, new Object[] {date1, date2},
                    arguments);
        }
    }

    /**
     * Check if the first date is before or equal to the second one.
     * 
     * @param date1
     *            the first date
     * @param date2
     *            the second date
     * @param exception
     *            the exception to throw on error
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     *            IllegalArgumentException if at least one date is {@code null}
     *            and if date is not before and not equal the other one.
     * @param <T>
     *            the type of date ({@link Date} or {@link Calendar})
     * @param <E>
     *            the type of exception
     * @throws E
     *             if at least one date is {@code null} and if dates is NOT
     *             before and NOT equal. The standard exception is appended as
     *             suppressed.
     */
    protected static <T extends Comparable<T>, E extends Throwable> void isBeforeOrEqual(final T date1, final T date2, final E exception,
            final CharSequence message, final Object... arguments) throws E {
        if (AbstractAssertDate.compareDate(date1, date2, exception, null, null, true) > 0) {
            AssertObject.manageExceptions("date1 is not before than or equal to date2.", exception, message, new Object[] {date1, date2},
                    arguments);
        }
    }

    private static <T extends Comparable<T>, E extends Throwable> int compareDate(final T calendar1, final T calendar2, final E exception,
            final CharSequence message, final Object[] arguments, final boolean throwIfNull) throws E {
        if (calendar1 != null && calendar2 != null) {
            return calendar1.compareTo(calendar2);
        } else if (throwIfNull) {
            if (calendar1 != null) {
                AssertObject.manageExceptions("date1 is null.", exception, message, new Object[] {calendar1}, arguments);
            } else if (calendar2 != null) {
                AssertObject.manageExceptions("date2 is null.", exception, message, new Object[] {calendar2}, arguments);
            }
            AssertObject.manageExceptions("date1 and date2 are null.", exception, message, new Object[] {calendar1, calendar2}, arguments);
        } else if (calendar1 != null) {
            return 1;
        } else if (calendar2 != null) {
            return -1;
        }
        return 0;
    }
}