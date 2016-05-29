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

/**
 * Assertion utility class that assists in validating arguments for dates.
 * 
 * Future: manage datetime J8, offset, timezone... manage calendar field for
 * before and after methods.
 * 
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public abstract class AbstractDateAssert extends AbstractNumberAssert {

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
     * Assert that the first date is equal to the second one.
     * 
     * <pre>
     * AssertUtils.isEqual(new Date(), new Date());
     * </pre>
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date are not
     *             equal.
     */
    public static void isEqual(final Date date1, final Date date2) {
        AbstractDateAssert.isEqual(date1, date2, (CharSequence) null);
    }

    /**
     * Assert that the first date is equal to the second one.
     * 
     * <pre>
     * AssertUtils.isEqual(new Date(), new Date(), &quot;The dates are not equal&quot;);
     * </pre>
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date are not
     *             equal.
     */
    public static void isEqual(final Date date1, final Date date2, final CharSequence message, final Object... arguments) {
        AbstractDateAssert.isAround(date1, date2, -1, -1, null, message, arguments);
    }

    /**
     * Assert that the first date is equal to the second one.
     * 
     * <pre>
     * AssertUtils.isEqual(new Date(), new Date(), exceptionToThrowOnError);
     * </pre>
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one date is {@code null} and if date are not
     *             equal. The standard exception is appended as suppressed.
     */
    public static <E extends Throwable> void isEqual(final Date date1, final Date date2, final E exception) throws E {
        AbstractDateAssert.isAround(date1, date2, -1, -1, exception, null);
    }

    /**
     * Assert that the first date is around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.isAround(new Date(), new Date(), Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     */
    public static void isAround(final Date date1, final Date date2, final int calendarField, final int calendarAmount) {
        AbstractDateAssert.isAround(date1, date2, calendarField, calendarAmount, (CharSequence) null);
    }

    /**
     * Assert that the first date is around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.isAround(new Date(), new Date(), Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     *            IllegalArgumentException if at least one date is {@code null}
     *            and if date are not around.
     */
    public static void isAround(final Date date1, final Date date2, final int calendarField, final int calendarAmount,
            final CharSequence message, final Object... arguments) {
        AbstractDateAssert.isAround(date1, date2, calendarField, calendarAmount, null, message, arguments);
    }

    /**
     * Assert that the first date is around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.isAround(new Date(), new Date(), Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one date is {@code null} and if date are not
     *             equal. The standard exception is appended as suppressed.
     */
    public static <E extends Throwable> void isAround(final Date date1, final Date date2, final int calendarField, final int calendarAmount,
            final E exception) throws E {
        AbstractDateAssert.isAround(date1, date2, calendarField, calendarAmount, exception, null);
    }

    private static <E extends Throwable> void isAround(final Date date1, final Date date2, final int calendarField,
            final int calendarAmount, final E exception, final CharSequence message, final Object... arguments) throws E {

        final int compare = AbstractDateAssert.compareDate(date1, date2, exception, null, null, false);

        if (compare != 0) {
            if (date1 != null && date2 != null && calendarField != -1) {
                if (!SUPPORTED_FIELDS.contains(calendarField)) {
                    AbstractAssert.manageExceptions("calendarField isn't correct see Calendar fields' list.", exception, message,
                            new Object[] {date1, date2, calendarField, calendarAmount}, arguments);
                } else if (calendarAmount == 0) {
                    AbstractAssert.manageExceptions("calendarAmount cannot be equal to 0.", exception, message,
                            new Object[] {date1, date2, calendarField, calendarAmount}, arguments);
                }

                if (!isAround(date1, date2, calendarField, calendarAmount, compare)) {
                    AbstractAssert.manageExceptions("date1 is not around to date2.", exception, message,
                            new Object[] {date1, date2, calendarField, calendarAmount}, arguments);
                }
            } else {
                AbstractAssert.manageExceptions("date1 is not equal to date2.", exception, message, new Object[] {date1, date2}, arguments);
            }
        }
    }

    private static boolean isAround(final Date date1, final Date date2, final int calendarField, final int calendarAmount,
            final int compare) {

        final Calendar calendar1 = Calendar.getInstance();
        final Calendar calendar2 = Calendar.getInstance();
        calendar1.setTime(date1);
        calendar2.setTime(date2);

        if (compare < 0) {
            calendar1.add(calendarField, calendarAmount);
            if (calendar1.before(calendar2)) {
                return false;
            }
        } else if (compare > 0) {
            calendar2.add(calendarField, calendarAmount);
            if (calendar2.before(calendar1)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Assert that the first date is not around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.isNotAround(new Date(), new Date(), Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes not around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     */
    public static void isNotAround(final Date date1, final Date date2, final int calendarField, final int calendarAmount) {
        AbstractDateAssert.isNotAround(date1, date2, calendarField, calendarAmount, (CharSequence) null);
    }

    /**
     * Assert that the first date is not around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.isNotAround(new Date(), new Date(), Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes not around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     *            IllegalArgumentException if date are around.
     */
    public static void isNotAround(final Date date1, final Date date2, final int calendarField, final int calendarAmount,
            final CharSequence message, final Object... arguments) {
        AbstractDateAssert.isNotAround(date1, date2, calendarField, calendarAmount, null, message, arguments);
    }

    /**
     * Assert that the first date is not around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.isNotAround(new Date(), new Date(), Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes not around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             If date are around. The standard exception is appended as
     *             suppressed.
     */
    public static <E extends Throwable> void isNotAround(final Date date1, final Date date2, final int calendarField,
            final int calendarAmount, final E exception) throws E {
        AbstractDateAssert.isNotAround(date1, date2, calendarField, calendarAmount, exception, null);
    }

    private static <E extends Throwable> void isNotAround(final Date date1, final Date date2, final int calendarField,
            final int calendarAmount, final E exception, final CharSequence message, final Object... arguments) throws E {

        final int compare = AbstractDateAssert.compareDate(date1, date2, exception, null, null, false);

        if (compare != 0 && date1 != null && date2 != null && calendarField != -1) {
            if (!SUPPORTED_FIELDS.contains(calendarField)) {
                AbstractAssert.manageExceptions("calendarField isn't correct see Calendar fields' list.", exception, message,
                        new Object[] {date1, date2, calendarField, calendarAmount}, arguments);
            } else if (calendarAmount == 0) {
                AbstractAssert.manageExceptions("calendarAmount cannot be equal to 0.", exception, message,
                        new Object[] {date1, date2, calendarField, calendarAmount}, arguments);
            }

            if (isAround(date1, date2, calendarField, calendarAmount, compare)) {
                AbstractAssert.manageExceptions("date1 is around to date2.", exception, message,
                        new Object[] {date1, date2, calendarField, calendarAmount}, arguments);
            }
        } else if (compare == 0) {
            AbstractAssert.manageExceptions("date1 is equal to date2.", exception, message, new Object[] {date1, date2}, arguments);
        }
    }

    /**
     * Assert that the first date is not equal to the second one.
     * 
     * <pre>
     * AssertUtils.isNotEqual(new Date(), new Date());
     * </pre>
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date are equal.
     */
    public static void isNotEqual(final Date date1, final Date date2) {
        AbstractDateAssert.isNotEqual(date1, date2, (CharSequence) null);
    }

    /**
     * Assert that the first date is not equal to the second one.
     * 
     * <pre>
     * AssertUtils.isNotEqual(new Date(), new Date(), &quot;The dates are equal&quot;);
     * </pre>
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date are equal.
     */
    public static void isNotEqual(final Date date1, final Date date2, final CharSequence message, final Object... arguments) {
        AbstractDateAssert.isNotEqual(date1, date2, null, message, arguments);
    }

    /**
     * Assert that the first date is not equal to the second one.
     * 
     * <pre>
     * AssertUtils.isNotEqual(new Date(), new Date(), exceptionToThrowOnError);
     * </pre>
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one date is {@code null} and if date are equal.
     *             The standard exception is appended as suppressed.
     */
    public static <E extends Throwable> void isNotEqual(final Date date1, final Date date2, final E exception) throws E {
        AbstractDateAssert.isNotEqual(date1, date2, exception, null);
    }

    private static <E extends Throwable> void isNotEqual(final Date date1, final Date date2, final E exception, final CharSequence message,
            final Object... arguments) throws E {
        AbstractDateAssert.isNotAround(date1, date2, -1, -1, exception, message, arguments);
    }

    /**
     * Assert that the first date is after than the second one.
     * 
     * <pre>
     * AssertUtils.isAfter(new Date(), new Date());
     * </pre>
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date
     * @throws IllegalArgumentException
     *             If at least one date is {@code null} and if date1 is not
     *             after than date2.
     */
    public static void isAfter(final Date date1, final Date date2) {
        AbstractDateAssert.isAfter(date1, date2, (CharSequence) null);
    }

    /**
     * Assert that the first date is after than the second one. In message,
     * parameters can be used through format '%p' (first %p will be replaced by
     * first parameter, second...).
     * 
     * 
     * <pre>
     * AssertUtils.isAfter(new Date(), new Date(), &quot;The date1 is not after than date2&quot;);
     * </pre>
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             If at least one date is {@code null} and if date is not after
     *             than date2
     */
    public static void isAfter(final Date date1, final Date date2, final CharSequence message, final Object... arguments) {
        AbstractDateAssert.isAfter(date1, date2, null, message, arguments);
    }

    /**
     * Assert that the first date is after than the second one.
     * 
     * <pre>
     * AssertUtils.isAfter(new Date(), new Date(), exceptionToThrowOnError);
     * </pre>
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             If at least one date is {@code null} and if date is not after
     *             than date2. The standard exception is appended as suppressed.
     */
    public static <E extends Throwable> void isAfter(final Date date1, final Date date2, final E exception) throws E {
        AbstractDateAssert.isAfter(date1, date2, exception, null);
    }

    private static <E extends Throwable> void isAfter(final Date date1, final Date date2, final E exception, final CharSequence message,
            final Object... arguments) throws E {
        if (AbstractDateAssert.compareDate(date1, date2, exception, null, null, true) <= 0) {
            AbstractAssert.manageExceptions("date1 is not after than date2.", exception, message, new Object[] {date1, date2}, arguments);
        }
    }

    /**
     * Assert that the first date is after than or equal to the second one.
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is before
     *             than date2.
     */
    public static void isAfterOrEqual(final Date date1, final Date date2) {
        AbstractDateAssert.isAfterOrEqual(date1, date2, (CharSequence) null);
    }

    /**
     * Assert that the first date is after than or equal to the second one.
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is before
     *             than date2.
     */
    public static void isAfterOrEqual(final Date date1, final Date date2, final CharSequence message, final Object... arguments) {
        AbstractDateAssert.isAfterOrEqual(date1, date2, null, message, arguments);
    }

    /**
     * Assert that the first date is after than or equal to the second one.
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one date is {@code null} and if date1 is before
     *             than date2. The standard exception is appended as suppressed.
     */
    public static <E extends Throwable> void isAfterOrEqual(final Date date1, final Date date2, final E exception) throws E {
        AbstractDateAssert.isAfterOrEqual(date1, date2, exception, null);
    }

    private static <E extends Throwable> void isAfterOrEqual(final Date date1, final Date date2, final E exception,
            final CharSequence message, final Object... arguments) throws E {
        if (AbstractDateAssert.compareDate(date1, date2, exception, null, null, true) < 0) {
            AbstractAssert.manageExceptions("date1 is not after than or equal to date2.", exception, message, new Object[] {date1, date2},
                    arguments);
        }
    }

    /**
     * Assert that the first date is before than the second one.
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is not
     *             before than date2.
     */
    public static void isBefore(final Date date1, final Date date2) {
        AbstractDateAssert.isBefore(date1, date2, (CharSequence) null);
    }

    /**
     * Assert that the first date is before than the second one.
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is not
     *             before than date2.
     */
    public static void isBefore(final Date date1, final Date date2, final CharSequence message, final Object... arguments) {
        AbstractDateAssert.isBefore(date1, date2, null, message, arguments);
    }

    /**
     * Assert that the first date is before than the second one.
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one date is {@code null} and if date1 is not
     *             before than date2. The standard exception is appended as
     *             suppressed.
     */
    public static <E extends Throwable> void isBefore(final Date date1, final Date date2, final E exception) throws E {
        AbstractDateAssert.isBefore(date1, date2, exception, null);
    }

    private static <E extends Throwable> void isBefore(final Date date1, final Date date2, final E exception, final CharSequence message,
            final Object... arguments) throws E {
        if (AbstractDateAssert.compareDate(date1, date2, exception, null, null, true) >= 0) {
            AbstractAssert.manageExceptions("date1 is not before than or equal to date2.", exception, message, new Object[] {date1, date2},
                    arguments);
        }
    }

    /**
     * Assert that the first date is before than or equal to the second one.
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is after
     *             than date2.
     */
    public static void isBeforeOrEqual(final Date date1, final Date date2) {
        AbstractDateAssert.isBeforeOrEqual(date1, date2, (CharSequence) null);
    }

    /**
     * Assert that the first date is before than or equal to the second one.
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is after
     *             than date2.
     */
    public static void isBeforeOrEqual(final Date date1, final Date date2, final CharSequence message, final Object... arguments) {
        AbstractDateAssert.isBeforeOrEqual(date1, date2, null, message, arguments);
    }

    /**
     * Assert that the first date is before than or equal to the second one.
     * 
     * @param date1
     *            The first date
     * @param date2
     *            The second date
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one date is {@code null} and if date1 is after
     *             than date2. The standard exception is appended as suppressed.
     */
    public static <E extends Throwable> void isBeforeOrEqual(final Date date1, final Date date2, final E exception) throws E {
        AbstractDateAssert.isBeforeOrEqual(date1, date2, exception, null);
    }

    private static <E extends Throwable> void isBeforeOrEqual(final Date date1, final Date date2, final E exception,
            final CharSequence message, final Object... arguments) throws E {
        if (AbstractDateAssert.compareDate(date1, date2, exception, null, null, true) > 0) {
            AbstractAssert.manageExceptions("date1 is not before than or equal to date2.", exception, message, new Object[] {date1, date2},
                    arguments);
        }
    }

    private static <E extends Throwable> int compareDate(final Date date1, final Date date2, final E exception, final CharSequence message,
            final Object[] arguments, final boolean throwIfNull) throws E {
        if (date1 != null && date2 != null) {
            return date1.compareTo(date2);
        } else if (throwIfNull) {
            if (date1 != null) {
                AbstractAssert.manageExceptions("date1 is null.", exception, message, new Object[] {date1}, arguments);
            } else if (date2 != null) {
                AbstractAssert.manageExceptions("date2 is null.", exception, message, new Object[] {date2}, arguments);
            }
            AbstractAssert.manageExceptions("date1 and date2 are null.", exception, message, new Object[] {date1, date2}, arguments);
        } else if (date1 != null) {
            return 1;
        } else if (date2 != null) {
            return -1;
        }
        return 0;
    }
}
