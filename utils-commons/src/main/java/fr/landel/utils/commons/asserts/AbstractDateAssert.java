/*
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons.asserts;

import java.util.Date;

/**
 * Assertion utility class that assists in validating arguments for dates.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public abstract class AbstractDateAssert extends AbstractNumberAssert {

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
        isEqual(date1, date2, (String) null);
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
    public static void isEqual(final Date date1, final Date date2, final String message, final Object... arguments) {
        isEqual(date1, date2, null, message, arguments);
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
        isEqual(date1, date2, exception, null);
    }

    private static <E extends Throwable> void isEqual(final Date date1, final Date date2, final E exception, final String message,
            final Object... arguments) throws E {
        if (compareDate(date1, date2, exception) != 0) {
            manageExceptions("date1 is not equal to date2.", exception, message, new Object[] {date1, date2}, arguments);
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
    public static <N extends Number> void isNotEqual(final Date date1, final Date date2) {
        isNotEqual(date1, date2, (String) null);
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
    public static void isNotEqual(final Date date1, final Date date2, final String message, final Object... arguments) {
        if (compareDate(date1, date2, message, arguments) == 0) {
            throw new IllegalArgumentException(getMessage("date1 is equal to date2.", message, new Object[] {date1, date2}, arguments));
        }
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
        if (compareDate(date1, date2, exception) == 0) {
            exception.addSuppressed(new IllegalArgumentException("date1 is equal to date2."));
            throw exception;
        }
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
        AbstractDateAssert.isAfter(date1, date2, (String) null);
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
    public static void isAfter(final Date date1, final Date date2, final String message, final Object... arguments) {
        if (compareDate(date1, date2, message, arguments) <= 0) {
            throw new IllegalArgumentException(
                    getMessage("date1 is not after than date2.", message, new Object[] {date1, date2}, arguments));
        }
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
        if (compareDate(date1, date2, exception) <= 0) {
            exception.addSuppressed(new IllegalArgumentException("date1 is not after than date2."));
            throw exception;
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
        AbstractDateAssert.isAfterOrEqual(date1, date2, (String) null);
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
    public static void isAfterOrEqual(final Date date1, final Date date2, final String message, final Object... arguments) {
        if (compareDate(date1, date2, message, arguments) < 0) {
            throw new IllegalArgumentException(
                    getMessage("date1 is not after than or equal to date2.", message, new Object[] {date1, date2}, arguments));
        }
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
        if (compareDate(date1, date2, exception) < 0) {
            exception.addSuppressed(new IllegalArgumentException("date1 is not after than or equal to date2."));
            throw exception;
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
        AbstractDateAssert.isBefore(date1, date2, (String) null);
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
    public static void isBefore(final Date date1, final Date date2, final String message, final Object... arguments) {
        if (compareDate(date1, date2, message, arguments) >= 0) {
            throw new IllegalArgumentException(
                    getMessage("date1 is not before than date2.", message, new Object[] {date1, date2}, arguments));
        }
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
        if (compareDate(date1, date2, exception) >= 0) {
            exception.addSuppressed(new IllegalArgumentException("date1 is not not before than date2."));
            throw exception;
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
        AbstractDateAssert.isBeforeOrEqual(date1, date2, (String) null);
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
    public static void isBeforeOrEqual(final Date date1, final Date date2, final String message, final Object... arguments) {
        if (compareDate(date1, date2, message, arguments) > 0) {
            throw new IllegalArgumentException(
                    getMessage("date1 is not before than or equal to date2.", message, new Object[] {date1, date2}, arguments));
        }
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
        if (compareDate(date1, date2, exception) > 0) {
            exception.addSuppressed(new IllegalArgumentException("date1 is not before than or equal to date2."));
            throw exception;
        }
    }

    private static int compareDate(final Date date1, final Date date2, final String message, final Object... arguments) {
        if (date1 != null && date2 != null) {
            return date1.compareTo(date2);
        } else if (date1 != null) {
            throw new IllegalArgumentException(getMessage("date1 is null.", message, new Object[] {date1, date2}, arguments));
        } else if (date2 != null) {
            throw new IllegalArgumentException(getMessage("date2 is null.", message, new Object[] {date1, date2}, arguments));
        } else {
            throw new IllegalArgumentException(getMessage("date1 and date2 are null.", message, new Object[] {date1, date2}, arguments));
        }
    }

    private static <E extends Throwable> int compareDate(final Date date1, final Date date2, final E exception) throws E {
        if (date1 != null && date2 != null) {
            return date1.compareTo(date2);
        } else if (date1 != null) {
            exception.addSuppressed(new IllegalArgumentException("date1 is null."));
        } else if (date2 != null) {
            exception.addSuppressed(new IllegalArgumentException("date2 is null."));
        } else {
            exception.addSuppressed(new IllegalArgumentException("date1 and date2 are null."));
        }
        throw exception;
    }
}
