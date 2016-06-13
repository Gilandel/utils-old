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

import java.util.Calendar;
import java.util.Date;

import fr.landel.utils.commons.DateUtils;

/**
 * Assertion utility class that assists in validating arguments for dates.
 * 
 * Future: manage datetime J8, offset, timezone...
 * 
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public class AssertDate extends AbstractAssertDate<AssertDate, Date> {

    protected AssertDate(final Date object) {
        super(object);
    }

    /**
     * Assert that the first date is equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isEqual(date2);
     * </pre>
     * 
     * @param date
     *            The second date
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date are not
     *             equal.
     */
    public AssertDate isEqual(final Date date) {
        return this.isEqual(date, (CharSequence) null);
    }

    /**
     * Assert that the first date is equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isEqual(date2, &quot;The dates are not equal&quot;);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date are not
     *             equal.
     */
    public AssertDate isEqual(final Date date, final CharSequence message, final Object... arguments) {
        AssertDate.isAround(this.get(), date, -1, -1, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first date is equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isEqual(date2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one date is {@code null} and if date are not
     *             equal. The standard exception is appended as suppressed.
     */
    public <E extends Throwable> AssertDate isEqual(final Date date, final E exception) throws E {
        AssertDate.isAround(this.get(), date, -1, -1, exception, null);

        return this;
    }

    /**
     * Assert that the first date is equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isEqual(date2);
     * </pre>
     * 
     * @param date
     *            The second date
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date are not
     *             equal.
     */
    public AssertDate isEqual(final Calendar date) {
        return this.isEqual(date, (CharSequence) null);
    }

    /**
     * Assert that the first date is equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isEqual(date2, &quot;The dates are not equal&quot;);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date are not
     *             equal.
     */
    public AssertDate isEqual(final Calendar date, final CharSequence message, final Object... arguments) {
        AssertDate.isAround(DateUtils.getCalendar(this.get()), date, -1, -1, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first date is equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isEqual(date2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one date is {@code null} and if date are not
     *             equal. The standard exception is appended as suppressed.
     */
    public <E extends Throwable> AssertDate isEqual(final Calendar date, final E exception) throws E {
        AssertDate.isAround(DateUtils.getCalendar(this.get()), date, -1, -1, exception, null);

        return this;
    }

    /**
     * Assert that the first date is around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.check(date1).isAround(date2, Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @return this
     */
    public AssertDate isAround(final Date date, final int calendarField, final int calendarAmount) {
        return this.isAround(date, calendarField, calendarAmount, (CharSequence) null);
    }

    /**
     * Assert that the first date is around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.check(date1).isAround(date2, Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date
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
     * @return this
     */
    public AssertDate isAround(final Date date, final int calendarField, final int calendarAmount, final CharSequence message,
            final Object... arguments) {
        AssertDate.isAround(this.get(), date, calendarField, calendarAmount, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first date is around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.check(date1).isAround(date2, Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one date is {@code null} and if date are not
     *             equal. The standard exception is appended as suppressed.
     */
    public <E extends Throwable> AssertDate isAround(final Date date, final int calendarField, final int calendarAmount, final E exception)
            throws E {
        AssertDate.isAround(this.get(), date, calendarField, calendarAmount, exception, null);

        return this;
    }

    /**
     * Assert that the first date is around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.check(date1).isAround(date2, Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @return this
     */
    public AssertDate isAround(final Calendar date, final int calendarField, final int calendarAmount) {
        return this.isAround(date, calendarField, calendarAmount, (CharSequence) null);
    }

    /**
     * Assert that the first date is around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.check(date1).isAround(date2, Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date
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
     * @return this
     */
    public AssertDate isAround(final Calendar date, final int calendarField, final int calendarAmount, final CharSequence message,
            final Object... arguments) {
        AssertDate.isAround(DateUtils.getCalendar(this.get()), date, calendarField, calendarAmount, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first date is around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.check(date1).isAround(date2, Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one date is {@code null} and if date are not
     *             equal. The standard exception is appended as suppressed.
     */
    public <E extends Throwable> AssertDate isAround(final Calendar date, final int calendarField, final int calendarAmount,
            final E exception) throws E {
        AssertDate.isAround(DateUtils.getCalendar(this.get()), date, calendarField, calendarAmount, exception, null);

        return this;
    }

    /**
     * Assert that the first date is not around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.check(date1).isNotAround(date2, Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes not around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @return this
     */
    public AssertDate isNotAround(final Date date, final int calendarField, final int calendarAmount) {
        return this.isNotAround(date, calendarField, calendarAmount, (CharSequence) null);
    }

    /**
     * Assert that the first date is not around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.check(date1).isNotAround(date2, Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes not around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date
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
     * @return this
     */
    public AssertDate isNotAround(final Date date, final int calendarField, final int calendarAmount, final CharSequence message,
            final Object... arguments) {
        AssertDate.isNotAround(this.get(), date, calendarField, calendarAmount, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first date is not around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.check(date1).isNotAround(date2, Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes not around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             If date are around. The standard exception is appended as
     *             suppressed.
     */
    public <E extends Throwable> AssertDate isNotAround(final Date date, final int calendarField, final int calendarAmount,
            final E exception) throws E {
        AssertDate.isNotAround(this.get(), date, calendarField, calendarAmount, exception, null);

        return this;
    }

    /**
     * Assert that the first date is not around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.check(date1).isNotAround(date2, Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes not around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @return this
     */
    public AssertDate isNotAround(final Calendar date, final int calendarField, final int calendarAmount) {
        this.isNotAround(date, calendarField, calendarAmount, (CharSequence) null);

        return this;
    }

    /**
     * Assert that the first date is not around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.check(date1).isNotAround(date2, Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes not around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date
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
     * @return this
     */
    public AssertDate isNotAround(final Calendar date, final int calendarField, final int calendarAmount, final CharSequence message,
            final Object... arguments) {
        AssertDate.isNotAround(DateUtils.getCalendar(this.get()), date, calendarField, calendarAmount, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first date is not around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * AssertUtils.check(date1).isNotAround(date2, Calendar.MINUTE, 5, exceptionToThrowOnError);
     * // Check that the first date is 5 minutes not around the second one.
     * </pre>
     * 
     * @see #SUPPORTED_FIELDS
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param calendarField
     *            The calendar field
     * @param calendarAmount
     *            The calendar amount
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             If date are around. The standard exception is appended as
     *             suppressed.
     */
    public <E extends Throwable> AssertDate isNotAround(final Calendar date, final int calendarField, final int calendarAmount,
            final E exception) throws E {
        AssertDate.isNotAround(DateUtils.getCalendar(this.get()), date, calendarField, calendarAmount, exception, null);

        return this;
    }

    /**
     * Assert that the first date is not equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isNotEqual(date2);
     * </pre>
     * 
     * @param date
     *            The second date
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date are equal.
     */
    public AssertDate isNotEqual(final Date date) {
        return this.isNotEqual(date, (CharSequence) null);
    }

    /**
     * Assert that the first date is not equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isNotEqual(date2, &quot;The dates are equal&quot;);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date are equal.
     */
    public AssertDate isNotEqual(final Date date, final CharSequence message, final Object... arguments) {
        AssertDate.isNotEqual(this.get(), date, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first date is not equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isNotEqual(date2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one date is {@code null} and if date are equal.
     *             The standard exception is appended as suppressed.
     */
    public <E extends Throwable> AssertDate isNotEqual(final Date date, final E exception) throws E {
        AssertDate.isNotEqual(this.get(), date, exception, null);

        return this;
    }

    /**
     * Assert that the first date is not equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isNotEqual(date2);
     * </pre>
     * 
     * @param date
     *            The second date
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date are equal.
     */
    public AssertDate isNotEqual(final Calendar date) {
        return this.isNotEqual(date, (CharSequence) null);
    }

    /**
     * Assert that the first date is not equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isNotEqual(date2, &quot;The dates are equal&quot;);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date are equal.
     */
    public AssertDate isNotEqual(final Calendar date, final CharSequence message, final Object... arguments) {
        AssertDate.isNotEqual(this.get(), date, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first date is not equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isNotEqual(date2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param date
     *            The second date message the exception message, use the default
     *            assertion if null
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one date is {@code null} and if date are equal.
     *             The standard exception is appended as suppressed.
     */
    public <E extends Throwable> AssertDate isNotEqual(final Calendar date, final E exception) throws E {
        AssertDate.isNotEqual(this.get(), date, exception, null);

        return this;
    }

    /**
     * Assert that the first date is after than the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isAfter(date2);
     * </pre>
     * 
     * @param date
     *            The second date
     * @return this
     * @throws IllegalArgumentException
     *             If at least one date is {@code null} and if date1 is not
     *             after than date2.
     */
    public AssertDate isAfter(final Date date) {
        return this.isAfter(date, (CharSequence) null);
    }

    /**
     * Assert that the first date is after than the second one. In message,
     * parameters can be used through format '%p' (first %p will be replaced by
     * first parameter, second...).
     * 
     * 
     * <pre>
     * AssertUtils.check(date1).isAfter(date2, &quot;The date1 is not after than date2&quot;);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             If at least one date is {@code null} and if date is not after
     *             than date2
     */
    public AssertDate isAfter(final Date date, final CharSequence message, final Object... arguments) {
        AssertDate.isAfter(this.get(), date, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first date is after than the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isAfter(date2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             If at least one date is {@code null} and if date is not after
     *             than date2. The standard exception is appended as suppressed.
     */
    public <E extends Throwable> AssertDate isAfter(final Date date, final E exception) throws E {
        AssertDate.isAfter(this.get(), date, exception, null);

        return this;
    }

    /**
     * Assert that the first date is after than the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isAfter(date2);
     * </pre>
     * 
     * @param date
     *            The second date
     * @return this
     * @throws IllegalArgumentException
     *             If at least one date is {@code null} and if date1 is not
     *             after than date2.
     */
    public AssertDate isAfter(final Calendar date) {
        return this.isAfter(date, (CharSequence) null);
    }

    /**
     * Assert that the first date is after than the second one. In message,
     * parameters can be used through format '%p' (first %p will be replaced by
     * first parameter, second...).
     * 
     * 
     * <pre>
     * AssertUtils.check(date1).isAfter(date2, &quot;The date1 is not after than date2&quot;);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             If at least one date is {@code null} and if date is not after
     *             than date2
     */
    public AssertDate isAfter(final Calendar date, final CharSequence message, final Object... arguments) {
        AssertDate.isAfter(DateUtils.getCalendar(this.get()), date, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first date is after than the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isAfter(date2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             If at least one date is {@code null} and if date is not after
     *             than date2. The standard exception is appended as suppressed.
     */
    public <E extends Throwable> AssertDate isAfter(final Calendar date, final E exception) throws E {
        AssertDate.isAfter(DateUtils.getCalendar(this.get()), date, exception, null);

        return this;
    }

    /**
     * Assert that the first date is after than or equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isAfterOrEqual(date2);
     * </pre>
     * 
     * @param date
     *            The second date
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is before
     *             than date2.
     */
    public AssertDate isAfterOrEqual(final Date date) {
        return this.isAfterOrEqual(date, (CharSequence) null);
    }

    /**
     * Assert that the first date is after than or equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isAfterOrEqual(date2, &quot;The date1 is not after than and not equal to date2&quot;);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is before
     *             than date2.
     */
    public AssertDate isAfterOrEqual(final Date date, final CharSequence message, final Object... arguments) {
        AssertDate.isAfterOrEqual(this.get(), date, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first date is after than or equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isAfterOrEqual(date2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one date is {@code null} and if date1 is before
     *             than date2. The standard exception is appended as suppressed.
     */
    public <E extends Throwable> AssertDate isAfterOrEqual(final Date date, final E exception) throws E {
        AssertDate.isAfterOrEqual(this.get(), date, exception, null);

        return this;
    }

    /**
     * Assert that the first date is after than or equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isAfterOrEqual(date2);
     * </pre>
     * 
     * @param date
     *            The second date
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is before
     *             than date2.
     */
    public AssertDate isAfterOrEqual(final Calendar date) {
        return this.isAfterOrEqual(date, (CharSequence) null);
    }

    /**
     * Assert that the first date is after than or equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isAfterOrEqual(date2, &quot;The date1 is not after than and not equal to date2&quot;);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is before
     *             than date2.
     */
    public AssertDate isAfterOrEqual(final Calendar date, final CharSequence message, final Object... arguments) {
        AssertDate.isAfterOrEqual(DateUtils.getCalendar(this.get()), date, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first date is after than or equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isAfterOrEqual(date2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one date is {@code null} and if date1 is before
     *             than date2. The standard exception is appended as suppressed.
     */
    public <E extends Throwable> AssertDate isAfterOrEqual(final Calendar date, final E exception) throws E {
        AssertDate.isAfterOrEqual(DateUtils.getCalendar(this.get()), date, exception, null);

        return this;
    }

    /**
     * Assert that the first date is before than the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isBefore(date2);
     * </pre>
     * 
     * @param date
     *            The second date
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is not
     *             before than date2.
     */
    public AssertDate isBefore(final Date date) {
        return this.isBefore(date, (CharSequence) null);
    }

    /**
     * Assert that the first date is before than the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isBefore(date2, &quot;The date1 is not before than date2&quot;);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is not
     *             before than date2.
     */
    public AssertDate isBefore(final Date date, final CharSequence message, final Object... arguments) {
        AssertDate.isBefore(this.get(), date, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first date is before than the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isBefore(date2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @return this
     * @throws E
     *             if at least one date is {@code null} and if date1 is not
     *             before than date2. The standard exception is appended as
     *             suppressed.
     */
    public <E extends Throwable> AssertDate isBefore(final Date date, final E exception) throws E {
        AssertDate.isBefore(this.get(), date, exception, null);

        return this;
    }

    /**
     * Assert that the first date is before than the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isBefore(date2);
     * </pre>
     * 
     * @param date
     *            The second date
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is not
     *             before than date2.
     */
    public AssertDate isBefore(final Calendar date) {
        return this.isBefore(date, (CharSequence) null);
    }

    /**
     * Assert that the first date is before than the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isBefore(date2, &quot;The date1 is not before than date2&quot;);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is not
     *             before than date2.
     */
    public AssertDate isBefore(final Calendar date, final CharSequence message, final Object... arguments) {
        AssertDate.isBefore(DateUtils.getCalendar(this.get()), date, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first date is before than the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isBefore(date2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one date is {@code null} and if date1 is not
     *             before than date2. The standard exception is appended as
     *             suppressed.
     */
    public <E extends Throwable> AssertDate isBefore(final Calendar date, final E exception) throws E {
        AssertDate.isBefore(DateUtils.getCalendar(this.get()), date, exception, null);

        return this;
    }

    /**
     * Assert that the first date is before than or equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isBeforeOrEqual(date2);
     * </pre>
     * 
     * @param date
     *            The second date
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is after
     *             than date2.
     */
    public AssertDate isBeforeOrEqual(final Date date) {
        this.isBeforeOrEqual(date, (CharSequence) null);

        return this;
    }

    /**
     * Assert that the first date is before than or equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isBeforeOrEqual(date2, &quot;The date1 is not before than and not equal to date2&quot;);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is after
     *             than date2.
     */
    public AssertDate isBeforeOrEqual(final Date date, final CharSequence message, final Object... arguments) {
        AssertDate.isBeforeOrEqual(this.get(), date, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first date is before than or equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isBeforeOrEqual(date2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one date is {@code null} and if date1 is after
     *             than date2. The standard exception is appended as suppressed.
     */
    public <E extends Throwable> AssertDate isBeforeOrEqual(final Date date, final E exception) throws E {
        AssertDate.isBeforeOrEqual(this.get(), date, exception, null);

        return this;
    }

    /**
     * Assert that the first date is before than or equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isBeforeOrEqual(date2);
     * </pre>
     * 
     * @param date
     *            The second date
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is after
     *             than date2.
     */
    public AssertDate isBeforeOrEqual(final Calendar date) {
        return this.isBeforeOrEqual(date, (CharSequence) null);
    }

    /**
     * Assert that the first date is before than or equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isBeforeOrEqual(date2, &quot;The date1 is not before than and not equal to date2&quot;);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if at least one date is {@code null} and if date1 is after
     *             than date2.
     */
    public AssertDate isBeforeOrEqual(final Calendar date, final CharSequence message, final Object... arguments) {
        AssertDate.isBeforeOrEqual(DateUtils.getCalendar(this.get()), date, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first date is before than or equal to the second one.
     * 
     * <pre>
     * AssertUtils.check(date1).isBeforeOrEqual(date2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param date
     *            The second date
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one date is {@code null} and if date1 is after
     *             than date2. The standard exception is appended as suppressed.
     */
    public <E extends Throwable> AssertDate isBeforeOrEqual(final Calendar date, final E exception) throws E {
        AssertDate.isBeforeOrEqual(DateUtils.getCalendar(this.get()), date, exception, null);

        return this;
    }
}