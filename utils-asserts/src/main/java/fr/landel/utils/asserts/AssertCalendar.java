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

/**
 * Assertion utility class that assists in validating arguments for dates.
 * 
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public class AssertCalendar extends AbstractAssertDate<AssertCalendar, Calendar> {

    /**
     * 
     * Constructor
     * 
     * @param object
     *            The calendar to check
     */
    protected AssertCalendar(final Calendar object) {
        super(object);
    }

    /**
     * Assert that the first date is equal to the second one.
     * 
     * <pre>
     * Assertor.that(date1).isEqual(date2).toThrow();
     * Assertor.that(date1).isEqual(date2).getResult();
     * </pre>
     * 
     * @param date
     *            The second date
     * @return the operator
     */
    public Operator<AssertCalendar, Calendar> isEqual(final Calendar date) {
        return super.isEqual(date);
    }

    /**
     * Assert that the first date is not equal to the second one.
     * 
     * <pre>
     * Assertor.that(date1).isNotEqual(date2).toThrow();
     * Assertor.that(date1).isNotEqual(date2).getResult();
     * </pre>
     * 
     * @param date
     *            The second date
     * @return the operator
     */
    public Operator<AssertCalendar, Calendar> isNotEqual(final Calendar date) {
        return super.isNotEqual(date);
    }

    /**
     * Assert that the first date is around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * Assertor.that(date1).isAround(date2, Calendar.MINUTE, 5).toThrow();
     * Assertor.that(date1).isAround(date2, Calendar.MINUTE, 5).getResult();
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
     * @return the operator
     */
    public Operator<AssertCalendar, Calendar> isAround(final Calendar date, final int calendarField, final int calendarAmount) {
        return super.isAround(date, calendarField, calendarAmount);
    }

    /**
     * Assert that the first date is not around to the second one following the
     * calendar field and amount.
     * 
     * <pre>
     * Assertor.that(date1).isNotAround(date2, Calendar.MINUTE, 5).toThrow();
     * Assertor.that(date1).isNotAround(date2, Calendar.MINUTE, 5).getResult();
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
     * @return the operator
     */
    public Operator<AssertCalendar, Calendar> isNotAround(final Calendar date, final int calendarField, final int calendarAmount) {
        return super.isNotAround(date, calendarField, calendarAmount);
    }

    /**
     * Assert that the first date is after than the second one.
     * 
     * <pre>
     * Assertor.that(date1).isAfter(date2).toThrow();
     * Assertor.that(date1).isAfter(date2).getResult();
     * </pre>
     * 
     * @param date
     *            The second date
     * @return the operator
     */
    public Operator<AssertCalendar, Calendar> isAfter(final Calendar date) {
        return super.isAfter(date);
    }

    /**
     * Assert that the first date is after than or equal to the second one.
     * 
     * <pre>
     * Assertor.that(date1).isAfterOrEqual(date2).toThrow();
     * Assertor.that(date1).isAfterOrEqual(date2).getResult();
     * </pre>
     * 
     * @param date
     *            The second date
     * @return the operator
     */
    public Operator<AssertCalendar, Calendar> isAfterOrEqual(final Calendar date) {
        return super.isAfterOrEqual(date);
    }

    /**
     * Assert that the first date is before than the second one.
     * 
     * <pre>
     * Assertor.that(date1).isBefore(date2).toThrow();
     * Assertor.that(date1).isBefore(date2).getResult();
     * </pre>
     * 
     * @param date
     *            The second date
     * @return the operator
     */
    public Operator<AssertCalendar, Calendar> isBefore(final Calendar date) {
        return super.isBefore(date);
    }

    /**
     * Assert that the first date is before than or equal to the second one.
     * 
     * <pre>
     * Assertor.that(date1).isBeforeOrEqual(date2).toThrow();
     * Assertor.that(date1).isBeforeOrEqual(date2).getResult();
     * </pre>
     * 
     * @param date
     *            The second date
     * @return the operator
     */
    public Operator<AssertCalendar, Calendar> isBeforeOrEqual(final Calendar date) {
        return super.isBeforeOrEqual(date);
    }
}