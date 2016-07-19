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
     * Assertor.that(date1).isEqual(date2).getResult();
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
    public Operator<AssertDate, Date> isNotEqual(final Date date) {
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
    public Operator<AssertDate, Date> isAround(final Date date, final int calendarField, final int calendarAmount) {
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
    public Operator<AssertDate, Date> isNotAround(final Date date, final int calendarField, final int calendarAmount) {
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
    public Operator<AssertDate, Date> isAfter(final Date date) {
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
    public Operator<AssertDate, Date> isAfterOrEqual(final Date date) {
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
    public Operator<AssertDate, Date> isBefore(final Date date) {
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
    public Operator<AssertDate, Date> isBeforeOrEqual(final Date date) {
        return super.isBeforeOrEqual(date);
    }
}
