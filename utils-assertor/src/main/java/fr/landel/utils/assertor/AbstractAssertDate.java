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

import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

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
    protected AbstractAssertDate(final T object) {
        super(object);
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
     * Check if the first date is around the second one.
     * 
     * @param date2
     *            the second date
     * @param calendarField
     *            the calendar field
     * @param calendarAmount
     *            the calendar amount
     * @return the operator
     */
    protected Operator<A, T> isAround(final T date2, final int calendarField, final int calendarAmount) {

        final int compare = Comparators.compare(this.get(), date2);

        boolean condition = true;
        final StringBuilder message = new StringBuilder();

        if (compare != 0) {
            if (this.get() != null && date2 != null && calendarField != -1) {
                if (!SUPPORTED_FIELDS.contains(calendarField)) {
                    condition = false;
                    message.append("calendarField isn't correct see Calendar fields' list");
                } else if (calendarAmount == 0) {
                    condition = false;
                    message.append("calendarAmount cannot be equal to 0");
                } else if (!isAround(this.get(), date2, calendarField, calendarAmount, compare)) {
                    condition = false;
                    message.append("date '").append(this.getParam()).append("' is not around date '")
                            .append(AssertObject.getParam(this.getParamIndex() + 1)).append("'");
                }
            } else {
                condition = false;
                message.append("date '").append(this.getParam()).append("' is not equal to date '")
                        .append(AssertObject.getParam(this.getParamIndex() + 1)).append("'");
            }
        }
        return this.combine(condition, message, date2, calendarField, calendarAmount);
    }

    /**
     * Check if the first date is NOT around the second one.
     * 
     * @param date2
     *            the second date
     * @param calendarField
     *            the calendar field
     * @param calendarAmount
     *            the calendar amount
     * @return the operator
     */
    protected Operator<A, T> isNotAround(final T date2, final int calendarField, final int calendarAmount) {

        final int compare = Comparators.compare(this.get(), date2);

        boolean condition = true;
        final StringBuilder message = new StringBuilder();

        if (compare != 0 && this.get() != null && date2 != null && calendarField != -1) {
            if (!SUPPORTED_FIELDS.contains(calendarField)) {
                condition = false;
                message.append("calendarField isn't correct see Calendar fields' list");
            } else if (calendarAmount == 0) {
                condition = false;
                message.append("calendarAmount cannot be equal to 0");
            } else if (AbstractAssertDate.isAround(this.get(), date2, calendarField, calendarAmount, compare)) {
                condition = false;
                message.append("date '").append(this.getParam()).append("' is around date '")
                        .append(AssertObject.getParam(this.getParamIndex() + 1)).append("'");
            }
        } else if (compare == 0) {
            condition = false;
            message.append("date '").append(this.getParam()).append("' is equal to date '")
                    .append(AssertObject.getParam(this.getParamIndex() + 1)).append("'");
        }

        return this.combine(condition, message, date2, calendarField, calendarAmount);
    }

    /**
     * Check if the first date is equal to the second one.
     * 
     * @param date2
     *            the second date
     * @return the operator
     */
    protected Operator<A, T> isEqual(final T date2) {
        return this.isAround(date2, -1, -1);
    }

    /**
     * Check if the first date is NOT equal to the second one.
     * 
     * @param date2
     *            the second date
     * @return the operator
     */
    protected Operator<A, T> isNotEqual(final T date2) {
        return this.isNotAround(date2, -1, -1);
    }

    /**
     * Check if the first date is after the second one.
     * 
     * @param date2
     *            the second date
     * @return the operator
     */
    protected Operator<A, T> isAfter(final T date2) {
        return this.combine(this.get() != null && date2 != null && Comparators.compare(this.get(), date2) > 0,
                new StringBuilder("date '").append(this.getParam()).append("' is not after date '")
                        .append(AssertObject.getParam(this.getParamIndex() + 1)).append("'"),
                date2);
    }

    /**
     * Check if the first date is after or equal to the second one.
     * 
     * @param date2
     *            the second date
     * @return the operator
     */
    protected Operator<A, T> isAfterOrEqual(final T date2) {
        return this.combine(this.get() != null && date2 != null && Comparators.compare(this.get(), date2) >= 0,
                new StringBuilder("date '").append(this.getParam()).append("' is not after or equal to date '")
                        .append(AssertObject.getParam(this.getParamIndex() + 1)).append("'"),
                date2);
    }

    /**
     * Check if the first date is before the second one.
     * 
     * @param date2
     *            the second date
     * @return the operator
     */
    protected Operator<A, T> isBefore(final T date2) {
        return this.combine(this.get() != null && date2 != null && Comparators.compare(this.get(), date2) < 0,
                new StringBuilder("date '").append(this.getParam()).append("' is not before date '")
                        .append(AssertObject.getParam(this.getParamIndex() + 1)).append("'"),
                date2);
    }

    /**
     * Check if the first date is before or equal to the second one.
     * 
     * @param date2
     *            the second date
     * @return the operator
     */
    protected Operator<A, T> isBeforeOrEqual(final T date2) {
        return this.combine(this.get() != null && date2 != null && Comparators.compare(this.get(), date2) <= 0,
                new StringBuilder("date '").append(this.getParam()).append("' is not before or equal to date '")
                        .append(AssertObject.getParam(this.getParamIndex() + 1)).append("'"),
                date2);
    }
}
