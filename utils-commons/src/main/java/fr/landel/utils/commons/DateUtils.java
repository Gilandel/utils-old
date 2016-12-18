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
package fr.landel.utils.commons;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Calendar;
import java.util.Date;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility class to manage dates.
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public final class DateUtils extends org.apache.commons.lang3.time.DateUtils {

    /**
     * Number of seconds in a standard minute.
     */
    public static final long SECONDS_PER_MINUTE = 60L;
    /**
     * Number of minutes in a standard hour.
     */
    public static final long MINUTES_PER_HOUR = 60L;
    /**
     * Number of hours in a standard day.
     */
    public static final long HOURS_PER_DAY = 24L;
    /**
     * Number of days in a standard week.
     */
    public static final long DAYS_PER_WEEK = 7L;
    /**
     * Number of days in a standard year.
     */
    public static final long DAYS_PER_YEAR = 365L;
    /**
     * Number of days in a leap year.
     */
    public static final long DAYS_PER_YEAR_LEAP = 366L;
    /**
     * Number of milliseconds in a standard week.
     */
    public static final long MILLIS_PER_WEEK = MILLIS_PER_DAY * DAYS_PER_WEEK;
    /**
     * Number of milliseconds in a standard year.
     */
    public static final long MILLIS_PER_YEAR = MILLIS_PER_DAY * DAYS_PER_YEAR;
    /**
     * Number of milliseconds in a leap year.
     */
    public static final long MILLIS_PER_YEAR_LEAP = MILLIS_PER_DAY * DAYS_PER_YEAR_LEAP;
    /**
     * Number of seconds in a standard hour.
     */
    public static final long SECONDS_PER_HOUR = SECONDS_PER_MINUTE * MINUTES_PER_HOUR;
    /**
     * Number of seconds in a standard day.
     */
    public static final long SECONDS_PER_DAY = SECONDS_PER_HOUR * HOURS_PER_DAY;
    /**
     * Number of seconds in a standard week.
     */
    public static final long SECONDS_PER_WEEK = SECONDS_PER_DAY * DAYS_PER_WEEK;
    /**
     * Number of seconds in a standard year.
     */
    public static final long SECONDS_PER_YEAR = SECONDS_PER_DAY * DAYS_PER_YEAR;
    /**
     * Number of seconds in a leap year.
     */
    public static final long SECONDS_PER_YEAR_LEAP = SECONDS_PER_DAY * DAYS_PER_YEAR_LEAP;
    /**
     * Number of minutes in a standard day.
     */
    public static final long MINUTES_PER_DAY = MINUTES_PER_HOUR * HOURS_PER_DAY;
    /**
     * Number of minutes in a standard week.
     */
    public static final long MINUTES_PER_WEEK = MINUTES_PER_DAY * DAYS_PER_WEEK;
    /**
     * Number of minutes in a standard year.
     */
    public static final long MINUTES_PER_YEAR = MINUTES_PER_DAY * DAYS_PER_YEAR;
    /**
     * Number of minutes in a leap year.
     */
    public static final long MINUTES_PER_YEAR_LEAP = MINUTES_PER_DAY * DAYS_PER_YEAR_LEAP;
    /**
     * Number of hours in a standard week.
     */
    public static final long HOURS_PER_WEEK = HOURS_PER_DAY * DAYS_PER_WEEK;
    /**
     * Number of hours in a standard year.
     */
    public static final long HOURS_PER_YEAR = HOURS_PER_DAY * DAYS_PER_YEAR;
    /**
     * Number of hours in a leap year.
     */
    public static final long HOURS_PER_YEAR_LEAP = HOURS_PER_DAY * DAYS_PER_YEAR_LEAP;

    private static final Logger LOGGER = LoggerFactory.getLogger(DateUtils.class);

    /**
     * Hidden constructor.
     */
    private DateUtils() {
        super();
    }

    /**
     * Date Wrapper.
     * 
     * @param date
     *            The date to wrap
     * @return The wrapped date
     */
    public static Date getDate(final Date date) {
        if (date == null) {
            return null;
        }

        return new Date(date.getTime());
    }

    /**
     * Get the date if not null otherwise defaultDate.
     * 
     * @param date
     *            The input date
     * @param defaultDate
     *            The default date
     * @return a date
     */
    public static Date getDefaultIfNull(final Date date, final Date defaultDate) {
        if (date != null) {
            return date;
        }

        return defaultDate;
    }

    /**
     * Get the date if not null, not empty and parseable otherwise defaultDate.
     * 
     * @param date
     *            The input date
     * @param df
     *            The date formatter
     * @param defaultDate
     *            The default date
     * @return a date
     */
    public static Date getDefaultIfEmpty(final String date, final DateFormat df, final Date defaultDate) {
        if (StringUtils.isNotEmpty(date) && df != null) {
            try {
                return df.parse(date);
            } catch (final ParseException e) {
                LOGGER.error("Error occurred in getDefaultIfEmpty", e);
            }
        }
        return defaultDate;
    }

    /**
     * Get the date if not null, not empty and parseable otherwise null.
     * 
     * @param date
     *            The input date
     * @param df
     *            The date formatter
     * @return a date or null
     */
    public static Date getNullIfEmpty(final String date, final DateFormat df) {
        return getDefaultIfEmpty(date, df, null);
    }

    /**
     * Get a new calendar instance from the date
     * 
     * @param date
     *            The input date
     * @return The calendar or null (if date is null)
     */
    public static Calendar getCalendar(final Date date) {
        Calendar calendar = null;
        if (date != null) {
            calendar = Calendar.getInstance();
            calendar.setTime(date);
        }
        return calendar;
    }

    /**
     * Compare two dates and returns the duration between them following the
     * time scale.
     * 
     * <p>
     * precondition: {@code date1} cannot be {@code null}, {@code date2} cannot
     * be {@code null} and {@code scale} cannot be {@code null}
     * </p>
     * 
     * 
     * @param date1
     *            first date
     * @param date2
     *            second date
     * @param scale
     *            the time scale
     * @return the duration (can be negative)
     */
    public static long between(final Date date1, final Date date2, final TimeUnit scale) {
        Objects.requireNonNull(date1, "The parameter date1 cannot be null");
        Objects.requireNonNull(date2, "The parameter date2 cannot be null");
        Objects.requireNonNull(scale, "The parameter scale cannot be null");

        return scale.convert(date2.getTime() - date1.getTime(), TimeUnit.MILLISECONDS);
    }

    /**
     * Compare two dates and returns the duration between them following the
     * time scale.
     * 
     * <p>
     * precondition: {@code date1} cannot be {@code null}, {@code date2} cannot
     * be {@code null} and {@code scale} cannot be {@code null}
     * </p>
     * 
     * 
     * @param date1
     *            first date
     * @param date2
     *            second date
     * @param scale
     *            the time scale
     * @return the duration (can be negative)
     */
    public static long between(final Calendar date1, final Calendar date2, final TimeUnit scale) {
        Objects.requireNonNull(date1, "The parameter date1 cannot be null");
        Objects.requireNonNull(date2, "The parameter date2 cannot be null");

        return between(date1.getTime(), date2.getTime(), scale);
    }
}
