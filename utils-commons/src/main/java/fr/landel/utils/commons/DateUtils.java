/*
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
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
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility class to manage dates.
 *
 * @since Nov 27, 2015
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

    private static final List<Integer> CALENDAR_FIELDS;
    static {
        List<Integer> list = new ArrayList<>();
        list.add(Calendar.ERA);
        list.add(Calendar.YEAR);
        list.add(Calendar.MONTH);
        list.add(Calendar.WEEK_OF_YEAR);
        list.add(Calendar.WEEK_OF_MONTH);
        list.add(Calendar.DATE);
        list.add(Calendar.DAY_OF_MONTH);
        list.add(Calendar.DAY_OF_YEAR);
        list.add(Calendar.DAY_OF_WEEK);
        list.add(Calendar.DAY_OF_WEEK_IN_MONTH);
        list.add(Calendar.AM_PM);
        list.add(Calendar.HOUR);
        list.add(Calendar.HOUR_OF_DAY);
        list.add(Calendar.MINUTE);
        list.add(Calendar.SECOND);
        list.add(Calendar.MILLISECOND);
        CALENDAR_FIELDS = Collections.unmodifiableList(list);
    }

    private static final Logger LOGGER = LoggerFactory.getLogger(DateUtils.class);

    /**
     * Hidden constructor.
     */
    private DateUtils() {
        super();
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
    public static Date defaultIfEmpty(final String date, final DateFormat df, final Date defaultDate) {
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
    public static Date nullIfEmpty(final String date, final DateFormat df) {
        return defaultIfEmpty(date, df, null);
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
     * precondition: {@code calendar1} cannot be {@code null}, {@code calendar2}
     * cannot be {@code null} and {@code scale} cannot be {@code null}
     * </p>
     * 
     * @param calendar1
     *            first date
     * @param calendar2
     *            second date
     * @param scale
     *            the time scale
     * @return the duration (can be negative)
     */
    public static long between(final Calendar calendar1, final Calendar calendar2, final TimeUnit scale) {
        Objects.requireNonNull(calendar1, "The parameter calendar1 cannot be null");
        Objects.requireNonNull(calendar2, "The parameter calendar2 cannot be null");

        return between(calendar1.getTime(), calendar2.getTime(), scale);
    }

    /**
     * Clear all smaller fields. If 'MONTH' is specified, all smaller are
     * cleared like 'DATE', 'HOUR'... and 'MONTH' is NOT cleared.
     * 
     * <p>
     * precondition: {@code calendar} cannot be {@code null} and
     * {@code calendarField} must be valid
     * </p>
     * 
     * Valid calendar field:
     * <ul>
     * <li>{@link Calendar#ERA}</li>
     * <li>{@link Calendar#YEAR}</li>
     * <li>{@link Calendar#MONTH}</li>
     * <li>{@link Calendar#WEEK_OF_YEAR}</li>
     * <li>{@link Calendar#WEEK_OF_MONTH}</li>
     * <li>{@link Calendar#DATE}</li>
     * <li>{@link Calendar#DAY_OF_MONTH}</li>
     * <li>{@link Calendar#DAY_OF_YEAR}</li>
     * <li>{@link Calendar#DAY_OF_WEEK}</li>
     * <li>{@link Calendar#DAY_OF_WEEK_IN_MONTH}</li>
     * <li>{@link Calendar#AM_PM}</li>
     * <li>{@link Calendar#HOUR}</li>
     * <li>{@link Calendar#HOUR_OF_DAY}</li>
     * <li>{@link Calendar#MINUTE}</li>
     * <li>{@link Calendar#SECOND}</li>
     * <li>{@link Calendar#MILLISECOND}</li>
     * </ul>
     * 
     * @param calendar
     *            the date to clear
     * @param calendarField
     *            the calendar field bound
     */
    public static void clearSmaller(final Calendar calendar, final int calendarField) {
        Objects.requireNonNull(calendar, "The parameter calendar cannot be null");
        if (!CALENDAR_FIELDS.contains(calendarField)) {
            throw new IllegalArgumentException(StringUtils.inject("The parameter calendarField '{}' is invalid", calendarField));
        }

        for (int field : CALENDAR_FIELDS) {
            if (calendarField < field) {
                calendar.clear(field);
            }
        }
    }

    /**
     * Clear all bigger fields. If 'MONTH' is specified, all bigger are cleared
     * like 'DATE', 'HOUR'... and 'MONTH' is NOT cleared.
     * 
     * <p>
     * precondition: {@code calendar} cannot be {@code null} and
     * {@code calendarField} must be valid
     * </p>
     * 
     * Valid calendar field:
     * <ul>
     * <li>{@link Calendar#ERA}</li>
     * <li>{@link Calendar#YEAR}</li>
     * <li>{@link Calendar#MONTH}</li>
     * <li>{@link Calendar#WEEK_OF_YEAR}</li>
     * <li>{@link Calendar#WEEK_OF_MONTH}</li>
     * <li>{@link Calendar#DATE}</li>
     * <li>{@link Calendar#DAY_OF_MONTH}</li>
     * <li>{@link Calendar#DAY_OF_YEAR}</li>
     * <li>{@link Calendar#DAY_OF_WEEK}</li>
     * <li>{@link Calendar#DAY_OF_WEEK_IN_MONTH}</li>
     * <li>{@link Calendar#AM_PM}</li>
     * <li>{@link Calendar#HOUR}</li>
     * <li>{@link Calendar#HOUR_OF_DAY}</li>
     * <li>{@link Calendar#MINUTE}</li>
     * <li>{@link Calendar#SECOND}</li>
     * <li>{@link Calendar#MILLISECOND}</li>
     * </ul>
     * 
     * @param calendar
     *            the date to clear
     * @param calendarField
     *            the calendar field bound
     */
    public static void clearBigger(final Calendar calendar, final int calendarField) {
        Objects.requireNonNull(calendar, "The parameter calendar cannot be null");
        if (!CALENDAR_FIELDS.contains(calendarField)) {
            throw new IllegalArgumentException(StringUtils.inject("The parameter calendarField '{}' is invalid", calendarField));
        }

        for (int field : CALENDAR_FIELDS) {
            if (calendarField > field) {
                calendar.clear(field);
            }
        }
    }

    /**
     * Date Wrapper (security reason).
     * 
     * @param date
     *            The date to wrap
     * @return The wrapped date
     */
    public static Date cloneDate(final Date date) {
        if (date == null) {
            return null;
        }
        return new Date(date.getTime());
    }

    /**
     * Get the date from a local date/time. Use the default time-zone
     * {@link ZoneId#systemDefault()}.
     * 
     * @param date
     *            the input date
     * @return the mapped date
     */
    public static Date getDate(final LocalDateTime date) {
        return getDate(date, null);
    }

    /**
     * Get the date from a local date/time.
     * 
     * @param date
     *            the input date
     * @param zoneId
     *            the time-zone identifier (if {@code null}, use
     *            {@link ZoneId#systemDefault()})
     * @return the mapped date
     */
    public static Date getDate(final LocalDateTime date, final ZoneId zoneId) {
        return Date.from(date.atZone(ObjectUtils.defaultIfNull(zoneId, () -> ZoneId.systemDefault())).toInstant());
    }

    /**
     * Get the date from a local date and time. Use the default time-zone
     * {@link ZoneId#systemDefault()}.
     * 
     * @param date
     *            the input date
     * @param time
     *            the input time
     * @return the mapped date
     */
    public static Date getDate(final LocalDate date, final LocalTime time) {
        return getDate(date, time, null);
    }

    /**
     * Get the date from a local date and time.
     * 
     * @param date
     *            the input date
     * @param time
     *            the input time
     * @param zoneId
     *            the time-zone identifier (if {@code null}, use
     *            {@link ZoneId#systemDefault()})
     * @return the mapped date
     */
    public static Date getDate(final LocalDate date, final LocalTime time, final ZoneId zoneId) {
        return Date.from(time.atDate(date).atZone(ObjectUtils.defaultIfNull(zoneId, () -> ZoneId.systemDefault())).toInstant());
    }

    /**
     * Get the date from a local date. The time is defined to midnight at the
     * start of the day (00:00). Use the default time-zone
     * {@link ZoneId#systemDefault()}.
     * 
     * @param date
     *            the input date
     * @return the mapped date
     */
    public static Date getDate(final LocalDate date) {
        return getDate(date, (ZoneId) null);
    }

    /**
     * Get the date from a local date. The time is defined to midnight at the
     * start of the day (00:00).
     * 
     * @param date
     *            the input date
     * @param zoneId
     *            the time-zone identifier (if {@code null}, use
     *            {@link ZoneId#systemDefault()})
     * @return the mapped date
     */
    public static Date getDate(final LocalDate date, final ZoneId zoneId) {
        return getDate(date.atTime(LocalTime.MIN), zoneId);
    }

    /**
     * Get the date from a local time. The date is defined to epoch date
     * 1970-01-01 ({@link ChronoField#EPOCH_DAY}). Use the default time-zone
     * {@link ZoneId#systemDefault()}.
     * 
     * @param time
     *            the input time
     * @return the mapped date
     */
    public static Date getDate(final LocalTime time) {
        return getDate(time, null);
    }

    /**
     * Get the date from a local time. The date is defined to epoch date
     * 1970-01-01 ({@link ChronoField#EPOCH_DAY}).
     * 
     * @param time
     *            the input time
     * @param zoneId
     *            the time-zone identifier (if {@code null}, use
     *            {@link ZoneId#systemDefault()})
     * @return the mapped date
     */
    public static Date getDate(final LocalTime time, final ZoneId zoneId) {
        return getDate(time.atDate(LocalDate.ofEpochDay(0)), zoneId);
    }

    /**
     * Get the date from a date/time with an offset.
     * 
     * @param date
     *            the input date
     * @return the mapped date
     */
    public static Date getDate(final OffsetDateTime date) {
        return Date.from(date.toInstant());
    }

    /**
     * Get the date from a date/time with an offset. The date is defined to
     * epoch date 1970-01-01 ({@link ChronoField#EPOCH_DAY}).
     * 
     * @param time
     *            the input time
     * @return the mapped date
     */
    public static Date getDate(final OffsetTime time) {
        return getDate(null, time);
    }

    /**
     * Get the date from a date/time with an offset.
     * 
     * @param date
     *            the input date (if {@code null}, use the epoch date 1970-01-01
     *            ({@link ChronoField#EPOCH_DAY}))
     * @param time
     *            the input time
     * @return the mapped date
     */
    public static Date getDate(final LocalDate date, final OffsetTime time) {
        return Date.from(time.atDate(ObjectUtils.defaultIfNull(date, LocalDate.ofEpochDay(0))).toInstant());
    }

    /**
     * Get the date from a zoned date/time.
     * 
     * @param date
     *            the input date
     * @return the mapped date
     */
    public static Date getDate(final ZonedDateTime date) {
        return Date.from(date.toInstant());
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
     * Get the calendar from a local date/time. Use the default time-zone
     * {@link ZoneId#systemDefault()}.
     * 
     * @param date
     *            the input date
     * @return the mapped calendar
     */
    public static Calendar getCalendar(final LocalDateTime date) {
        return getCalendar(date, null);
    }

    /**
     * Get the calendar from a local date/time.
     * 
     * @param date
     *            the input date
     * @param zoneId
     *            the time-zone identifier (if {@code null}, use
     *            {@link ZoneId#systemDefault()})
     * @return the mapped calendar
     */
    public static Calendar getCalendar(final LocalDateTime date, final ZoneId zoneId) {
        return getCalendar(getDate(date, zoneId));
    }

    /**
     * Get the calendar from a local date and time. Use the default time-zone
     * {@link ZoneId#systemDefault()}.
     * 
     * @param date
     *            the input date
     * @param time
     *            the input time
     * @return the mapped calendar
     */
    public static Calendar getCalendar(final LocalDate date, final LocalTime time) {
        return getCalendar(date, time, null);
    }

    /**
     * Get the calendar from a local date and time.
     * 
     * @param date
     *            the input date
     * @param time
     *            the input time
     * @param zoneId
     *            the time-zone identifier (if {@code null}, use
     *            {@link ZoneId#systemDefault()})
     * @return the mapped calendar
     */
    public static Calendar getCalendar(final LocalDate date, final LocalTime time, final ZoneId zoneId) {
        return getCalendar(getDate(date, time, zoneId));
    }

    /**
     * Get the calendar from a local date. The time is defined to midnight at
     * the start of the day (00:00). Use the default time-zone
     * {@link ZoneId#systemDefault()}.
     * 
     * @param date
     *            the input date
     * @return the mapped calendar
     */
    public static Calendar getCalendar(final LocalDate date) {
        return getCalendar(date, (ZoneId) null);
    }

    /**
     * Get the calendar from a local date. The time is defined to midnight at
     * the start of the day (00:00).
     * 
     * @param date
     *            the input date
     * @param zoneId
     *            the time-zone identifier (if {@code null}, use
     *            {@link ZoneId#systemDefault()})
     * @return the mapped calendar
     */
    public static Calendar getCalendar(final LocalDate date, final ZoneId zoneId) {
        return getCalendar(date.atTime(LocalTime.MIN), zoneId);
    }

    /**
     * Get the calendar from a local time. The date is defined to epoch date
     * 1970-01-01 ({@link ChronoField#EPOCH_DAY}). Use the default time-zone
     * {@link ZoneId#systemDefault()}.
     * 
     * @param time
     *            the input time
     * @return the mapped calendar
     */
    public static Calendar getCalendar(final LocalTime time) {
        return getCalendar(time, null);
    }

    /**
     * Get the calendar from a local time. The date is defined to epoch date
     * 1970-01-01 ({@link ChronoField#EPOCH_DAY}).
     * 
     * @param time
     *            the input time
     * @param zoneId
     *            the time-zone identifier (if {@code null}, use
     *            {@link ZoneId#systemDefault()})
     * @return the mapped calendar
     */
    public static Calendar getCalendar(final LocalTime time, final ZoneId zoneId) {
        return getCalendar(getDate(time, zoneId));
    }

    /**
     * Get the calendar from a date/time with an offset.
     * 
     * @param date
     *            the input date
     * @return the mapped calendar
     */
    public static Calendar getCalendar(final OffsetDateTime date) {
        return getCalendar(getDate(date));
    }

    /**
     * Get the calendar from a date/time with an offset. The date is defined to
     * epoch date 1970-01-01 ({@link ChronoField#EPOCH_DAY}).
     * 
     * @param time
     *            the input time
     * @return the mapped calendar
     */
    public static Calendar getCalendar(final OffsetTime time) {
        return getCalendar(null, time);
    }

    /**
     * Get the calendar from a date/time with an offset.
     * 
     * @param date
     *            the input date (if {@code null}, use the epoch date 1970-01-01
     *            ({@link ChronoField#EPOCH_DAY}))
     * @param time
     *            the input time
     * @return the mapped calendar
     */
    public static Calendar getCalendar(final LocalDate date, final OffsetTime time) {
        return getCalendar(getDate(date, time));
    }

    /**
     * Get the calendar from a zoned date/time.
     * 
     * @param date
     *            the input date
     * @return the mapped calendar
     */
    public static Calendar getCalendar(final ZonedDateTime date) {
        return getCalendar(getDate(date));
    }

    /**
     * Get the local date/time from a date. Use the default time-zone
     * {@link ZoneId#systemDefault()}.
     * 
     * @param date
     *            the input date
     * @return the mapped date
     */
    public static LocalDateTime getLocalDateTime(final Date date) {
        return getLocalDateTime(getCalendar(date));
    }

    /**
     * Get the local date/time from a date
     * 
     * @param date
     *            the input date
     * @param zoneId
     *            the time-zone identifier (if {@code null}, use
     *            {@link ZoneId#systemDefault()})
     * @return the mapped date
     */
    public static LocalDateTime getLocalDateTime(final Date date, final ZoneId zoneId) {
        return LocalDateTime.ofInstant(date.toInstant(), ObjectUtils.defaultIfNull(zoneId, () -> ZoneId.systemDefault()));
    }

    /**
     * Get the local date/time from a calendar
     * 
     * @param calendar
     *            the input calendar
     * @return the mapped date
     */
    public static LocalDateTime getLocalDateTime(final Calendar calendar) {
        return LocalDateTime.ofInstant(calendar.toInstant(), calendar.getTimeZone().toZoneId());
    }

    /**
     * Get the local date from a date. Use the default time-zone
     * {@link ZoneId#systemDefault()}.
     * 
     * @param date
     *            the input date
     * @return the mapped date
     */
    public static LocalDate getLocalDate(final Date date) {
        return getLocalDate(getCalendar(date));
    }

    /**
     * Get the local date from a date.
     * 
     * @param date
     *            the input date
     * @param zoneId
     *            the time-zone identifier (if {@code null}, use
     *            {@link ZoneId#systemDefault()})
     * @return the mapped date
     */
    public static LocalDate getLocalDate(final Date date, final ZoneId zoneId) {
        return getLocalDateTime(date, zoneId).toLocalDate();
    }

    /**
     * Get the local date from a calendar.
     * 
     * @param calendar
     *            the input calendar
     * @return the mapped date
     */
    public static LocalDate getLocalDate(final Calendar calendar) {
        return getLocalDateTime(calendar).toLocalDate();
    }

    /**
     * Get the local time from a date. Use the default time-zone
     * {@link ZoneId#systemDefault()}.
     * 
     * @param date
     *            the input date
     * @return the mapped date
     */
    public static LocalTime getLocalTime(final Date date) {
        return getLocalTime(getCalendar(date));
    }

    /**
     * Get the local time from a date.
     * 
     * @param date
     *            the input date
     * @param zoneId
     *            the time-zone identifier (if {@code null}, use
     *            {@link ZoneId#systemDefault()})
     * @return the mapped date
     */
    public static LocalTime getLocalTime(final Date date, final ZoneId zoneId) {
        return getLocalDateTime(date, zoneId).toLocalTime();
    }

    /**
     * Get the local time from a calendar.
     * 
     * @param calendar
     *            the input calendar
     * @return the mapped date
     */
    public static LocalTime getLocalTime(final Calendar calendar) {
        return getLocalDateTime(calendar).toLocalTime();
    }

    /**
     * Get the zoned date/time from a date. Use the default time-zone
     * {@link ZoneId#systemDefault()}.
     * 
     * @param date
     *            the input date
     * @return the mapped date
     */
    public static ZonedDateTime getZonedDateTime(final Date date) {
        return getZonedDateTime(getCalendar(date));
    }

    /**
     * Get the zoned date/time from a date.
     * 
     * @param date
     *            the input date
     * @param zoneId
     *            the time-zone identifier (if {@code null}, use
     *            {@link ZoneId#systemDefault()})
     * @return the mapped date
     */
    public static ZonedDateTime getZonedDateTime(final Date date, final ZoneId zoneId) {
        return ZonedDateTime.ofInstant(date.toInstant(), ObjectUtils.defaultIfNull(zoneId, () -> ZoneId.systemDefault()));
    }

    /**
     * Get the zoned date/time from a calendar.
     * 
     * @param calendar
     *            the input calendar
     * @return the mapped date
     */
    public static ZonedDateTime getZonedDateTime(final Calendar calendar) {
        return ZonedDateTime.ofInstant(calendar.toInstant(), calendar.getTimeZone().toZoneId());
    }

    /**
     * Get the date/time with an offset from a date. Use the default time-zone
     * {@link ZoneId#systemDefault()}.
     * 
     * @param date
     *            the input date
     * @return the mapped date
     */
    public static OffsetDateTime getOffsetDateTime(final Date date) {
        return getOffsetDateTime(getCalendar(date));
    }

    /**
     * Get the date/time with an offset from a date.
     * 
     * @param date
     *            the input date
     * @param zoneId
     *            the time-zone identifier (if {@code null}, use
     *            {@link ZoneId#systemDefault()})
     * @return the mapped date
     */
    public static OffsetDateTime getOffsetDateTime(final Date date, final ZoneId zoneId) {
        return OffsetDateTime.ofInstant(date.toInstant(), ObjectUtils.defaultIfNull(zoneId, () -> ZoneId.systemDefault()));
    }

    /**
     * Get the date/time with an offset from a calendar.
     * 
     * @param calendar
     *            the input calendar
     * @return the mapped date
     */
    public static OffsetDateTime getOffsetDateTime(final Calendar calendar) {
        return OffsetDateTime.ofInstant(calendar.toInstant(), calendar.getTimeZone().toZoneId());
    }

    /**
     * Get the time with an offset from a date. Use the default time-zone
     * {@link ZoneId#systemDefault()}.
     * 
     * @param date
     *            the input date
     * @return the mapped date
     */
    public static OffsetTime getOffsetTime(final Date date) {
        return getOffsetTime(getCalendar(date));
    }

    /**
     * Get the time with an offset from a date.
     * 
     * @param date
     *            the input date
     * @param zoneId
     *            the time-zone identifier (if {@code null}, use
     *            {@link ZoneId#systemDefault()})
     * @return the mapped date
     */
    public static OffsetTime getOffsetTime(final Date date, final ZoneId zoneId) {
        return OffsetTime.ofInstant(date.toInstant(), ObjectUtils.defaultIfNull(zoneId, () -> ZoneId.systemDefault()));
    }

    /**
     * Get the time with an offset from a calendar.
     * 
     * @param calendar
     *            the input calendar
     * @return the mapped date
     */
    public static OffsetTime getOffsetTime(final Calendar calendar) {
        return OffsetTime.ofInstant(calendar.toInstant(), calendar.getTimeZone().toZoneId());
    }
}
