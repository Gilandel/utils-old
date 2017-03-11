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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;
import java.util.concurrent.TimeUnit;

import org.junit.Test;

/**
 * Check utility class (dates). {@link DateUtils}
 *
 * @since Nov 27, 2015
 * @author Gilles Landel
 *
 */
public class DateUtilsTest {

    private static final int YEAR = 2014;
    private static final int MONTH = 7;
    private static final int DAY = 15;

    /**
     * Check get date wrapper
     */
    @Test
    public void testGetDate() {
        Calendar calendar = Calendar.getInstance();

        assertNull(DateUtils.cloneDate((Date) null));

        calendar.set(YEAR, MONTH, DAY);
        Date date1 = calendar.getTime();

        assertNotNull(date1);

        Date date2 = DateUtils.cloneDate(date1);

        assertEquals(date1, date2);

        assertFalse(System.identityHashCode(date1) == System.identityHashCode(date2));
    }

    /**
     * Check get default date if null
     */
    @Test
    public void testGetDefaultIfNull() {
        Calendar calendar = Calendar.getInstance();

        calendar.set(YEAR, MONTH, DAY);
        Date date1 = calendar.getTime();

        calendar.set(YEAR, MONTH + 1, DAY);
        Date dateDefault = calendar.getTime();

        assertNotNull(date1);

        Date date2 = ObjectUtils.defaultIfNull(date1, dateDefault);
        assertEquals(date1, date2);

        date2 = ObjectUtils.defaultIfNull(null, dateDefault);
        assertEquals(dateDefault, date2);

        date2 = ObjectUtils.defaultIfNull(null, (Date) null);
        assertNull(date2);
    }

    /**
     * Check get default date if empty
     */
    @Test
    public void testGetDefaultIfEmpty() {
        Calendar calendar = Calendar.getInstance();
        DateFormat df = new SimpleDateFormat("dd/MM/yyyy");

        calendar.set(YEAR, MONTH, DAY, 0, 0, 0);
        Date date1 = calendar.getTime();

        calendar.set(YEAR, MONTH + 1, DAY);
        Date dateDefault = calendar.getTime();

        assertNotNull(date1);

        Date date2 = DateUtils.defaultIfEmpty(DAY + "/" + (MONTH + 1) + "/" + YEAR, df, dateDefault);
        assertEquals(date1.toString(), date2.toString());

        date2 = DateUtils.defaultIfEmpty("", df, dateDefault);
        assertEquals(dateDefault.toString(), date2.toString());

        date2 = DateUtils.defaultIfEmpty(null, df, dateDefault);
        assertEquals(dateDefault.toString(), date2.toString());

        date2 = DateUtils.defaultIfEmpty(DAY + "/" + (MONTH + 1) + "/" + YEAR, null, dateDefault);
        assertEquals(dateDefault.toString(), date2.toString());

        date2 = DateUtils.defaultIfEmpty("UNPARSEABLE", df, dateDefault);
        assertEquals(dateDefault.toString(), date2.toString());
    }

    /**
     * Check get null date if empty
     */
    @Test
    public void testGetNullIfEmpty() {
        DateFormat df = new SimpleDateFormat("dd/MM/yyyy");

        Date date2 = DateUtils.nullIfEmpty(DAY + "/" + (MONTH + 1) + "/" + YEAR, df);
        assertEquals(date2.toString(), date2.toString());

        date2 = DateUtils.nullIfEmpty(null, df);
        assertNull(date2);

        date2 = DateUtils.nullIfEmpty("", df);
        assertNull(date2);

        date2 = DateUtils.nullIfEmpty("UNPARSEABLE", df);
        assertNull(date2);
    }

    /**
     * Check get calendar
     */
    @Test
    public void testGetCalendar() {
        final Calendar calendar = Calendar.getInstance();
        final Date date = calendar.getTime();

        assertEquals(calendar, DateUtils.getCalendar(date));
        assertNull(DateUtils.getCalendar((Date) null));
    }

    /**
     * Check constant
     */
    @Test
    public void testConstant() {
        assertEquals(1000L * 60 * 60 * 24 * 7, DateUtils.MILLIS_PER_WEEK);
        assertEquals(1000L * 60 * 60 * 24 * 365, DateUtils.MILLIS_PER_YEAR);
        assertEquals(1000L * 60 * 60 * 24 * 366, DateUtils.MILLIS_PER_YEAR_LEAP);

        assertEquals(60, DateUtils.SECONDS_PER_MINUTE);
        assertEquals(60 * 60, DateUtils.SECONDS_PER_HOUR);
        assertEquals(60 * 60 * 24, DateUtils.SECONDS_PER_DAY);
        assertEquals(60 * 60 * 24 * 7, DateUtils.SECONDS_PER_WEEK);
        assertEquals(60 * 60 * 24 * 365, DateUtils.SECONDS_PER_YEAR);
        assertEquals(60 * 60 * 24 * 366, DateUtils.SECONDS_PER_YEAR_LEAP);

        assertEquals(60, DateUtils.MINUTES_PER_HOUR);
        assertEquals(60 * 24, DateUtils.MINUTES_PER_DAY);
        assertEquals(60 * 24 * 7, DateUtils.MINUTES_PER_WEEK);
        assertEquals(60 * 24 * 365, DateUtils.MINUTES_PER_YEAR);
        assertEquals(60 * 24 * 366, DateUtils.MINUTES_PER_YEAR_LEAP);

        assertEquals(24, DateUtils.HOURS_PER_DAY);
        assertEquals(24 * 7, DateUtils.HOURS_PER_WEEK);
        assertEquals(24 * 365, DateUtils.HOURS_PER_YEAR);
        assertEquals(24 * 366, DateUtils.HOURS_PER_YEAR_LEAP);

        assertEquals(7, DateUtils.DAYS_PER_WEEK);
        assertEquals(365, DateUtils.DAYS_PER_YEAR);
        assertEquals(366, DateUtils.DAYS_PER_YEAR_LEAP);
    }

    /**
     * Check between
     */
    @Test
    public void testBetween() {
        final int diffDays = 20;
        final int hour1 = 11;
        final int hour2 = 23;
        final int diffHours = hour2 - hour1;
        final Calendar calendar1 = new GregorianCalendar(YEAR, MONTH, DAY, hour1, 0, 0);
        final Calendar calendar2 = new GregorianCalendar(YEAR, MONTH, DAY + diffDays, hour2, 0, 0);
        final Date date1 = calendar1.getTime();
        final Date date2 = calendar2.getTime();

        assertEquals(diffDays, DateUtils.between(date1, date2, TimeUnit.DAYS));
        assertEquals(0, DateUtils.between(date1, date1, TimeUnit.DAYS));
        assertEquals(-diffDays, DateUtils.between(date2, date1, TimeUnit.DAYS));

        assertEquals(diffDays * DateUtils.HOURS_PER_DAY + diffHours, DateUtils.between(date1, date2, TimeUnit.HOURS));
        assertEquals(0, DateUtils.between(date1, date1, TimeUnit.HOURS));
        assertEquals(-diffDays * DateUtils.HOURS_PER_DAY - diffHours, DateUtils.between(date2, date1, TimeUnit.HOURS));

        assertEquals(diffDays, DateUtils.between(calendar1, calendar2, TimeUnit.DAYS));
        assertEquals(0, DateUtils.between(calendar1, calendar1, TimeUnit.DAYS));
        assertEquals(-diffDays, DateUtils.between(calendar2, calendar1, TimeUnit.DAYS));
    }

    /**
     * Check between
     */
    @Test(expected = NullPointerException.class)
    public void testBetweenDate1() {
        DateUtils.between(null, new Date(), TimeUnit.DAYS);
    }

    /**
     * Check between
     */
    @Test(expected = NullPointerException.class)
    public void testBetweenDate2() {
        DateUtils.between(new Date(), null, TimeUnit.DAYS);
    }

    /**
     * Check between
     */
    @Test(expected = NullPointerException.class)
    public void testBetweenScale() {
        DateUtils.between(new Date(), new Date(), null);
    }

    /**
     * Check between
     */
    @Test(expected = NullPointerException.class)
    public void testBetweenCalendar1() {
        DateUtils.between(null, Calendar.getInstance(), TimeUnit.DAYS);
    }

    /**
     * Check between
     */
    @Test(expected = NullPointerException.class)
    public void testBetweenCalendar2() {
        DateUtils.between(Calendar.getInstance(), null, TimeUnit.DAYS);
    }

    /**
     * Check {@link DateUtils#clearSmaller(Calendar, int)}
     */
    @Test
    public void testClearSmaller() {
        Calendar calendar = new GregorianCalendar(2017, 2, 28, 6, 3, 0);

        DateUtils.clearSmaller(calendar, Calendar.YEAR);
        assertEquals(2017, calendar.get(Calendar.YEAR));
        assertEquals(0, calendar.get(Calendar.MONTH));
        assertEquals(0, calendar.get(Calendar.MINUTE));

        calendar = new GregorianCalendar(2017, 2, 28, 13, 3, 0);
        assertEquals(1, calendar.get(Calendar.AM_PM));
        DateUtils.clearSmaller(calendar, Calendar.AM_PM);
        assertEquals(2017, calendar.get(Calendar.YEAR));
        assertEquals(1, calendar.get(Calendar.AM_PM));
        assertEquals(0, calendar.get(Calendar.HOUR));

        calendar = new GregorianCalendar(2017, 2, 28, 13, 3, 0);
        DateUtils.clearSmaller(calendar, Calendar.DATE);
        assertEquals(2017, calendar.get(Calendar.YEAR));
        assertEquals(87, calendar.get(Calendar.DAY_OF_YEAR));
        assertEquals(28, calendar.get(Calendar.DAY_OF_MONTH));
        assertEquals(0, calendar.get(Calendar.HOUR));

        try {
            DateUtils.clearSmaller(null, Calendar.YEAR);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        try {
            DateUtils.clearSmaller(calendar, Calendar.DST_OFFSET);
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals("The parameter calendarField '16' is invalid", e.getMessage());
        }
    }

    /**
     * Check {@link DateUtils#clearBigger(Calendar, int)}
     */
    @Test
    public void testClearBigger() {
        final int epochYear = 1970;

        Calendar calendar = new GregorianCalendar(2017, 2, 28, 6, 3, 0);

        DateUtils.clearBigger(calendar, Calendar.YEAR);
        assertEquals(2017, calendar.get(Calendar.YEAR));
        assertEquals(2, calendar.get(Calendar.MONTH));
        assertEquals(3, calendar.get(Calendar.MINUTE));

        calendar = new GregorianCalendar(2017, 2, 28, 13, 3, 0);
        assertEquals(1, calendar.get(Calendar.AM_PM));
        DateUtils.clearBigger(calendar, Calendar.AM_PM);
        assertEquals(epochYear, calendar.get(Calendar.YEAR));
        assertEquals(1, calendar.get(Calendar.AM_PM));
        assertEquals(13, calendar.get(Calendar.HOUR_OF_DAY));

        calendar = new GregorianCalendar(2017, 2, 28, 13, 3, 0);
        DateUtils.clearBigger(calendar, Calendar.DATE);
        assertEquals(epochYear, calendar.get(Calendar.YEAR));
        assertEquals(28, calendar.get(Calendar.DAY_OF_YEAR));
        assertEquals(28, calendar.get(Calendar.DAY_OF_MONTH));
        assertEquals(13, calendar.get(Calendar.HOUR_OF_DAY));

        try {
            DateUtils.clearBigger(null, Calendar.YEAR);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        try {
            DateUtils.clearBigger(calendar, Calendar.DST_OFFSET);
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals("The parameter calendarField '16' is invalid", e.getMessage());
        }
    }

    /**
     * Check {@link DateUtils#getDate}
     */
    @Test
    public void testGetTimeDate() {
        Calendar calendar = Calendar.getInstance();
        calendar.set(2017, 2, 4, 16, 59, 20); // mars
        calendar.set(Calendar.MILLISECOND, 0);

        final ZoneOffset offset = OffsetDateTime.now().getOffset();

        LocalDateTime localDateTime = LocalDateTime.ofInstant(calendar.toInstant(), ZoneId.systemDefault());
        assertEquals(calendar.getTime(), DateUtils.getDate(localDateTime));
        assertEquals(calendar, DateUtils.getCalendar(localDateTime));

        LocalDate localDate = LocalDate.of(2017, 03, 04);
        calendar.set(2017, 02, 04, 0, 0, 0);
        assertEquals(calendar.getTime(), DateUtils.getDate(localDate));
        assertEquals(calendar, DateUtils.getCalendar(localDate));

        LocalTime localTime = LocalTime.of(1, 2, 3, 0);
        calendar.set(1970, 0, 1, 1, 2, 3);
        assertEquals(calendar, DateUtils.getCalendar(localTime));
        assertEquals(calendar.getTime(), DateUtils.getDate(localTime));

        calendar.set(2017, 02, 04, 1, 2, 3);
        assertEquals(calendar, DateUtils.getCalendar(localDate, localTime));
        assertEquals(calendar.getTime(), DateUtils.getDate(localDate, localTime));

        OffsetDateTime offsetDateTime = OffsetDateTime.of(localDateTime, ZoneOffset.UTC);
        calendar.setTimeZone(TimeZone.getTimeZone(ZoneOffset.UTC));
        calendar.set(2017, 2, 4, 16, 59, 20);
        assertEquals(calendar.getTime(), DateUtils.getCalendar(offsetDateTime).getTime());
        assertEquals(calendar.getTime(), DateUtils.getDate(offsetDateTime));

        OffsetTime offsetTime = OffsetTime.of(localTime, ZoneOffset.UTC);
        calendar.set(1970, 0, 1, 1, 2, 3);
        assertEquals(calendar.getTime(), DateUtils.getCalendar(offsetTime).getTime());
        assertEquals(calendar.getTime(), DateUtils.getDate(offsetTime));

        ZonedDateTime zonedDateTime = ZonedDateTime.of(localDateTime, ZoneOffset.UTC);
        calendar.set(2017, 2, 4, 16, 59, 20);
        assertEquals(calendar.getTime(), DateUtils.getCalendar(zonedDateTime).getTime());
        assertEquals(calendar.getTime(), DateUtils.getDate(zonedDateTime));

        localDateTime = LocalDateTime.ofInstant(calendar.toInstant(), ZoneOffset.UTC);
        assertEquals(localDateTime, DateUtils.getLocalDateTime(calendar));
        assertEquals(localDateTime, DateUtils.getLocalDateTime(calendar.getTime(), ZoneOffset.UTC));
        if (ZoneOffset.UTC.equals(offset)) {
            assertEquals(localDateTime, DateUtils.getLocalDateTime(calendar.getTime(), null));
        } else {
            assertNotEquals(localDateTime, DateUtils.getLocalDateTime(calendar.getTime(), null));
        }

        localDateTime = LocalDateTime.ofInstant(calendar.toInstant(), offset);
        assertEquals(localDateTime, DateUtils.getLocalDateTime(calendar.getTime()));

        assertEquals(localDate, DateUtils.getLocalDate(calendar));
        assertEquals(localDate, DateUtils.getLocalDate(calendar.getTime()));
        assertEquals(localDate, DateUtils.getLocalDate(calendar.getTime(), ZoneId.systemDefault()));
        assertEquals(localDate, DateUtils.getLocalDate(calendar.getTime(), null));

        calendar.setTimeZone(TimeZone.getDefault());
        calendar.set(2017, 2, 4, 1, 2, 3);
        assertEquals(localTime, DateUtils.getLocalTime(calendar));
        assertEquals(localTime, DateUtils.getLocalTime(calendar.getTime()));
        assertEquals(localTime, DateUtils.getLocalTime(calendar.getTime(), ZoneId.systemDefault()));
        assertEquals(localTime, DateUtils.getLocalTime(calendar.getTime(), null));

        calendar.setTimeZone(TimeZone.getTimeZone(ZoneOffset.UTC));
        calendar.set(2017, 2, 4, 16, 59, 20);
        offsetDateTime = OffsetDateTime.ofInstant(offsetDateTime.toInstant(), ZoneOffset.UTC);
        assertEquals(offsetDateTime, DateUtils.getOffsetDateTime(calendar));
        assertEquals(offsetDateTime, DateUtils.getOffsetDateTime(calendar.getTime(), ZoneOffset.UTC));
        if (ZoneOffset.UTC.equals(offset)) {
            assertEquals(offsetDateTime, DateUtils.getOffsetDateTime(calendar.getTime(), null));
        } else {
            assertNotEquals(offsetDateTime, DateUtils.getOffsetDateTime(calendar.getTime(), null));
        }

        offsetDateTime = OffsetDateTime.ofInstant(offsetDateTime.toInstant(), offset);
        assertEquals(offsetDateTime, DateUtils.getOffsetDateTime(calendar.getTime()));

        calendar.setTimeZone(TimeZone.getTimeZone(ZoneId.systemDefault()));
        calendar.set(2017, 2, 4, 1, 2, 3);
        localTime = LocalTime.of(1, 2, 3, 0);
        offsetTime = OffsetTime.of(localTime, offset);
        assertEquals(offsetTime, DateUtils.getOffsetTime(calendar));
        assertEquals(offsetTime, DateUtils.getOffsetTime(calendar.getTime()));
        assertEquals(offsetTime, DateUtils.getOffsetTime(calendar.getTime(), ZoneId.systemDefault()));
        assertEquals(offsetTime, DateUtils.getOffsetTime(calendar.getTime(), null));

        localDateTime = LocalDateTime.ofInstant(calendar.toInstant(), ZoneId.systemDefault());
        zonedDateTime = ZonedDateTime.of(localDateTime, ZoneId.systemDefault());
        assertEquals(zonedDateTime, DateUtils.getZonedDateTime(calendar));
        assertEquals(zonedDateTime, DateUtils.getZonedDateTime(calendar.getTime()));
        assertEquals(zonedDateTime, DateUtils.getZonedDateTime(calendar.getTime(), ZoneId.systemDefault()));
        assertEquals(zonedDateTime, DateUtils.getZonedDateTime(calendar.getTime(), null));
    }
}
