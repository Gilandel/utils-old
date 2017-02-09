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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.concurrent.TimeUnit;

import org.junit.Test;

/**
 * Check utility class (dates).
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

        assertNull(DateUtils.getDate((Date) null));

        calendar.set(YEAR, MONTH, DAY);
        Date date1 = calendar.getTime();

        assertNotNull(date1);

        Date date2 = DateUtils.getDate(date1);

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

        Date date2 = DateUtils.getDefaultIfNull(date1, dateDefault);
        assertEquals(date1, date2);

        date2 = DateUtils.getDefaultIfNull(null, dateDefault);
        assertEquals(dateDefault, date2);

        date2 = DateUtils.getDefaultIfNull(null, null);
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

        Date date2 = DateUtils.getDefaultIfEmpty(DAY + "/" + (MONTH + 1) + "/" + YEAR, df, dateDefault);
        assertEquals(date1.toString(), date2.toString());

        date2 = DateUtils.getDefaultIfEmpty("", df, dateDefault);
        assertEquals(dateDefault.toString(), date2.toString());

        date2 = DateUtils.getDefaultIfEmpty(null, df, dateDefault);
        assertEquals(dateDefault.toString(), date2.toString());

        date2 = DateUtils.getDefaultIfEmpty(DAY + "/" + (MONTH + 1) + "/" + YEAR, null, dateDefault);
        assertEquals(dateDefault.toString(), date2.toString());

        date2 = DateUtils.getDefaultIfEmpty("UNPARSEABLE", df, dateDefault);
        assertEquals(dateDefault.toString(), date2.toString());
    }

    /**
     * Check get null date if empty
     */
    @Test
    public void testGetNullIfEmpty() {
        DateFormat df = new SimpleDateFormat("dd/MM/yyyy");

        Date date2 = DateUtils.getNullIfEmpty(DAY + "/" + (MONTH + 1) + "/" + YEAR, df);
        assertEquals(date2.toString(), date2.toString());

        date2 = DateUtils.getNullIfEmpty(null, df);
        assertNull(date2);

        date2 = DateUtils.getNullIfEmpty("", df);
        assertNull(date2);

        date2 = DateUtils.getNullIfEmpty("UNPARSEABLE", df);
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
}
