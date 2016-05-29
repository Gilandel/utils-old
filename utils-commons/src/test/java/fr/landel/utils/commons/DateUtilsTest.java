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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import org.junit.Test;

/**
 * Check utility class (dates).
 *
 * @since 27 nov. 2015
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

        assertNull(DateUtils.getDate(null));

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
}