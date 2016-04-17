/*
 * #%L
 * gl-utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package org.gl.utils.commons;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;

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

    private static final Logger LOGGER = LoggerFactory.getLogger(DateUtils.class);

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
}
