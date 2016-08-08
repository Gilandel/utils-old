/*-
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

import java.util.Calendar;
import java.util.Locale;
import java.util.function.Supplier;

/**
 * (Description)
 *
 * @since 3 août 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateAssertorCalendar extends PredicateAssertor<PredicateStepCalendar, Calendar> {

    @Override
    default PredicateStepCalendar get(final Supplier<AssertorResult<Calendar>> supplier) {
        return () -> supplier;
    }

    @Override
    default PredicateAssertorCalendar not() {
        return () -> AssertorHelper.not(getStep());
    }

    default PredicateStepCalendar isEqual(final Calendar date) {
        return this.isEqual(date, null);
    }

    default PredicateStepCalendar isEqual(final Calendar date, final CharSequence message, final Object... arguments) {
        return this.isEqual(date, null, message, arguments);
    }

    default PredicateStepCalendar isEqual(final Calendar date, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorDate.isEqual(this.getStep(), date, locale, message, arguments);
    }

    default PredicateStepCalendar isNotEqual(final Calendar date) {
        return this.isNotEqual(date, null);
    }

    default PredicateStepCalendar isNotEqual(final Calendar date, final CharSequence message, final Object... arguments) {
        return this.isNotEqual(date, null, message, arguments);
    }

    default PredicateStepCalendar isNotEqual(final Calendar date, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorDate.isNotEqual(this.getStep(), date, locale, message, arguments);
    }

    default PredicateStepCalendar isAround(final Calendar date, final int calendarField, final int calendarAmount) {
        return this.isAround(date, calendarField, calendarAmount, null);
    }

    default PredicateStepCalendar isAround(final Calendar date, final int calendarField, final int calendarAmount,
            final CharSequence message, final Object... arguments) {
        return this.isAround(date, calendarField, calendarAmount, null, message, arguments);
    }

    default PredicateStepCalendar isAround(final Calendar date, final int calendarField, final int calendarAmount, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return () -> AssertorDate.isAround(this.getStep(), date, calendarField, calendarAmount, locale, message, arguments);
    }

    default PredicateStepCalendar isNotAround(final Calendar date, final int calendarField, final int calendarAmount) {
        return this.isNotAround(date, calendarField, calendarAmount, null);
    }

    default PredicateStepCalendar isNotAround(final Calendar date, final int calendarField, final int calendarAmount,
            final CharSequence message, final Object... arguments) {
        return this.isNotAround(date, calendarField, calendarAmount, null, message, arguments);
    }

    default PredicateStepCalendar isNotAround(final Calendar date, final int calendarField, final int calendarAmount, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return () -> AssertorDate.isNotAround(this.getStep(), date, calendarField, calendarAmount, locale, message, arguments);
    }

    default PredicateStepCalendar isAfter(final Calendar date) {
        return this.isAfter(date, null);
    }

    default PredicateStepCalendar isAfter(final Calendar date, final CharSequence message, final Object... arguments) {
        return this.isAfter(date, null, message, arguments);
    }

    default PredicateStepCalendar isAfter(final Calendar date, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorDate.isAfter(this.getStep(), date, locale, message, arguments);
    }

    default PredicateStepCalendar isAfterOrEquals(final Calendar date) {
        return this.isAfterOrEquals(date, null);
    }

    default PredicateStepCalendar isAfterOrEquals(final Calendar date, final CharSequence message, final Object... arguments) {
        return this.isAfterOrEquals(date, null, message, arguments);
    }

    default PredicateStepCalendar isAfterOrEquals(final Calendar date, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorDate.isAfterOrEquals(this.getStep(), date, locale, message, arguments);
    }

    default PredicateStepCalendar isBefore(final Calendar date) {
        return this.isBefore(date, null);
    }

    default PredicateStepCalendar isBefore(final Calendar date, final CharSequence message, final Object... arguments) {
        return this.isBefore(date, null, message, arguments);
    }

    default PredicateStepCalendar isBefore(final Calendar date, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorDate.isBefore(this.getStep(), date, locale, message, arguments);
    }

    default PredicateStepCalendar isBeforeOrEquals(final Calendar date) {
        return this.isBeforeOrEquals(date, null);
    }

    default PredicateStepCalendar isBeforeOrEquals(final Calendar date, final CharSequence message, final Object... arguments) {
        return this.isBeforeOrEquals(date, null, message, arguments);
    }

    default PredicateStepCalendar isBeforeOrEquals(final Calendar date, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorDate.isBeforeOrEquals(this.getStep(), date, locale, message, arguments);
    }
}