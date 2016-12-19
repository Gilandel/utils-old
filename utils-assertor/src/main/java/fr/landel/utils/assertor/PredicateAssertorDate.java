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

import java.util.Date;
import java.util.Locale;

/**
 * (Description)
 *
 * @since 3 août 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateAssertorDate extends PredicateAssertor<PredicateStepDate, Date> {

    @Override
    default PredicateStepDate get(final StepAssertor<Date> result) {
        return () -> result;
    }

    @Override
    default PredicateAssertorDate not() {
        return () -> HelperAssertor.not(getStep());
    }

    default PredicateStepDate isEqual(final Date date) {
        return this.isEqual(date, null);
    }

    default PredicateStepDate isEqual(final Date date, final CharSequence message, final Object... arguments) {
        return this.isEqual(date, null, message, arguments);
    }

    default PredicateStepDate isEqual(final Date date, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorDate.isEqual(this.getStep(), date, Message.of(locale, message, arguments));
    }

    default PredicateStepDate isNotEqual(final Date date) {
        return this.isNotEqual(date, null);
    }

    default PredicateStepDate isNotEqual(final Date date, final CharSequence message, final Object... arguments) {
        return this.isNotEqual(date, null, message, arguments);
    }

    default PredicateStepDate isNotEqual(final Date date, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorDate.isNotEqual(this.getStep(), date, Message.of(locale, message, arguments));
    }

    default PredicateStepDate isAround(final Date date, final int calendarField, final int calendarAmount) {
        return this.isAround(date, calendarField, calendarAmount, null);
    }

    default PredicateStepDate isAround(final Date date, final int calendarField, final int calendarAmount, final CharSequence message,
            final Object... arguments) {
        return this.isAround(date, calendarField, calendarAmount, null, message, arguments);
    }

    default PredicateStepDate isAround(final Date date, final int calendarField, final int calendarAmount, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return () -> AssertorDate.isAround(this.getStep(), date, calendarField, calendarAmount, Message.of(locale, message, arguments));
    }

    default PredicateStepDate isNotAround(final Date date, final int calendarField, final int calendarAmount) {
        return this.isNotAround(date, calendarField, calendarAmount, null);
    }

    default PredicateStepDate isNotAround(final Date date, final int calendarField, final int calendarAmount, final CharSequence message,
            final Object... arguments) {
        return this.isNotAround(date, calendarField, calendarAmount, null, message, arguments);
    }

    default PredicateStepDate isNotAround(final Date date, final int calendarField, final int calendarAmount, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return () -> AssertorDate.isNotAround(this.getStep(), date, calendarField, calendarAmount, Message.of(locale, message, arguments));
    }

    default PredicateStepDate isAfter(final Date date) {
        return this.isAfter(date, null);
    }

    default PredicateStepDate isAfter(final Date date, final CharSequence message, final Object... arguments) {
        return this.isAfter(date, null, message, arguments);
    }

    default PredicateStepDate isAfter(final Date date, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorDate.isAfter(this.getStep(), date, Message.of(locale, message, arguments));
    }

    default PredicateStepDate isAfter(final Date date, final int calendarField, final int calendarAmount) {
        return this.isAfter(date, calendarField, calendarAmount, null);
    }

    default PredicateStepDate isAfter(final Date date, final int calendarField, final int calendarAmount, final CharSequence message,
            final Object... arguments) {
        return this.isAfter(date, calendarField, calendarAmount, null, message, arguments);
    }

    default PredicateStepDate isAfter(final Date date, final int calendarField, final int calendarAmount, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return () -> AssertorDate.isAfter(this.getStep(), date, calendarField, calendarAmount, Message.of(locale, message, arguments));
    }

    default PredicateStepDate isAfterOrEquals(final Date date) {
        return this.isAfterOrEquals(date, null);
    }

    default PredicateStepDate isAfterOrEquals(final Date date, final CharSequence message, final Object... arguments) {
        return this.isAfterOrEquals(date, null, message, arguments);
    }

    default PredicateStepDate isAfterOrEquals(final Date date, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorDate.isAfterOrEquals(this.getStep(), date, Message.of(locale, message, arguments));
    }

    default PredicateStepDate isAfterOrEquals(final Date date, final int calendarField, final int calendarAmount) {
        return this.isAfterOrEquals(date, calendarField, calendarAmount, null);
    }

    default PredicateStepDate isAfterOrEquals(final Date date, final int calendarField, final int calendarAmount,
            final CharSequence message, final Object... arguments) {
        return this.isAfterOrEquals(date, calendarField, calendarAmount, null, message, arguments);
    }

    default PredicateStepDate isAfterOrEquals(final Date date, final int calendarField, final int calendarAmount, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return () -> AssertorDate.isAfterOrEquals(this.getStep(), date, calendarField, calendarAmount,
                Message.of(locale, message, arguments));
    }

    default PredicateStepDate isBefore(final Date date) {
        return this.isBefore(date, null);
    }

    default PredicateStepDate isBefore(final Date date, final CharSequence message, final Object... arguments) {
        return this.isBefore(date, null, message, arguments);
    }

    default PredicateStepDate isBefore(final Date date, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorDate.isBefore(this.getStep(), date, Message.of(locale, message, arguments));
    }

    default PredicateStepDate isBefore(final Date date, final int calendarField, final int calendarAmount) {
        return this.isBefore(date, calendarField, calendarAmount, null);
    }

    default PredicateStepDate isBefore(final Date date, final int calendarField, final int calendarAmount, final CharSequence message,
            final Object... arguments) {
        return this.isBefore(date, calendarField, calendarAmount, null, message, arguments);
    }

    default PredicateStepDate isBefore(final Date date, final int calendarField, final int calendarAmount, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return () -> AssertorDate.isBefore(this.getStep(), date, calendarField, calendarAmount, Message.of(locale, message, arguments));
    }

    default PredicateStepDate isBeforeOrEquals(final Date date) {
        return this.isBeforeOrEquals(date, null);
    }

    default PredicateStepDate isBeforeOrEquals(final Date date, final CharSequence message, final Object... arguments) {
        return this.isBeforeOrEquals(date, null, message, arguments);
    }

    default PredicateStepDate isBeforeOrEquals(final Date date, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorDate.isBeforeOrEquals(this.getStep(), date, Message.of(locale, message, arguments));
    }

    default PredicateStepDate isBeforeOrEquals(final Date date, final int calendarField, final int calendarAmount) {
        return this.isBeforeOrEquals(date, calendarField, calendarAmount, null);
    }

    default PredicateStepDate isBeforeOrEquals(final Date date, final int calendarField, final int calendarAmount,
            final CharSequence message, final Object... arguments) {
        return this.isBeforeOrEquals(date, calendarField, calendarAmount, null, message, arguments);
    }

    default PredicateStepDate isBeforeOrEquals(final Date date, final int calendarField, final int calendarAmount, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return () -> AssertorDate.isBeforeOrEquals(this.getStep(), date, calendarField, calendarAmount,
                Message.of(locale, message, arguments));
    }
}