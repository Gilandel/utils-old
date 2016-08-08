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
import java.util.Date;
import java.util.Locale;
import java.util.Map;

/**
 * (Description)
 *
 * @since 3 août 2016
 * @author Gilles
 *
 */
public class Assertor {

    /**
     * default locale
     */
    private static Locale locale = Locale.getDefault();

    public static <S extends PredicateStep<S, T>, T> PredicateAssertor<S, T> that(final T t) {
        return () -> () -> new AssertorResult<>(t, EnumType.getType(t));
    }

    public static <T> PredicateAssertorClass<T> that(final Class<T> clazz) {
        return () -> () -> new AssertorResult<>(clazz, EnumType.CLASS);
    }

    public static PredicateAssertorBoolean that(final Boolean bool) {
        return () -> () -> new AssertorResult<>(bool, EnumType.BOOLEAN);
    }

    public static <N extends Number & Comparable<N>> PredicateAssertorNumber<N> that(final N number) {
        return () -> () -> new AssertorResult<>(number, EnumType.getType(number));
    }

    public static <T extends CharSequence> PredicateAssertorCharSequence<T> that(final T charSequence) {
        return () -> () -> new AssertorResult<>(charSequence, EnumType.CHAR_SEQUENCE);
    }

    public static <T> PredicateAssertorArray<T> that(final T[] array) {
        return () -> () -> new AssertorResult<>(array, EnumType.ARRAY);
    }

    public static <T> PredicateAssertorIterable<T> that(final Iterable<T> iterable) {
        return () -> () -> new AssertorResult<>(iterable, EnumType.ITERABLE);
    }

    public static <K, V> PredicateAssertorMap<K, V> that(final Map<K, V> map) {
        return () -> () -> new AssertorResult<>(map, EnumType.MAP);
    }

    public static PredicateAssertorDate that(final Date date) {
        return () -> () -> new AssertorResult<>(date, EnumType.DATE);
    }

    public static PredicateAssertorCalendar that(final Calendar calendar) {
        return () -> () -> new AssertorResult<>(calendar, EnumType.DATE);
    }

    /**
     * @return the locale
     */
    public static final Locale getLocale() {
        return Assertor.locale;
    }

    /**
     * @param locale
     *            The locale
     * @return the locale if not null otherwise the default one
     */
    public static final Locale getLocale(final Locale locale) {
        if (locale != null) {
            return locale;
        }
        return Assertor.locale;
    }

    /**
     * Define the default locale for the assertor. Be aware in
     * multi-threading...
     * 
     * @param locale
     *            the locale to set
     */
    public static final void setLocale(final Locale locale) {
        Assertor.locale = locale;
    }
}