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
 * The basis class to start an assertor chain. The 'that' method is defined here
 * for all managed types.
 * 
 * <p>
 * Global locale can also be defined here. The locale will be used for decimal
 * conversion for example. This locale is used if no locale is defined for the
 * error message.
 * </p>
 *
 * @since Aug 3, 2016
 * @author Gilles
 *
 */
public class Assertor {

    /**
     * default locale
     */
    private static Locale locale = Locale.getDefault();

    /**
     * First step to check an object.
     * 
     * @param t
     *            the object to check
     * @param <S>
     *            the predicate step type
     * @param <T>
     *            the type of object under checking
     * @return the predicate assertor
     */
    public static <S extends PredicateStep<S, T>, T> PredicateAssertor<S, T> that(final T t) {
        return () -> new StepAssertor<>(t, EnumType.getType(t));
    }

    /**
     * First step to check a class.
     * 
     * @param clazz
     *            the class to check
     * @param <T>
     *            the type of class under checking
     * @return the predicate class assertor
     */
    public static <T> PredicateAssertorClass<T> that(final Class<T> clazz) {
        return () -> new StepAssertor<>(clazz, EnumType.CLASS);
    }

    /**
     * First step to check a boolean.
     * 
     * @param bool
     *            the boolean to check
     * @return the predicate boolean assertor
     */
    public static PredicateAssertorBoolean that(final Boolean bool) {
        return () -> new StepAssertor<>(bool, EnumType.BOOLEAN);
    }

    /**
     * First step to check a number.
     * 
     * @param number
     *            the number to check
     * @param <N>
     *            the type of number under checking
     * @return the predicate number assertor
     */
    public static <N extends Number & Comparable<N>> PredicateAssertorNumber<N> that(final N number) {
        return () -> new StepAssertor<>(number, EnumType.getType(number));
    }

    /**
     * First step to check a char sequence.
     * 
     * @param charSequence
     *            the char sequence to check
     * @param <T>
     *            the type of char sequence under checking
     * @return the predicate char sequence assertor
     */
    public static <T extends CharSequence> PredicateAssertorCharSequence<T> that(final T charSequence) {
        return () -> new StepAssertor<>(charSequence, EnumType.CHAR_SEQUENCE);
    }

    /**
     * First step to check an array.
     * 
     * @param array
     *            the array to check
     * @param <T>
     *            the type of array under checking
     * @return the predicate array assertor
     */
    public static <T> PredicateAssertorArray<T> that(final T[] array) {
        return () -> new StepAssertor<>(array, EnumType.ARRAY);
    }

    /**
     * First step to check an iterable.
     * 
     * @param iterable
     *            the iterable to check
     * @param <T>
     *            the type of iterable under checking
     * @return the predicate iterable assertor
     */
    public static <T> PredicateAssertorIterable<T> that(final Iterable<T> iterable) {
        return () -> new StepAssertor<>(iterable, EnumType.ITERABLE);
    }

    /**
     * First step to check a map.
     * 
     * @param map
     *            the map to check
     * @param <K>
     *            the type of map's key under checking
     * @param <V>
     *            the type of map's value under checking
     * @return the predicate map assertor
     */
    public static <K, V> PredicateAssertorMap<K, V> that(final Map<K, V> map) {
        return () -> new StepAssertor<>(map, EnumType.MAP);
    }

    /**
     * First step to check an enumeration.
     * 
     * @param enumeration
     *            the enumeration to check
     * @param <T>
     *            the type of enumeration under checking
     * @return the predicate enumeration assertor
     */
    public static <T extends Enum<T>> PredicateAssertorEnum<T> that(final T enumeration) {
        return () -> new StepAssertor<>(enumeration, EnumType.ENUMERATION);
    }

    /**
     * First step to check a date.
     * 
     * @param date
     *            the date to check
     * @return the predicate date assertor
     */
    public static PredicateAssertorDate that(final Date date) {
        return () -> new StepAssertor<>(date, EnumType.DATE);
    }

    /**
     * First step to check a calendar.
     * 
     * @param calendar
     *            the calendar to check
     * @return the predicate calendar assertor
     */
    public static PredicateAssertorCalendar that(final Calendar calendar) {
        return () -> new StepAssertor<>(calendar, EnumType.CALENDAR);
    }

    /**
     * Get the global locale used for generate messages of exceptions
     * 
     * @return the locale
     */
    public static final Locale getLocale() {
        return Assertor.locale;
    }

    /**
     * Get the global locale used for generate messages of exceptions
     * 
     * @param locale
     *            The locale
     * @return the locale if not null otherwise the default one
     */
    static final Locale getLocale(final Locale locale) {
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