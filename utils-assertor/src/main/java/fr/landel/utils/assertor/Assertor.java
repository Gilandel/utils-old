/*
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
 * Assertion utility class that assists in validating arguments.
 *
 * <p>
 * Useful for identifying programmer errors early and clearly at runtime.
 * </p>
 *
 * <p>
 * For example, if the contract of a public method states it does not allow
 * {@code null} arguments, {@code Assert} can be used to validate that contract.
 * Doing this clearly indicates a contract violation when it occurs and protects
 * the class's invariants.
 * </p>
 * 
 * <p>
 * Typically used to validate method arguments rather than configuration
 * properties, to check for cases that are usually programmer errors rather than
 * configuration errors. In contrast to configuration initialization code, there
 * is usually no point in falling back to defaults in such methods.
 * </p>
 * 
 * <p>
 * This class is similar to JUnit's assertion library. If an argument value is
 * deemed invalid, an {@link IllegalArgumentException} is thrown (typically).
 * But the developer can also specified a specific exception since generic
 * throws.
 * </p>
 * 
 * <p>
 * No methods throw intermediate exception, so be aware with not() method, some
 * functions check for {@code null} by example and the result will not as
 * expected. More explanations here: {@link AbstractAssertObject#not()}.
 * </p>
 * 
 * Example:
 * 
 * <pre>
 * Assertor.that(clazz).isNotNull().toThrow("The class must not be null");
 * Assertor.that(clazz).isNotNull("Cannot be null").and().isAssignableFrom(superClazz, "Has to be a super of MyClass").toThrow();
 * Assertor.that(i).isGT(0).toThrow("The value must be greater than zero");
 * Assertor.that(bool).isTrue().toThrow(new MyException("The value must be true"));
 * 
 * // The following code is equals to the next one
 * AssertCharSequence&lt;String&gt; assertor = Assertor.that("text");
 * assertor.contains("__");
 * assertor.contains("ext");
 * assertTrue(assertor.isOK());
 * 
 * // The next one
 * assertTrue(Assertor.that("text").contains("__").and().contains("ext").isOK());
 * </pre>
 * 
 * <p>
 * Optionally, the checked parameters can be displayed in exception messages by
 * using %s*, %1$s* or %1$.2f* (this is exactly the same syntax as
 * {@link java.util.Formatter}, just add an asterisk/star at the end)
 * </p>
 * 
 * <pre>
 * Assertor.that(10).isGT(20).toThrow("The number '%s*' is not greater than number '%s*'");
 * // Exception: "The number '10' is not greater than number '20'"
 * 
 * Assertor.that(10).isGT(20).toThrow("'%2$s*' &gt; '%1$s*'");
 * // Exception: "'20' &gt; '10'"
 * 
 * Assertor.that(10).isGT(20).toThrow("%s '%2$s*' &gt; '%1$s*'", "test");
 * // Exception: "test '20' &gt; '10'"
 * </pre>
 * 
 * <p>
 * Beside this, another function is provided to check expected exception result
 * </p>
 *
 * Example:
 * 
 * <pre>
 * Expect.exception(() -&gt; {
 *     Assertor.that("").isNotEmpty().toThrow();
 *     fail();
 * }, IllegalArgumentException.class, "this parameter '' must have length; it must not be null or empty");
 * </pre>
 *
 * @see <a href=
 *      "http://docs.spring.io/spring/docs/2.0.x/api/org/springframework/util/Assert.html?is-external=true">
 *      Based on: org.springframework.util.Assert</a>
 *
 * @since 1 juil. 2016
 * @author Gilles
 */
public class Assertor extends AssertorConstants {

    /**
     * Set to the default locale
     */
    private static Locale locale = Locale.getDefault();

    /**
     * Set to the default assertion prefix
     */
    private static String assertionPrefix = ASSERTION_PREFIX;

    /**
     * Set to the default assertion suffix
     */
    private static String assertionSuffix = ASSERTION_SUFFIX;

    /**
     * Hidden constructor
     */
    private Assertor() {
    }

    /**
     * @return the locale
     */
    public static final Locale getLocale() {
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

    /**
     * @return the assertionPrefix
     */
    public static String getAssertionPrefix() {
        return Assertor.assertionPrefix;
    }

    /**
     * @return the assertionSuffix
     */
    public static String getAssertionSuffix() {
        return Assertor.assertionSuffix;
    }

    /**
     * @param assertionPrefix
     *            the assertionPrefix to set
     */
    public static void setAssertionPrefix(final String assertionPrefix) {
        Assertor.assertionPrefix = assertionPrefix;
    }

    /**
     * @param assertionSuffix
     *            the assertionSuffix to set
     */
    public static void setAssertionSuffix(final String assertionSuffix) {
        Assertor.assertionSuffix = assertionSuffix;
    }

    public static <N extends Number & Comparable<N>> AssertNumber<N> that(final N number) {
        return new AssertNumber<>(number);
    }

    public static <K, V> AssertMap<K, V> that(final Map<K, V> map) {
        return new AssertMap<>(map);
    }

    public static <I extends Iterable<T>, T> AssertIterable<I, T> that(final I iterable) {
        return new AssertIterable<>(iterable);
    }

    public static <A> AssertArray<A> that(final A[] array) {
        return new AssertArray<>(array);
    }

    public static <T extends CharSequence> AssertCharSequence<T> that(final T text) {
        return new AssertCharSequence<>(text);
    }

    public static AssertDate that(final Date date) {
        return new AssertDate(date);
    }

    public static AssertCalendar that(final Calendar calendar) {
        return new AssertCalendar(calendar);
    }

    public static AssertBoolean that(final Boolean expression) {
        return new AssertBoolean(expression);
    }

    public static <T> AssertClass<T> that(final Class<T> clazz) {
        return new AssertClass<>(clazz);
    }

    @SuppressWarnings("unchecked")
    public static <A extends AssertObject<A, T>, T> A that(final T object) {
        return (A) new AssertObject<>(object, TYPE.UNKNOWN);
    }
}
