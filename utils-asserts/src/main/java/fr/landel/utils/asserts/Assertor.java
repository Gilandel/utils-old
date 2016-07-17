/*
 * #%L
 * utils-asserts
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.asserts;

import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.Map;

/**
 * Assertion utility class that assists in validating arguments.
 *
 * <p>
 * Useful for identifying programmer errors early and clearly at runtime.
 *
 * <p>
 * For example, if the contract of a public method states it does not allow
 * {@code null} arguments, {@code Assert} can be used to validate that contract.
 * Doing this clearly indicates a contract violation when it occurs and protects
 * the class's invariants.
 *
 * <p>
 * Typically used to validate method arguments rather than configuration
 * properties, to check for cases that are usually programmer errors rather than
 * configuration errors. In contrast to configuration initialization code, there
 * is usually no point in falling back to defaults in such methods.
 *
 * <p>
 * This class is similar to JUnit's assertion library. If an argument value is
 * deemed invalid, an {@link IllegalArgumentException} is thrown (typically).
 * But the developer can also specified a specific exception since Java 8. For
 * example:
 *
 * <pre>
 * Assertor.that(clazz).isNotNull().toThrow(&quot;The class must not be null&quot;);
 * Assertor.that(clazz).isNotNull().and().isAssignable(superClazz)
 * Assertor.that(i).isGT(0).toThrow(&quot;The value must be greater than zero&quot;);
 * Assertor.that(bool).isTrue().toThrow(new MyException(&quot;The value must be true&quot;));
 * 
 * // The following code is equals to the next one 
 * AssertCharSequence&lt;String&gt; assertor = Assertor.that("text");
 * assertor.contains("__");
 * assertor.contains("ext");
 * assertTrue(assertor.getResult());
 * 
 * // The next one
 * assertTrue(Assertor.that("text").contains("__").and().contains("ext").getResult());
 * </pre>
 * 
 * <p>
 * Optionally, the checked parameters can be displayed in exception messages by
 * using %p or %1$p
 * </p>
 * 
 * <pre>
 * Assertor.that(10).isGT(20).toThrow(&quot;The number '%p' is not greater than number '%p'&quot;);
 * // Exception: "The number '10' is not greater than number '20'"
 * 
 * Assertor.that(10).isGT(20).toThrow(&quot;'%2$p' &gt; '%1$p'&quot;);
 * // Exception: "'20' &gt; '10'"
 * </pre>
 *
 * Based on:
 * 
 * @see <a href=
 *      "http://docs.spring.io/spring/docs/2.0.x/api/org/springframework/util/Assert.html?is-external=true">
 *      org.springframework.util.Assert</a>
 *
 * @since 1 juil. 2016
 * @author Gilles
 */
public class Assertor {

    /**
     * AND operator
     */
    protected static final int AND = 0;

    /**
     * OR operator
     */
    protected static final int OR = 1;

    /**
     * XOR operator
     */
    protected static final int XOR = 2;

    /**
     * The operator strings
     */
    protected static final String[] OPERATORS = {" AND ", " OR ", " XOR "};

    /**
     * Default assertion prefix
     */
    private static final String ASSERTION_FAILED = "[Assertion failed] ";

    /**
     * Default locale
     */
    private static Locale locale = Locale.US;

    private static String assertionPrefix = ASSERTION_FAILED;

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
     * @param assertionPrefix
     *            the assertionPrefix to set
     */
    public static void setAssertionPrefix(String assertionPrefix) {
        Assertor.assertionPrefix = assertionPrefix;
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
        return (A) new AssertObject<>(object);
    }
}