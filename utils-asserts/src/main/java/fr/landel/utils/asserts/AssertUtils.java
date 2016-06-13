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

import org.apache.commons.lang3.ArrayUtils;

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
 * AssertUtils.isNotNull(clazz, &quot;The class must not be null&quot;);
 * AssertUtils.isGT(i, 0, &quot;The value must be greater than zero&quot;);
 * AssertUtils.isTrue(bool, new MyException(&quot;The value must be true&quot;));
 * </pre>
 *
 * <p>
 * Mainly for internal use within the framework; consider
 * <a href="http://commons.apache.org/proper/commons-lang/">Apache's Commons
 * Lang</a> for a more comprehensive suite of {@code String} utilities.
 * 
 * <p>
 * Optionally, the checked parameters can be displayed in exception messages by
 * using %p or %1$p
 * </p>
 * 
 * <pre>
 * AssertUtils.isGT(10, 20, &quot;The number '%p' is not greater than number '%p'&quot;);
 * // Exception: "The number '10' is not greater than number '20'"
 * AssertUtils.isGT(10, 20, &quot;'%2$p' &gt; '%1$p'&quot;);
 * // Exception: "'20' &gt; '10'"
 * </pre>
 *
 * Based on:
 * 
 * @see <a href=
 *      "http://docs.spring.io/spring/docs/2.0.x/api/org/springframework/util/Assert.html?is-external=true">
 *      org.springframework.util.Assert</a>
 *
 * @author Keith Donald
 * @author Juergen Hoeller
 * @author Colin Sampaleanu
 * @author Rob Harrop
 * @author Sam Brannen
 * @author Gilles Landel
 * @since 1.1.2
 */
public class AssertUtils {

    private static Locale locale = Locale.US;

    private AssertUtils() {
    }

    /**
     * @return the locale
     */
    public static final Locale getLocale() {
        return AssertUtils.locale;
    }

    /**
     * @param locale
     *            the locale to set
     */
    public static final void setLocale(final Locale locale) {
        AssertUtils.locale = locale;
    }

    public static <N extends Number & Comparable<N>> AssertNumber<N> check(final N number) {
        return new AssertNumber<>(number);
    }

    public static <K, V> AssertMap<K, V> check(final Map<K, V> map) {
        return new AssertMap<>(map);
    }

    public static <I extends Iterable<X>, X> AssertIterable<I, X> check(final I iterable) {
        return new AssertIterable<>(iterable);
    }

    public static <A> AssertArray<A> check(final A[] array) {
        return new AssertArray<>(array);
    }

    public static <S extends CharSequence> AssertCharSequence<S> check(final S text) {
        return new AssertCharSequence<>(text);
    }

    public static AssertDate check(final Date date) {
        return new AssertDate(date);
    }

    public static AssertCalendar check(final Calendar calendar) {
        return new AssertCalendar(calendar);
    }

    public static AssertBoolean check(final Boolean expression) {
        return new AssertBoolean(expression);
    }

    public static <X> AssertClass<X> check(final Class<X> clazz) {
        return new AssertClass<>(clazz);
    }

    public static <T extends AssertObject<T, O>, O> AssertObject<T, O> check(final O object) {
        return new AssertObject<>(object);
    }

    public static <T extends AssertMultipleObject<T, Object>> AssertMultipleObject<T, Object> check(final Object object,
            final Object... objects) {
        return new AssertMultipleObject<>(ArrayUtils.add(new Object[] {object}, objects));
    }
}