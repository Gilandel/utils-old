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

import java.util.Locale;
import java.util.function.BiFunction;
import java.util.function.Function;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.TriFunction;

/**
 * Utility classes to check {@link Enum}
 *
 * @since 5 août 2016
 * @author Gilles
 *
 */
public class AssertorEnum extends Constants {

    /**
     * Checks if the enumeration has the specified name.
     * 
     * @param result
     *            the previous result
     * @param name
     *            the enumeration property name
     * @param locale
     *            the message locale
     * @param message
     *            the message if invalid
     * @param arguments
     *            the message arguments
     * @param <T>
     *            the enumeration type
     * @param <E>
     *            the checker exception type
     * @return the result supplier
     */
    protected static <T extends Enum<T>, E extends Throwable> AssertorResult<T> hasNameIgnoreCase(final AssertorResult<T> result,
            final CharSequence name, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> object.name().equalsIgnoreCase(name.toString());

        return AssertorEnum.hasName(result, name, MSG.ENUM.NAME, checker, locale, message, arguments);
    }

    /**
     * Checks if the enumeration has the specified name.
     * 
     * @param result
     *            the previous result
     * @param name
     *            the enumeration property name
     * @param locale
     *            the message locale
     * @param message
     *            the message if invalid
     * @param arguments
     *            the message arguments
     * @param <T>
     *            the enumeration type
     * @param <E>
     *            the checker exception type
     * @return the result supplier
     */
    protected static <T extends Enum<T>, E extends Throwable> AssertorResult<T> hasName(final AssertorResult<T> result,
            final CharSequence name, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> object.name().equals(name);

        return AssertorEnum.hasName(result, name, MSG.ENUM.NAME, checker, locale, message, arguments);
    }

    private static <T extends Enum<T>, E extends Throwable> AssertorResult<T> hasName(final AssertorResult<T> result,
            final CharSequence name, final CharSequence key, final BiFunctionThrowable<T, Boolean, Boolean, E> checker, final Locale locale,
            final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && StringUtils.isNotEmpty(name);

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage.getDefaultMessage(result, key,
                true, false, objectIndex, paramIndex);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, Assertor.getLocale(locale), message, arguments, key, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(name, EnumType.CHAR_SEQUENCE));
    }

    /**
     * Checks if the enumeration has the specified ordinal.
     * 
     * @param result
     *            the previous result
     * @param ordinal
     *            the enumeration property ordinal
     * @param locale
     *            the message locale
     * @param message
     *            the message if invalid
     * @param arguments
     *            the message arguments
     * @param <T>
     *            the enumeration type
     * @param <E>
     *            the checker exception type
     * @return the result supplier
     */
    protected static <T extends Enum<T>, E extends Throwable> AssertorResult<T> hasOrdinal(final AssertorResult<T> result, final int ordinal,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && ordinal >= 0;

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage.getDefaultMessage(result,
                MSG.ENUM.ORDINAL, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> object.ordinal() == ordinal;

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.ENUM.ORDINAL, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(ordinal, EnumType.NUMBER_INTEGER));
    }
}