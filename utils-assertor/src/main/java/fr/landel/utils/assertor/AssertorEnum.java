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
import java.util.function.Function;
import java.util.function.Supplier;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.QuadFunction;
import fr.landel.utils.commons.function.TriFunction;

/**
 * Utility classes to check {@link Enum}
 *
 * @since 5 août 2016
 * @author Gilles
 *
 */
public class AssertorEnum extends AssertorConstants {

    /**
     * Checks if the enumeration has the specified name.
     * 
     * @param step
     *            the step supplier
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
    protected static <T extends Enum<T>, E extends Throwable> Supplier<AssertorResult<T>> isNameIgnoreCase(
            final Supplier<AssertorResult<T>> step, final CharSequence name, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> object.name().equalsIgnoreCase(name.toString());

        return AssertorEnum.isName(step, name, MSG.ENUM.NAME, checker, locale, message, arguments);
    }

    /**
     * Checks if the enumeration has the specified name.
     * 
     * @param step
     *            the step supplier
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
    protected static <T extends Enum<T>, E extends Throwable> Supplier<AssertorResult<T>> isName(final Supplier<AssertorResult<T>> step,
            final CharSequence name, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> object.name().equals(name);

        return AssertorEnum.isName(step, name, MSG.ENUM.NAME, checker, locale, message, arguments);
    }

    private static <T extends Enum<T>, E extends Throwable> Supplier<AssertorResult<T>> isName(final Supplier<AssertorResult<T>> step,
            final CharSequence name, final CharSequence key, final BiFunctionThrowable<T, Boolean, Boolean, E> checker, final Locale locale,
            final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && StringUtils.isNotEmpty(name);

        final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, key, true, false, objectIndex, paramIndex);

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, Assertor.getLocale(locale), message, arguments, key, not, objectIndex,
                        paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(name, EnumType.CHAR_SEQUENCE));
    }

    /**
     * Checks if the enumeration has the specified ordinal.
     * 
     * @param step
     *            the step supplier
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
    protected static <T extends Enum<T>, E extends Throwable> Supplier<AssertorResult<T>> isOrdinal(final Supplier<AssertorResult<T>> step,
            final int ordinal, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && ordinal >= 0;

        final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.ENUM.ORDINAL, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> object.ordinal() == ordinal;

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, Assertor.getLocale(locale), message, arguments, MSG.ENUM.ORDINAL, not,
                        objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(ordinal, EnumType.NUMBER_INTEGER));
    }
}