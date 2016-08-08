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
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.QuadFunction;
import fr.landel.utils.commons.function.TriFunction;

public class AssertorCharSequence extends AssertorConstants {

    protected static <T extends CharSequence, E extends Throwable> Supplier<AssertorResult<T>> hasLength(
            final Supplier<AssertorResult<T>> step, final int length, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> length >= 0 && object != null;

        final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.CSQ.LENGTH, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> object.length() == length;

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.CSQ.LENGTH, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(length, EnumType.NUMBER_INTEGER));
    }

    protected static <T extends CharSequence, E extends Throwable> Supplier<AssertorResult<T>> isEmpty(
            final Supplier<AssertorResult<T>> step, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> StringUtils.isEmpty(object);

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.CSQ.EMPTY, not, objectIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false);
    }

    protected static <T extends CharSequence, E extends Throwable> Supplier<AssertorResult<T>> isNotEmpty(
            final Supplier<AssertorResult<T>> step, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> StringUtils.isNotEmpty(object);

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.CSQ.EMPTY, !not, objectIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false);
    }

    protected static <T extends CharSequence, E extends Throwable> Supplier<AssertorResult<T>> isBlank(
            final Supplier<AssertorResult<T>> step, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> StringUtils.isBlank(object);

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.CSQ.BLANK, not, objectIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false);
    }

    protected static <T extends CharSequence, E extends Throwable> Supplier<AssertorResult<T>> isNotBlank(
            final Supplier<AssertorResult<T>> step, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> StringUtils.isNotBlank(object);

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.CSQ.BLANK, !not, objectIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false);
    }

    protected static <T extends CharSequence, E extends Throwable> Supplier<AssertorResult<T>> contains(
            final Supplier<AssertorResult<T>> step, final CharSequence substring, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && StringUtils.isNotEmpty(substring);

        final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.CSQ.CONTAINS, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> containsCharSequence(object, substring);

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.CSQ.CONTAINS, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(substring, EnumType.CHAR_SEQUENCE));
    }

    /**
     * Searches in char sequence, if the specified sub sequence exists in.
     * {@code null} values have to be checked before.
     * 
     * @param textToSearch
     *            where to search
     * @param substring
     *            chat to search
     * @return {@code true} if found, {@code false} otherwise
     */
    private static boolean containsCharSequence(final CharSequence textToSearch, final CharSequence substring) {
        int p = 0;
        int l = substring.length();
        for (int i = 0; i < textToSearch.length() & p < l; i++) {
            if (textToSearch.charAt(i) == substring.charAt(p)) {
                p++;
            }
        }
        return p == l;
    }

    protected static <T extends CharSequence, E extends Throwable> Supplier<AssertorResult<T>> startsWith(
            final Supplier<AssertorResult<T>> step, final CharSequence substring, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && StringUtils.isNotEmpty(substring);

        final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.CSQ.STARTS, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> StringUtils.startsWith(object, substring);

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.CSQ.STARTS, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(substring, EnumType.CHAR_SEQUENCE));
    }

    protected static <T extends CharSequence, E extends Throwable> Supplier<AssertorResult<T>> startsWithIgnoreCase(
            final Supplier<AssertorResult<T>> step, final CharSequence substring, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && StringUtils.isNotEmpty(substring);

        final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.CSQ.STARTS, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> StringUtils.startsWithIgnoreCase(object, substring);

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.CSQ.STARTS, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(substring, EnumType.CHAR_SEQUENCE));
    }

    protected static <T extends CharSequence, E extends Throwable> Supplier<AssertorResult<T>> endsWith(
            final Supplier<AssertorResult<T>> step, final CharSequence substring, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && StringUtils.isNotEmpty(substring);

        final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.CSQ.ENDS, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> StringUtils.endsWith(object, substring);

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.CSQ.ENDS, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(substring, EnumType.CHAR_SEQUENCE));
    }

    protected static <T extends CharSequence, E extends Throwable> Supplier<AssertorResult<T>> endsWithIgnoreCase(
            final Supplier<AssertorResult<T>> step, final CharSequence substring, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && StringUtils.isNotEmpty(substring);

        final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.CSQ.ENDS, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> StringUtils.endsWithIgnoreCase(object, substring);

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.CSQ.ENDS, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(substring, EnumType.CHAR_SEQUENCE));
    }

    protected static <T extends CharSequence, E extends Throwable> Supplier<AssertorResult<T>> matches(
            final Supplier<AssertorResult<T>> step, final Pattern pattern, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && pattern != null;

        final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.CSQ.MATCHES, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> pattern.matcher(object).matches();

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.CSQ.MATCHES, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(pattern, EnumType.UNKNOWN));
    }

    protected static <T extends CharSequence, E extends Throwable> Supplier<AssertorResult<T>> matches(
            final Supplier<AssertorResult<T>> step, final CharSequence regex, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && StringUtils.isNotEmpty(regex);

        final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.CSQ.MATCHES, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> Pattern.matches(regex.toString(), object);

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.CSQ.MATCHES, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(regex, EnumType.CHAR_SEQUENCE));
    }

    protected static <T extends CharSequence, E extends Throwable> Supplier<AssertorResult<T>> find(final Supplier<AssertorResult<T>> step,
            final Pattern pattern, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && pattern != null;

        final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.CSQ.FIND, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> pattern.matcher(object).find();

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.CSQ.FIND, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(pattern, EnumType.UNKNOWN));
    }

    protected static <T extends CharSequence, E extends Throwable> Supplier<AssertorResult<T>> find(final Supplier<AssertorResult<T>> step,
            final CharSequence regex, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && StringUtils.isNotEmpty(regex);

        final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.CSQ.FIND, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> Pattern.compile(regex.toString()).matcher(object)
                .find();

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.CSQ.FIND, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(regex, EnumType.CHAR_SEQUENCE));
    }
}