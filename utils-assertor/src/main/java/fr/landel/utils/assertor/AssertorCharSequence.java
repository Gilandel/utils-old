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
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.TriFunction;

public class AssertorCharSequence extends Constants {

    protected static <T extends CharSequence, E extends Throwable> AssertorResult<T> hasLength(final AssertorResult<T> result,
            final int length, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> length >= 0 && object != null;

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage.getDefaultMessage(result,
                MSG.CSQ.LENGTH, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> object.length() == length;

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.CSQ.LENGTH, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(length, EnumType.NUMBER_INTEGER));
    }

    protected static <T extends CharSequence, E extends Throwable> AssertorResult<T> isEmpty(final AssertorResult<T> result,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> StringUtils.isEmpty(object);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.CSQ.EMPTY, not, objectIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false);
    }

    protected static <T extends CharSequence, E extends Throwable> AssertorResult<T> isNotEmpty(final AssertorResult<T> result,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> StringUtils.isNotEmpty(object);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.CSQ.EMPTY, !not, objectIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false);
    }

    protected static <T extends CharSequence, E extends Throwable> AssertorResult<T> isBlank(final AssertorResult<T> result,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> StringUtils.isBlank(object);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.CSQ.BLANK, not, objectIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false);
    }

    protected static <T extends CharSequence, E extends Throwable> AssertorResult<T> isNotBlank(final AssertorResult<T> result,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> StringUtils.isNotBlank(object);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.CSQ.BLANK, !not, objectIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false);
    }

    protected static <T extends CharSequence, E extends Throwable> AssertorResult<T> contains(final AssertorResult<T> result,
            final CharSequence substring, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && StringUtils.isNotEmpty(substring);

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage.getDefaultMessage(result,
                MSG.CSQ.CONTAINS, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> containsCharSequence(object, substring);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.CSQ.CONTAINS, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
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

    protected static <T extends CharSequence, E extends Throwable> AssertorResult<T> startsWith(final AssertorResult<T> result,
            final CharSequence substring, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && StringUtils.isNotEmpty(substring);

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage.getDefaultMessage(result,
                MSG.CSQ.STARTS, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> StringUtils.startsWith(object, substring);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.CSQ.STARTS, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(substring, EnumType.CHAR_SEQUENCE));
    }

    protected static <T extends CharSequence, E extends Throwable> AssertorResult<T> startsWithIgnoreCase(final AssertorResult<T> result,
            final CharSequence substring, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && StringUtils.isNotEmpty(substring);

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage.getDefaultMessage(result,
                MSG.CSQ.STARTS, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> StringUtils.startsWithIgnoreCase(object, substring);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.CSQ.STARTS, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(substring, EnumType.CHAR_SEQUENCE));
    }

    protected static <T extends CharSequence, E extends Throwable> AssertorResult<T> endsWith(final AssertorResult<T> result,
            final CharSequence substring, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && StringUtils.isNotEmpty(substring);

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage.getDefaultMessage(result,
                MSG.CSQ.ENDS, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> StringUtils.endsWith(object, substring);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.CSQ.ENDS, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(substring, EnumType.CHAR_SEQUENCE));
    }

    protected static <T extends CharSequence, E extends Throwable> AssertorResult<T> endsWithIgnoreCase(final AssertorResult<T> result,
            final CharSequence substring, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && StringUtils.isNotEmpty(substring);

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage.getDefaultMessage(result,
                MSG.CSQ.ENDS, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> StringUtils.endsWithIgnoreCase(object, substring);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.CSQ.ENDS, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(substring, EnumType.CHAR_SEQUENCE));
    }

    protected static <T extends CharSequence, E extends Throwable> AssertorResult<T> matches(final AssertorResult<T> result,
            final Pattern pattern, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && pattern != null;

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage.getDefaultMessage(result,
                MSG.CSQ.MATCHES, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> pattern.matcher(object).matches();

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.CSQ.MATCHES, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(pattern, EnumType.UNKNOWN));
    }

    protected static <T extends CharSequence, E extends Throwable> AssertorResult<T> matches(final AssertorResult<T> result,
            final CharSequence regex, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && StringUtils.isNotEmpty(regex);

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage.getDefaultMessage(result,
                MSG.CSQ.MATCHES, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> Pattern.matches(regex.toString(), object);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.CSQ.MATCHES, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(regex, EnumType.CHAR_SEQUENCE));
    }

    protected static <T extends CharSequence, E extends Throwable> AssertorResult<T> find(final AssertorResult<T> result,
            final Pattern pattern, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && pattern != null;

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage.getDefaultMessage(result,
                MSG.CSQ.FIND, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> pattern.matcher(object).find();

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.CSQ.FIND, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(pattern, EnumType.UNKNOWN));
    }

    protected static <T extends CharSequence, E extends Throwable> AssertorResult<T> find(final AssertorResult<T> result,
            final CharSequence regex, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && StringUtils.isNotEmpty(regex);

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage.getDefaultMessage(result,
                MSG.CSQ.FIND, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> Pattern.compile(regex.toString()).matcher(object)
                .find();

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.CSQ.FIND, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(regex, EnumType.CHAR_SEQUENCE));
    }
}