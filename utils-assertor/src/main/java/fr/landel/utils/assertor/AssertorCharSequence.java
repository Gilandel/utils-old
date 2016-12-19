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

import java.util.function.BiPredicate;
import java.util.function.Predicate;
import java.util.regex.Pattern;

import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.commons.StringUtils;

/**
 * Utility class to prepare the check of {@link CharSequence}
 *
 * @since Aug 10, 2016
 * @author Gilles
 *
 */
public class AssertorCharSequence extends Constants {

    /**
     * Prepare the next step to validate if the {@link CharSequence} has the
     * specified length
     * 
     * <p>
     * precondition: {@link CharSequence} cannot be {@code null} and size cannot
     * be lower than zero
     * </p>
     * 
     * @param step
     *            the current step
     * @param length
     *            the expected length
     * @param message
     *            the message if invalid
     * @param <T>
     *            the char sequence type
     * @return the next step
     */
    protected static <T extends CharSequence> StepAssertor<T> hasLength(final StepAssertor<T> step, final int length,
            final Message message) {

        final Predicate<T> preChecker = (object) -> length >= 0 && object != null;

        final BiPredicate<T, Boolean> checker = (object, not) -> object.length() == length;

        return new StepAssertor<>(step, preChecker, checker, false, message, MSG.CSQ.LENGTH, false,
                Pair.of(length, EnumType.NUMBER_INTEGER));
    }

    /**
     * Prepare the next step to validate if the {@link CharSequence} is
     * {@code null} or empty
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * @param step
     *            the current step
     * @param message
     *            the message if invalid
     * @param <T>
     *            the char sequence type
     * @return the next step
     */
    protected static <T extends CharSequence> StepAssertor<T> isEmpty(final StepAssertor<T> step, final Message message) {

        final BiPredicate<T, Boolean> checker = (object, not) -> StringUtils.isEmpty(object);

        return new StepAssertor<>(step, checker, false, message, MSG.CSQ.EMPTY, false);
    }

    /**
     * Prepare the next step to validate if the {@link CharSequence} is NOT
     * {@code null} and NOT empty
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * @param step
     *            the current step
     * @param message
     *            the message if invalid
     * @param <T>
     *            the char sequence type
     * @return the next step
     */
    protected static <T extends CharSequence> StepAssertor<T> isNotEmpty(final StepAssertor<T> step, final Message message) {

        final BiPredicate<T, Boolean> checker = (object, not) -> StringUtils.isNotEmpty(object);

        return new StepAssertor<>(step, checker, false, message, MSG.CSQ.EMPTY, true);
    }

    /**
     * Prepare the next step to validate if the {@link CharSequence} is
     * {@code null}, empty or blank
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * @param step
     *            the current step
     * @param message
     *            the message if invalid
     * @param <T>
     *            the char sequence type
     * @return the next step
     */
    protected static <T extends CharSequence> StepAssertor<T> isBlank(final StepAssertor<T> step, final Message message) {

        final BiPredicate<T, Boolean> checker = (object, not) -> StringUtils.isBlank(object);

        return new StepAssertor<>(step, checker, false, message, MSG.CSQ.BLANK, false);
    }

    /**
     * Prepare the next step to validate if the {@link CharSequence} is NOT
     * {@code null}, NOT empty and NOT blank
     * 
     * <p>
     * precondition: none
     * </p>
     * 
     * @param step
     *            the current step
     * @param message
     *            the message if invalid
     * @param <T>
     *            the char sequence type
     * @return the next step
     */
    protected static <T extends CharSequence> StepAssertor<T> isNotBlank(final StepAssertor<T> step, final Message message) {

        final BiPredicate<T, Boolean> checker = (object, not) -> StringUtils.isNotBlank(object);

        return new StepAssertor<>(step, checker, false, message, MSG.CSQ.BLANK, true);
    }

    /**
     * Prepare the next step to validate if the {@link CharSequence} contains
     * the specified character
     * 
     * <p>
     * precondition: neither {@link CharSequence} or {@code character} cannot be
     * {@code null}
     * </p>
     * 
     * @param step
     *            the current step
     * @param character
     *            the character to find
     * @param message
     *            the message if invalid
     * @param <T>
     *            the char sequence type
     * @return the next step
     */
    protected static <T extends CharSequence> StepAssertor<T> contains(final StepAssertor<T> step, final Character character,
            final Message message) {

        final Predicate<T> preChecker = (object) -> object != null && character != null;

        final BiPredicate<T, Boolean> checker = (object, not) -> object.toString().indexOf(character) > -1;

        return new StepAssertor<>(step, preChecker, checker, false, message, MSG.CSQ.CONTAINS, false,
                Pair.of(character, EnumType.CHARACTER));
    }

    /**
     * Prepare the next step to validate if the {@link CharSequence} contains
     * the specified substring
     * 
     * <p>
     * precondition: {@link CharSequence} cannot be {@code null} and
     * {@code substring} cannot be {@code null} or empty
     * </p>
     * 
     * @param step
     *            the current step
     * @param substring
     *            the substring to find
     * @param message
     *            the message if invalid
     * @param <T>
     *            the char sequence type
     * @return the next step
     */
    protected static <T extends CharSequence> StepAssertor<T> contains(final StepAssertor<T> step, final CharSequence substring,
            final Message message) {

        final Predicate<T> preChecker = (object) -> object != null && StringUtils.isNotEmpty(substring);

        final BiPredicate<T, Boolean> checker = (object, not) -> containsCharSequence(object, substring);

        return new StepAssertor<>(step, preChecker, checker, false, message, MSG.CSQ.CONTAINS, false,
                Pair.of(substring, EnumType.CHAR_SEQUENCE));
    }

    /**
     * Prepare the next step to validate if the {@link CharSequence} starts with
     * the specified substring
     * 
     * <p>
     * precondition: {@link CharSequence} cannot be {@code null} and
     * {@code substring} cannot be {@code null} or empty
     * </p>
     * 
     * @param step
     *            the current step
     * @param substring
     *            the substring to find
     * @param message
     *            the message if invalid
     * @param <T>
     *            the char sequence type
     * @return the next step
     */
    protected static <T extends CharSequence> StepAssertor<T> startsWith(final StepAssertor<T> step, final CharSequence substring,
            final Message message) {

        final Predicate<T> preChecker = (object) -> object != null && StringUtils.isNotEmpty(substring);

        final BiPredicate<T, Boolean> checker = (object, not) -> StringUtils.startsWith(object, substring);

        return new StepAssertor<>(step, preChecker, checker, false, message, MSG.CSQ.STARTS, false,
                Pair.of(substring, EnumType.CHAR_SEQUENCE));
    }

    /**
     * Prepare the next step to validate if the {@link CharSequence} starts with
     * the specified substring (insensitive case)
     * 
     * <p>
     * precondition: {@link CharSequence} cannot be {@code null} and
     * {@code substring} cannot be {@code null} or empty
     * </p>
     * 
     * @param step
     *            the current step
     * @param substring
     *            the substring to find
     * @param message
     *            the message if invalid
     * @param <T>
     *            the char sequence type
     * @return the next step
     */
    protected static <T extends CharSequence> StepAssertor<T> startsWithIgnoreCase(final StepAssertor<T> step, final CharSequence substring,
            final Message message) {

        final Predicate<T> preChecker = (object) -> object != null && StringUtils.isNotEmpty(substring);

        final BiPredicate<T, Boolean> checker = (object, not) -> StringUtils.startsWithIgnoreCase(object, substring);

        return new StepAssertor<>(step, preChecker, checker, false, message, MSG.CSQ.STARTS, false,
                Pair.of(substring, EnumType.CHAR_SEQUENCE));
    }

    /**
     * Prepare the next step to validate if the {@link CharSequence} ends with
     * the specified substring
     * 
     * <p>
     * precondition: {@link CharSequence} cannot be {@code null} and
     * {@code substring} cannot be {@code null} or empty
     * </p>
     * 
     * @param step
     *            the current step
     * @param substring
     *            the substring to find
     * @param message
     *            the message if invalid
     * @param <T>
     *            the char sequence type
     * @return the next step
     */
    protected static <T extends CharSequence> StepAssertor<T> endsWith(final StepAssertor<T> step, final CharSequence substring,
            final Message message) {

        final Predicate<T> preChecker = (object) -> object != null && StringUtils.isNotEmpty(substring);

        final BiPredicate<T, Boolean> checker = (object, not) -> StringUtils.endsWith(object, substring);

        return new StepAssertor<>(step, preChecker, checker, false, message, MSG.CSQ.ENDS, false,
                Pair.of(substring, EnumType.CHAR_SEQUENCE));
    }

    /**
     * Prepare the next step to validate if the {@link CharSequence} ends with
     * the specified substring (insensitive case)
     * 
     * <p>
     * precondition: {@link CharSequence} cannot be {@code null} and
     * {@code substring} cannot be {@code null} or empty
     * </p>
     * 
     * @param step
     *            the current step
     * @param substring
     *            the substring to find
     * @param message
     *            the message if invalid
     * @param <T>
     *            the char sequence type
     * @return the next step
     */
    protected static <T extends CharSequence> StepAssertor<T> endsWithIgnoreCase(final StepAssertor<T> step, final CharSequence substring,
            final Message message) {

        final Predicate<T> preChecker = (object) -> object != null && StringUtils.isNotEmpty(substring);

        final BiPredicate<T, Boolean> checker = (object, not) -> StringUtils.endsWithIgnoreCase(object, substring);

        return new StepAssertor<>(step, preChecker, checker, false, message, MSG.CSQ.ENDS, false,
                Pair.of(substring, EnumType.CHAR_SEQUENCE));
    }

    /**
     * Prepare the next step to validate if the {@link CharSequence} matches the
     * specified pattern
     * 
     * <p>
     * precondition: neither {@link CharSequence} or {@code pattern} cannot be
     * {@code null}
     * </p>
     * 
     * @param step
     *            the current step
     * @param pattern
     *            the pattern to validate
     * @param message
     *            the message if invalid
     * @param <T>
     *            the char sequence type
     * @return the next step
     */
    protected static <T extends CharSequence> StepAssertor<T> matches(final StepAssertor<T> step, final Pattern pattern,
            final Message message) {

        final Predicate<T> preChecker = (object) -> object != null && pattern != null;

        final BiPredicate<T, Boolean> checker = (object, not) -> pattern.matcher(object).matches();

        return new StepAssertor<>(step, preChecker, checker, false, message, MSG.CSQ.MATCHES, false, Pair.of(pattern, EnumType.UNKNOWN));
    }

    /**
     * Prepare the next step to validate if the {@link CharSequence} matches the
     * specified regular expression
     * 
     * <p>
     * precondition: {@link CharSequence} cannot be {@code null} and the
     * expression regular cannot be {@code null} or empty
     * </p>
     * 
     * @param step
     *            the current step
     * @param regex
     *            the regular expression to validate
     * @param message
     *            the message if invalid
     * @param <T>
     *            the char sequence type
     * @return the next step
     */
    protected static <T extends CharSequence> StepAssertor<T> matches(final StepAssertor<T> step, final CharSequence regex,
            final Message message) {

        final Predicate<T> preChecker = (object) -> object != null && StringUtils.isNotEmpty(regex);

        final BiPredicate<T, Boolean> checker = (object, not) -> Pattern.matches(regex.toString(), object);

        return new StepAssertor<>(step, preChecker, checker, false, message, MSG.CSQ.MATCHES, false,
                Pair.of(regex, EnumType.CHAR_SEQUENCE));
    }

    /**
     * Prepare the next step to validate if the specified pattern can be found
     * in the {@link CharSequence}
     * 
     * <p>
     * precondition: neither {@link CharSequence} or {@code pattern} cannot be
     * {@code null}
     * </p>
     * 
     * @param step
     *            the current step
     * @param pattern
     *            the pattern to validate
     * @param message
     *            the message if invalid
     * @param <T>
     *            the char sequence type
     * @return the next step
     */
    protected static <T extends CharSequence> StepAssertor<T> find(final StepAssertor<T> step, final Pattern pattern,
            final Message message) {

        final Predicate<T> preChecker = (object) -> object != null && pattern != null;

        final BiPredicate<T, Boolean> checker = (object, not) -> pattern.matcher(object).find();

        return new StepAssertor<>(step, preChecker, checker, false, message, MSG.CSQ.FIND, false, Pair.of(pattern, EnumType.UNKNOWN));
    }

    /**
     * Prepare the next step to validate if the specified regular expression can
     * be found in the {@link CharSequence}
     * 
     * <p>
     * precondition: {@link CharSequence} cannot be {@code null} and the
     * expression regular cannot be {@code null} or empty
     * </p>
     * 
     * @param step
     *            the current step
     * @param regex
     *            the regular expression to validate
     * @param message
     *            the message if invalid
     * @param <T>
     *            the char sequence type
     * @return the next step
     */
    protected static <T extends CharSequence> StepAssertor<T> find(final StepAssertor<T> step, final CharSequence regex,
            final Message message) {

        final Predicate<T> preChecker = (object) -> object != null && StringUtils.isNotEmpty(regex);

        final BiPredicate<T, Boolean> checker = (object, not) -> Pattern.compile(regex.toString()).matcher(object).find();

        return new StepAssertor<>(step, preChecker, checker, false, message, MSG.CSQ.FIND, false, Pair.of(regex, EnumType.CHAR_SEQUENCE));
    }

    /**
     * Searches in char sequence, if the specified sub sequence exists in.
     * {@code null} values have to be checked first.
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
        for (int i = 0; i < textToSearch.length() && p < l; i++) {
            if (textToSearch.charAt(i) == substring.charAt(p)) {
                p++;
            } else {
                p = 0;
            }
        }
        return p == l;
    }
}