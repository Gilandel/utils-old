/*
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Utility class to manage strings.
 *
 * @since Nov 27, 2015
 * @author Gilles Landel
 *
 */
public final class StringUtils extends org.apache.commons.lang3.StringUtils {

    /**
     * The comma separator to join (for readability)
     */
    public static final String SEPARATOR_COMMA = ", ";

    /**
     * The semicolon separator to join (for readability)
     */
    public static final String SEPARATOR_SEMICOLON = "; ";

    private static final String BRACE_OPEN = "{";
    private static final String BRACE_CLOSE = "}";
    private static final String BRACE_OPEN_EXCLUDE = "{{";
    private static final String BRACE_CLOSE_EXCLUDE = "}}";
    private static final String BRACE_OPEN_TMP = "[#TMP#[";
    private static final String BRACE_CLOSE_TMP = "]#TMP#]";
    private static final String BRACES = "{}";

    /**
     * Hidden constructor.
     */
    private StringUtils() {
        super();
    }

    /**
     * Get the char sequence if not empty and null otherwise.
     * 
     * @param cs
     *            The CharSequence to check, may be null
     * @param <C>
     *            Type of the char sequence
     * @return a char sequence or null
     */
    public static <C extends CharSequence> C nullIfEmpty(final C cs) {
        if (isNotEmpty(cs)) {
            return cs;
        }
        return null;
    }

    /**
     * Get the char sequence if not null and not empty and the default one
     * otherwise.
     * 
     * @param cs
     *            The CharSequence to check, may be null
     * @param defaultCS
     *            The default char sequence
     * @param <C>
     *            Type of the char sequence
     * @return a char sequence
     */
    public static <C extends CharSequence> C defaultIfEmpty(final C cs, final C defaultCS) {
        if (isNotEmpty(cs)) {
            return cs;
        }
        return defaultCS;
    }

    /**
     * Get the char sequence if not null and the default one otherwise.
     * 
     * @param cs
     *            The CharSequence to check, may be null
     * @param defaultCS
     *            The default char sequence
     * @param <C>
     *            Type of the char sequence
     * @return a char sequence
     */
    public static <C extends CharSequence> C defaultIfNull(final C cs, final C defaultCS) {
        if (cs != null) {
            return cs;
        }
        return defaultCS;
    }

    /**
     * Get the toString if not null and the default one otherwise.
     * 
     * @param obj
     *            The object to check, may be null
     * @param defaultStr
     *            The default string
     * @return a string
     */
    public static String toStringOrDefaultIfNull(final Object obj, final String defaultStr) {
        if (obj != null) {
            return obj.toString();
        }
        return defaultStr;
    }

    /**
     * Get the string part between the separator at the index position. The
     * index starts from the left at 0, or at -1 from the right. If the index is
     * over the number of split parts, the method returns an empty string.
     * 
     * <pre>
     * StringUtils.substring("test1::test2:test3::test4", "::", 0) =&gt; "test1"
     * StringUtils.substring("test1::test2:test3::test4", "::", 1) =&gt; "test2:test3"
     * StringUtils.substring("test1::test2:test3::test4", "::", 2) =&gt; "test4"
     * StringUtils.substring("test1::test2:test3::test4", "::", -1) =&gt; "test4"
     * StringUtils.substring("test1::test2:test3::test4", "::", -2) =&gt; "test2:test3"
     * StringUtils.substring("test1::test2:test3::test4", "::", -3) =&gt; "test1"
     * StringUtils.substring("test1::test2:test3::test4", "::", 100) =&gt; ""
     * StringUtils.substring("test1::test2:test3::test4", "::", -100) =&gt; ""
     * </pre>
     * 
     * @param str
     *            The input string
     * @param separator
     *            The string separator
     * @param index
     *            The index position
     * @return The sub-string
     */
    public static String substring(final String str, final String separator, final int index) {
        if (index > -1) {
            return substring(str, separator, index, index + 1);
        } else {
            return substring(str, separator, index, index - 1);
        }
    }

    /**
     * Get the string part between the separator at the index position. The
     * index starts from the left at 0, or at -1 from the right. If the from
     * index is over the number of split parts, the method returns an empty
     * string (the to index isn't checked).
     * 
     * <pre>
     * StringUtils.substring("test1::test2:test3::test4", "::", 0, 1); // =&gt; "test1""
     * StringUtils.substring("test1::test2:test3::test4", "::", 1, 2); // =&gt; "test2:test3""
     * StringUtils.substring("test1::test2:test3::test4", "::", 1, 3); // =&gt; "test2:test3::test4""
     * StringUtils.substring("test1::test2:test3::test4", "::", 1, 100); // =&gt; "test2:test3::test4""
     * StringUtils.substring("test1::test2:test3::test4", "::", -1, -2); // =&gt; "test4"
     * StringUtils.substring("test1::test2:test3::test4", "::", -2, -3); // =&gt; "test2:test3"
     * StringUtils.substring("test1::test2:test3::test4", "::", 0, -2); // =&gt; "test1"
     * StringUtils.substring("test1::test2:test3::test4", "::", -3, 0); / =&gt; IllegalArgumentException, from=1, to=0
     * StringUtils.substring("test1::test2:test3::test4", "::", 0, 0); // =&gt; IllegalArgumentException, from=to
     * StringUtils.substring("test1::test2:test3::test4", "::", 0, -3); // =&gt; IllegalArgumentException, from=0, to=0
     * </pre>
     * 
     * @param str
     *            The input string
     * @param separator
     *            The string separator
     * @param from
     *            The from index position (inclusive)
     * @param to
     *            The to index position (exclusive)
     * @return The sub-string
     */
    public static String substring(final String str, final String separator, final int from, final int to) {
        if (isNotEmpty(str)) {
            if (from == to) {
                throw new IllegalArgumentException("The 'from' index is equal to 'to' index");
            }

            final List<String> strs = splitAsList(str, separator);
            final int size = strs.size();

            if (from >= size || (from < 0 && Math.abs(from) - 1 >= size)) {
                return "";
            } else {
                final StringBuilder stringBuilder = new StringBuilder();

                int end;
                int start;

                if (from > -1) {
                    start = from;
                    if (to > -1) {
                        end = to;
                    } else if (size + to > start) {
                        end = size + to;
                    } else {
                        throw new IllegalArgumentException("The 'to' index is invalid");
                    }
                } else if (to > -1) {
                    start = size + from + 1;
                    end = to;
                } else if (to < from) {
                    if (size + to + 1 < 0) {
                        start = 0;
                    } else {
                        start = size + to + 1;
                    }
                    end = size + from + 1;
                } else {
                    throw new IllegalArgumentException("The 'to' index is invalid");
                }

                if (start >= end) {
                    throw new IllegalArgumentException("The 'from' and 'to' indexes are invalid");
                }

                for (int i = start; i < end && i < size; i++) {
                    stringBuilder.append(strs.get(i));
                    if (i < end - 1 && i < size - 1) {
                        stringBuilder.append(separator);
                    }
                }

                return stringBuilder.toString();
            }
        }
        return str;
    }

    private static List<String> splitAsList(final String str, final String separator) {
        int pos = 0;
        int pPos = 0;
        final int separatorLength = separator.length();
        final List<String> strs = new ArrayList<>();
        while ((pos = str.indexOf(separator, pPos)) > -1) {
            strs.add(str.substring(pPos, pos));
            pPos = pos + separatorLength;
        }
        strs.add(str.substring(pPos, str.length()));
        return strs;
    }

    /**
     * Replace the part of a string between two bounds
     * 
     * <pre>
     * StringUtils.replace("I'll go to the beach this afternoon.", "theater", 15, 20)
     * // =&gt; "I'll go to the theater this afternoon."
     * </pre>
     * 
     * @param string
     *            The input string
     * @param replacement
     *            The replacement string
     * @param start
     *            The start position (exclusive)
     * @param end
     *            The end position (inclusive)
     * @return The new string
     * @throws IllegalArgumentException
     *             If input string is null or empty. If replacement string is
     *             null. If the start is lower than 0. If the start is lower
     *             than the end. If the end is greater than the string length.
     */
    public static String replace(final String string, final String replacement, final int start, final int end) {
        if (isEmpty(string)) {
            throw new IllegalArgumentException("The input string cannot be empty");
        } else if (replacement == null) {
            throw new IllegalArgumentException("The replacement string cannot be null");
        } else if (start < 0) {
            throw new IllegalArgumentException("The start parameter must be greated than or equal to 0");
        } else if (end > string.length()) {
            throw new IllegalArgumentException("The end parameter must be lower than or equal to the length of string");
        } else if (start >= end) {
            throw new IllegalArgumentException("The start parameter must be lower than the end");
        }

        String part1 = "";
        if (start > 0) {
            part1 = string.substring(0, start);
        }
        String part2 = "";
        if (end < string.length()) {
            part2 = string.substring(end);
        }

        return part1 + replacement + part2;
    }

    /**
     * Converts the char sequence in char array
     * 
     * @param sequence
     *            the input sequence
     * @return the array
     */
    public static char[] toChars(final CharSequence sequence) {
        Objects.requireNonNull(sequence);

        final int length = sequence.length();
        char[] chars = new char[length];
        for (int i = 0; i < length; i++) {
            chars[i] = sequence.charAt(i);
        }
        return chars;
    }

    /**
     * <p>
     * Joins the elements of the provided array into a single String containing
     * the provided list of elements. Each element is separated by a comma
     * followed by a space.
     * </p>
     *
     * <p>
     * No delimiter is added before or after the list. A {@code null} separator
     * is the same as an empty String (""). Null objects or empty strings within
     * the array are represented by empty strings.
     * </p>
     *
     * <pre>
     * StringUtils.join(null)            = null
     * StringUtils.join([])              = ""
     * StringUtils.join([null])          = ""
     * StringUtils.join(["a"])           = "a"
     * StringUtils.join(["a", "b", "c"]) = "a, b, c"
     * </pre>
     *
     * @param elements
     *            the array of values to join together, may be null
     * @param <T>
     *            the type of each element
     * @return the joined String, {@code null} if null array input
     */
    @SafeVarargs
    public static <T> String joinComma(final T... elements) {
        return join(elements, SEPARATOR_COMMA);
    }

    /**
     * <p>
     * Joins the elements of the provided {@code Iterable} into a single String
     * containing the provided elements.
     * </p>
     *
     * <p>
     * No delimiter is added before or after the list. The comma followed by a
     * space is used as separator (", ").
     * </p>
     *
     * <p>
     * See the examples here: {@link #join(Object[],String)}.
     * </p>
     *
     * @param iterable
     *            the {@link Iterable} providing the values to join together,
     *            may be null
     * @param <T>
     *            the type of each element
     * @return the joined String, {@code null} if null iterator input
     */
    public static <T> String joinComma(final Iterable<T> iterable) {
        if (iterable == null) {
            return null;
        }
        return join(iterable.iterator(), SEPARATOR_COMMA);
    }

    /**
     * Injects all arguments in the specified char sequence. The arguments are
     * injected by replacement of the braces. If no index is specified between
     * braces, an internal index is created and the index is automatically
     * incremented. The index starts from 0. To exclude braces, just double them
     * (like {{0}} will return {0}). If text or number greater than arguments
     * number are specified, they are ignored.
     * 
     * <p>
     * precondition: {@code charSequence} cannot be {@code null}
     * </p>
     * 
     * <pre>
     * StringUtils.inject("", "test"); // =&gt; ""
     * 
     * StringUtils.inject("I'll go to the {} this {}", "beach", "afternoon");
     * // =&gt; "I'll go to the beach this afternoon"
     * 
     * StringUtils.inject("I'll go to the {1} this {0}", "afternoon", "beach");
     * // =&gt; "I'll go to the beach this afternoon"
     * 
     * StringUtils.inject("I'll go to the {1} this {}", "afternoon", "beach");
     * // =&gt; "I'll go to the beach this afternoon"
     * 
     * StringUtils.inject("I'll go to {} {3} {} {2}", "the", "this", "afternoon", "beach");
     * // =&gt; "I'll go to the beach this afternoon"
     * 
     * StringUtils.inject("I'll go to {{}}{3} {} {2}{{0}} {4} {text}", "the", "this", "afternoon", "beach");
     * // =&gt; "I'll go to {}beach the afternoon{0} {4} {text}"
     * </pre>
     * 
     * @param charSequence
     *            the input char sequence
     * @param arguments
     *            the arguments to inject
     * @return the result with replacements
     */
    public static String inject(final CharSequence charSequence, final Object... arguments) {
        if (charSequence == null) {
            throw new IllegalArgumentException("The input char sequence cannot be null");
        } else if (isEmpty(charSequence) || arguments == null || arguments.length == 0) {
            return charSequence.toString();
        }

        final StringBuilder output = new StringBuilder(charSequence);

        // if no brace, just returns the string
        if (output.indexOf(BRACE_OPEN) < 0) {
            return output.toString();
        }

        // replace the excluded braces by a temporary string
        replaceBrace(output, BRACE_OPEN_EXCLUDE, BRACE_OPEN_TMP);
        replaceBrace(output, BRACE_CLOSE_EXCLUDE, BRACE_CLOSE_TMP);

        // replace the braces without index by the arguments
        int i = 0;
        int index = 0;
        while ((index = output.indexOf(BRACES, index)) > -1 && i < arguments.length) {
            output.replace(index, index + BRACES.length(), String.valueOf(arguments[i++]));
            index += BRACES.length();
        }

        // replace braces with index by the arguments
        int len;
        String param;
        for (i = 0; i < arguments.length; ++i) {
            index = 0;
            param = new StringBuilder(BRACE_OPEN).append(i).append(BRACE_CLOSE).toString();
            len = param.length();
            while ((index = output.indexOf(param, index)) > -1) {
                output.replace(index, index + len, String.valueOf(arguments[i]));
                index += len;
            }
        }

        // replace the temporary brace by the simple brace
        replaceBrace(output, BRACE_OPEN_TMP, BRACE_OPEN);
        replaceBrace(output, BRACE_CLOSE_TMP, BRACE_CLOSE);

        return output.toString();
    }

    private static void replaceBrace(final StringBuilder output, final String text, final String replacement) {
        int index = 0;
        while ((index = output.indexOf(text, index)) > -1) {
            output.replace(index, index + text.length(), replacement);
            index += replacement.length();
        }
    }
}
