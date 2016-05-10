/*
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
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

/**
 * Utility class to manage strings.
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public final class StringUtils extends org.apache.commons.lang3.StringUtils {

    /**
     * Get the char sequence if not empty and null otherwise.
     * 
     * @param cs
     *            The CharSequence to check, may be null
     * @param <C>
     *            Type of the char sequence
     * @return a char sequence or null
     */
    public static <C extends CharSequence> C getNullIfEmpty(final C cs) {
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
    public static <C extends CharSequence> C getDefaultIfEmpty(final C cs, final C defaultCS) {
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
    public static <C extends CharSequence> C getDefaultIfNull(final C cs, final C defaultCS) {
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
    public static String getToStringOrDefaultIfNull(final Object obj, final String defaultStr) {
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
            } else if (size > 0) {
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
                    if (size + from + 1 < 0) {
                        start = 0;
                    } else {
                        start = size + from + 1;
                    }
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
     * StringUtils.replace("I go to the beach this afternoon.", "theater", 12, 17)
     * // =&gt; "I go to the theater this afternoon."
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
        Assert.isNotEmpty(string, "The input string cannot be empty");
        Assert.isNotNull(replacement, "The replacement string cannot be null");
        Assert.isGTE(start, 0, "The start parameter must be greated than or equal to 0");
        Assert.isLTE(end, string.length(), "The end parameter must be lower than or equal to the length of string");
        Assert.isLT(start, end, "The start parameter must be lower than the end");

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
}
