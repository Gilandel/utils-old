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

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.Transformer;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.tuple.Triple;

import fr.landel.utils.commons.CollectionUtils2;
import fr.landel.utils.commons.EnumChar;
import fr.landel.utils.commons.StringUtils;

/**
 * Prepare the {@link CharSequence} message before calling
 * {@link String#format}. This class replaces the parameters and arguments
 * indexes to replace all values in one call. To increase performance no regular
 * expression is used.
 *
 * @since Aug 9, 2016
 * @author Gilles
 *
 */
public final class HelperMessage extends Constants {

    // The regular expression from String#format
    // "%(\\d+\\$)?([-#+ 0,(\\<]*)?(\\d+)?(\\.\\d+)?([tT])?([a-zA-Z%])"

    /**
     * Empty {@code Object} array
     */
    private static final Object[] EMPTY_ARRAY = new Object[0];

    /**
     * Flags in regular expression (sorted for binarySearch)
     */
    private static final byte[] FLAGS = " #(+,-0<\\".getBytes(StandardCharsets.UTF_8);

    private static final Transformer<Triple<Object, EnumType, Boolean>, Object> PARAM_TRANSFORMER = new Transformer<Triple<Object, EnumType, Boolean>, Object>() {
        @Override
        public Object transform(final Triple<Object, EnumType, Boolean> input) {
            return input.getLeft();
        }
    };

    /**
     * Parse the string to find parameters and arguments expressions, changes
     * the index to match the combining of the two arrays and removes expression
     * with unavailable parameters or arguments.
     * 
     * <p>
     * Examples:
     * </p>
     * 
     * <pre>
     * "%s '%s*' '%s*' '%1$.2f' '%1$s*' '%s' '%s'"
     * 
     * // will become with 3 parameters and 2 arguments
     * 
     * "%4$s '%1$s' '%2$s' '%4$.2f' '%1$s' '%5$s' ''"
     * </pre>
     * 
     * @param text
     *            the text where to search
     * @param nbParameters
     *            the number of parameters
     * @param nbArguments
     *            the number of arguments
     * @return the new char sequence
     */
    public static CharSequence prepare(final CharSequence text, final int nbParameters, final int nbArguments) {
        return HelperMessage.prepare(text, nbParameters, 1, nbArguments, 1);
    }

    /**
     * Parse the string to find parameters and arguments expressions, changes
     * the index to match the combining of the two arrays and removes expression
     * with unavailable parameters or arguments.
     * 
     * <p>
     * Examples:
     * </p>
     * 
     * <pre>
     * "%s '%s*' '%s*' '%1$.2f' '%1$s*' '%s' '%s'"
     * 
     * // will become with 3 parameters and 2 arguments
     * 
     * "%4$s '%1$s' '%2$s' '%4$.2f' '%1$s' '%5$s' ''"
     * </pre>
     * 
     * @param text
     *            the text where to search
     * @param nbParameters
     *            the number of parameters
     * @param startParameters
     *            the position of the parameter found
     * @param nbArguments
     *            the number of arguments
     * @param startArguments
     *            the position of the first arguments (after parameters)
     * @return the new char sequence
     */
    public static CharSequence prepare(final CharSequence text, final int nbParameters, final int startParameters, final int nbArguments,
            final int startArguments) {

        final byte[] bytes = text.toString().getBytes(StandardCharsets.UTF_8);

        int start = -1;
        int state = 0;

        final Set<Group> groups = new TreeSet<>();

        int i;
        Group group = null;

        for (i = 0; i < bytes.length; i++) {
            if (group == null && bytes[i] == '%') {
                start = i;
                group = new Group(start);
            } else if (group != null) {
                if (state < 2 && bytes[i] > 47 && bytes[i] < 58) {
                    if (state == 0) {
                        state = 1;
                        group.index = 0;
                    }
                    group.index = group.index * 10 + bytes[i] - 48;
                } else if (state < 2 && bytes[i] == '$') {
                    state |= 2;
                } else if (state < 8 && Arrays.binarySearch(FLAGS, bytes[i]) > -1) {
                    state |= 4;
                    group.flags.append((char) bytes[i]);
                } else if (state < 16 && bytes[i] == '.') {
                    state |= 16;
                    group.number.append((char) bytes[i]);
                } else if (state < 64 && bytes[i] > 47 && bytes[i] < 58) {
                    if ((state & 16) == 16) {
                        state |= 32;
                    } else {
                        state |= 8;
                    }
                    group.number.append((char) bytes[i]);
                } else if (state < 64 && bytes[i] == 'T' || bytes[i] == 't') {
                    state |= 64;
                    group.time = (char) bytes[i];
                } else if (state < 128 && ((bytes[i] > 64 && bytes[i] < 91) || (bytes[i] > 96 && bytes[i] < 123) || bytes[i] == '%')) {
                    state |= 128;
                    group.type.append((char) bytes[i]);
                } else if (state < 256 && bytes[i] == '*') {
                    state |= 256;
                    group.asterisk = true;
                } else {
                    if ((state & 2) != 2 && group.index > -1) {
                        group.number.insert(0, String.valueOf(group.index));
                        group.index = -1;
                    }
                    if ((state & 128) == 128) {
                        group.end = i;
                        groups.add(group);
                    }
                    if (bytes[i] == '%') {
                        start = i;
                        state = 0;
                        group = new Group(start);
                    } else {
                        group = null;
                        state = 0;
                    }
                }
            }
        }

        if (group != null) {
            if ((state & 2) != 2 && group.index > -1) {
                group.number.insert(0, String.valueOf(group.index));
                group.index = -1;
            }

            group.end = i;
            groups.add(group);
        }

        return replaceAndClear(groups, reindex(groups, text, nbParameters, startParameters, nbArguments, startArguments));
    }

    private static StringBuilder reindex(final Set<Group> groups, final CharSequence text, final int nbParameters,
            final int startParameters, final int nbArguments, final int startArguments) {

        int posArg = startParameters - 1 + nbParameters + startArguments;
        int posParam = startParameters;
        final StringBuilder sb = new StringBuilder(text);

        for (Group group : groups) {
            if (group.index == -1) {
                if (group.asterisk) {
                    if (nbParameters < posParam) {
                        group.remove = true;
                    } else {
                        group.index = posParam;
                        posParam++;
                    }
                } else if (nbArguments < posArg - nbParameters) {
                    group.remove = true;
                } else {
                    group.index = posArg;
                    posArg++;
                }
            } else if (!group.asterisk) {
                if (nbArguments < group.index) {
                    group.remove = true;
                } else {
                    group.index += nbParameters;
                }
            } else if (nbParameters < group.index) {
                group.remove = true;
            }
        }
        return sb;
    }

    private static StringBuilder replaceAndClear(final Set<Group> groups, final StringBuilder sb) {
        groups.stream().sorted(Group.REVERSED_COMPARATOR).forEachOrdered((g) -> {
            if (g.remove) {
                sb.replace(g.start, g.end, "");
            } else {
                g.asterisk = false;
                sb.replace(g.start, g.end, g.toString());
            }
        });

        return sb;
    }

    /**
     * Converts parameters list into array and also converts types to improve
     * readability (ex: {@link Calendar} into {@link java.util.Date})
     * 
     * @param parameters
     *            the input list
     * @return the output array
     */
    public static Object[] convertParams(final List<Triple<Object, EnumType, Boolean>> parameters) {
        if (CollectionUtils.isNotEmpty(parameters)) {
            final List<Object> convertedParams = CollectionUtils2.transformIntoList(parameters, PARAM_TRANSFORMER);
            // The object, the type and if it's a checked object
            Triple<Object, EnumType, Boolean> triple;
            int calendarField = -1;

            // in order for binary search
            final EnumType[] surroundable = new EnumType[] {EnumType.ARRAY, EnumType.ITERABLE, EnumType.MAP};

            for (int i = 0; i < parameters.size(); i++) {
                triple = parameters.get(i);
                if (triple.getLeft() != null) {
                    if (EnumType.DATE.equals(triple.getMiddle()) && Calendar.class.isAssignableFrom(triple.getLeft().getClass())) {
                        convertedParams.set(i, ((Calendar) triple.getLeft()).getTime());
                    } else if (EnumType.CALENDAR_FIELD.equals(triple.getMiddle())) {
                        calendarField = (Integer) triple.getLeft();
                        if (CALENDAR_FIELDS.containsKey(calendarField)) {
                            convertedParams.set(i, CALENDAR_FIELDS.get(calendarField));
                        }
                    } else if (EnumType.CLASS.equals(triple.getMiddle())) {
                        convertedParams.set(i, ((Class<?>) triple.getLeft()).getSimpleName());
                    } else if (Arrays.binarySearch(surroundable, triple.getMiddle()) > -1) {
                        convertedParams.set(i, HelperMessage.surroundByBrackets(triple.getLeft(), triple.getMiddle()));
                    }
                }
            }
            return convertedParams.toArray();
        }
        return new Object[0];
    }

    private static StringBuilder surroundByBrackets(final Object object, final EnumType type) {
        final StringBuilder sb = new StringBuilder(EnumChar.BRACKET_OPEN.getUnicode());
        if (EnumType.ARRAY.equals(type)) {
            sb.append(StringUtils.join((Object[]) object, StringUtils.JOIN_SEPARATOR));
        } else if (EnumType.ITERABLE.equals(type)) {
            sb.append(StringUtils.join((Iterable<?>) object, StringUtils.JOIN_SEPARATOR));
        } else { // see surroundable
            Map<?, ?> map = (Map<?, ?>) object;
            sb.append(StringUtils.join(map.entrySet(), StringUtils.JOIN_SEPARATOR));
        }
        return sb.append(EnumChar.BRACKET_CLOSE.getUnicode());
    }

    /**
     * Gets the message (the locale can be change through <code>setLocale</code>
     * ). Supports injecting parameters in message by using %s* or %1$s*
     * 
     * <pre>
     * Operator.getMessage(10, 20, &quot;The number '%s*' is not greater than number '%s*'&quot;);
     * // Exception: "The number '10' is not greater than number '20'"
     * Operator.getMessage(10, 20, &quot;'%2$s*' &gt; '%1$s*'&quot;);
     * // Exception: "'20' &gt; '10'"
     * </pre>
     * 
     * @param result
     *            The assertor result
     * @param locale
     *            The message locale
     * @param message
     *            The user message
     * @param arguments
     *            The user arguments
     * @param defaultKey
     *            The default message key
     * @param not
     *            If not has to be appended to the default message key
     * @param paramIndexes
     *            The list of parameters to inject in message
     * @param <T>
     *            the type assertor result
     * @return The message formatted
     */
    protected static <T> String getMessage(final AssertorResult<T> result, final Locale locale, final CharSequence message,
            final Object[] arguments, final CharSequence defaultKey, final boolean not, final Integer... paramIndexes) {

        final String currentMessage;
        final Object[] currentArguments;
        if (message != null) {
            currentMessage = message.toString();
            currentArguments = arguments;
        } else {
            currentMessage = HelperMessage.getDefaultMessage(result, defaultKey, false, not, paramIndexes).toString();
            currentArguments = null;
        }

        if (currentMessage.indexOf('%') > -1) {
            return HelperMessage.getMessage(Constants.DEFAULT_ASSERTION, locale, currentMessage, result.getParameters(), currentArguments);
        } else {
            return currentMessage;
        }
    }

    /**
     * Gets the message (the locale can be change through <code>setLocale</code>
     * ). Supports injecting parameters in message by using %s* or %1$s*
     * 
     * <pre>
     * Operator.getMessage(10, 20, &quot;The number '%s*' is not greater than number '%s*'&quot;);
     * // Exception: "The number '10' is not greater than number '20'"
     * Operator.getMessage(10, 20, &quot;'%2$s*' &gt; '%1$s*'&quot;);
     * // Exception: "'20' &gt; '10'"
     * </pre>
     * 
     * @param defaultString
     *            The default message provided by each method
     * @param locale
     *            The message locale
     * @param message
     *            The user message
     * @param parameters
     *            The method parameters
     * @param arguments
     *            The user arguments
     * @return The message formatted
     */
    public static String getMessage(final CharSequence defaultString, final Locale locale, final CharSequence message,
            final List<Triple<Object, EnumType, Boolean>> parameters, final Object[] arguments) {

        String msg;
        Locale l = Assertor.getLocale(locale);

        if (StringUtils.isNotEmpty(message)) {
            if (CollectionUtils.isNotEmpty(parameters) || ArrayUtils.isNotEmpty(arguments)) {
                Object[] params = HelperMessage.convertParams(parameters);
                Object[] args = ObjectUtils.defaultIfNull(arguments, EMPTY_ARRAY);

                msg = HelperMessage.prepare(message, params.length, 1, args.length, 1).toString();

                msg = String.format(Assertor.getLocale(l), msg, ArrayUtils.addAll(params, args));
            } else {
                msg = message.toString();
            }
        } else {
            msg = defaultString.toString();
        }
        return msg;
    }

    /**
     * Get the message and define that the current condition uses a personalized
     * message, not the default one
     * 
     * @param result
     *            The assertor result (required, not null)
     * @param key
     *            The message key (required, not null)
     * @param precondition
     *            If 'precondition' suffix has to be appended
     * @param not
     *            If 'not' suffix has to be appended
     * @param paramIndexes
     *            The arguments index to replace in message
     * @param <T>
     *            the type of assertor result
     * @return The loaded property
     */
    protected static <T> CharSequence getDefaultMessage(final AssertorResult<T> result, final CharSequence key, final boolean precondition,
            final boolean not, final Integer... paramIndexes) {
        Objects.requireNonNull(result);
        Objects.requireNonNull(key);

        final StringBuilder keyProperty = new StringBuilder(key);

        if (precondition) {
            keyProperty.append(MSG.PRE);
        } else if (not) {
            // NOT is ignored if precondition mode
            // precondition is the same with or without not
            keyProperty.append(MSG.NOT);
        }

        final CharSequence[] arguments = new CharSequence[paramIndexes.length];
        for (int i = 0; i < paramIndexes.length; i++) {
            arguments[i] = HelperMessage.getParam(paramIndexes[i] + 1, result.getParameters().get(paramIndexes[i]).getMiddle());
        }

        return getProperty(keyProperty, arguments);
    }

    /**
     * Generates parameter string with default format for each supported types
     * 
     * @param index
     *            The index
     * @param type
     *            The index parameter type
     * @return the parameter string
     */
    protected static StringBuilder getParam(final int index, final EnumType type) {
        final StringBuilder stringBuilder = new StringBuilder();
        final String percent = "%";
        if (EnumType.CHAR_SEQUENCE.equals(type)) {
            stringBuilder.append(percent).append(index).append("$s*");
        } else if (EnumType.BOOLEAN.equals(type)) {
            stringBuilder.append(percent).append(index).append("$B*");
        } else if (EnumType.NUMBER_INTEGER.equals(type)) {
            stringBuilder.append(percent).append(index).append("$,d*");
        } else if (EnumType.NUMBER_DECIMAL.equals(type)) {
            stringBuilder.append(percent).append(index).append("$,.3f*");
        } else if (EnumType.DATE.equals(type)) {
            stringBuilder.append(percent).append(index).append("$tY*/");
            stringBuilder.append(percent).append(index).append("$tm*/");
            stringBuilder.append(percent).append(index).append("$td* ");
            stringBuilder.append(percent).append(index).append("$tH*:");
            stringBuilder.append(percent).append(index).append("$tM*:");
            stringBuilder.append(percent).append(index).append("$tS* ");
            stringBuilder.append(percent).append(index).append("$tZ*");
        } else {
            stringBuilder.append(percent).append(index).append("$s*");
        }
        return stringBuilder;
    }

    /**
     * Class to manage groups and ordering
     *
     * @since Aug 9, 2016
     * @author Gilles
     *
     */
    private static class Group implements Comparable<Group> {

        private static final Comparator<Group> REVERSED_COMPARATOR = new Comparator<Group>() {
            @Override
            public int compare(Group o1, Group o2) {
                return o2.start - o1.start;
            }
        };

        private final int start;
        private int end;
        private int index = -1;
        private StringBuilder flags;
        private StringBuilder number;
        private char time;
        private StringBuilder type;
        private boolean asterisk;

        private boolean remove;

        /**
         * Constructor
         *
         * @param start
         *            the start index
         */
        public Group(final int start) {
            this.start = start;
            this.flags = new StringBuilder();
            this.number = new StringBuilder();
            this.type = new StringBuilder();
        }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder("%");
            sb.append(this.index).append('$');
            sb.append(this.flags);
            sb.append(this.number);
            if (this.time > 0) {
                sb.append(this.time);
            }
            sb.append(this.type);
            return sb.toString();
        }

        @Override
        public int compareTo(final Group o) {
            return this.start - o.start;
        }
    }
}
