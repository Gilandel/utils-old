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

import java.util.Calendar;
import java.util.Locale;
import java.util.regex.Pattern;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ObjectUtils;

import fr.landel.utils.commons.StringUtils;

/**
 * 
 * Assertor helper class, to build exceptions and messages.
 *
 * @since 3 août 2016
 * @author Gilles
 *
 */
public final class AssertorHelper extends AssertorConstants {

    /**
     * {@link java.util.Formatter}
     * 
     * %[argument_index$][flags][width][.precision][t]conversion
     */
    private static final String FORMAT_SPECIFIER = "%(\\d+\\$)?([-#+ 0,(\\<]*)?(\\d+)?(\\.\\d+)?([tT])?([a-zA-Z%])";
    private static final Pattern PATTERN = Pattern.compile(FORMAT_SPECIFIER);
    private static final String PARAM_ID = "*";

    /**
     * Hidden Constructor
     */
    private AssertorHelper() {
        super();
    }

    /**
     * Manages exceptions (between raised a specific exception or the standard
     * IllegalArgumentException). If it's a specific exception, the default one
     * is added to it as suppressed exception.
     * 
     * @param defaultString
     *            The default message
     * @param exception
     *            The specific exception
     * @param locale
     *            The message locale
     * @param message
     *            The message for the IllegalArgumentException
     * @param parameters
     *            The method parameters
     * @param arguments
     *            The arguments for the IllegalArgumentException
     * @param <E>
     *            The specific exception type
     * @throws E
     *             If exception parameter is set
     */
    protected static <E extends Throwable> void manageExceptions(final CharSequence defaultString, final E exception, final Locale locale,
            final CharSequence message, final Object[] parameters, final Object[] arguments) throws E {
        if (exception != null) {
            exception.addSuppressed(new IllegalArgumentException(new StringBuilder().append(Assertor.getAssertionPrefix())
                    .append(defaultString).append(Assertor.getAssertionSuffix()).toString()));
            throw exception;
        } else {
            throw new IllegalArgumentException(AssertorHelper.getMessage(defaultString, locale, message, parameters, arguments));
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
            final Object[] parameters, final Object[] arguments) {
        String msg;

        Locale l = locale;
        if (locale == null) {
            l = Assertor.getLocale();
        }

        if (StringUtils.isNotEmpty(message)) {
            msg = message.toString();
            if (ArrayUtils.isNotEmpty(parameters)) {
                msg = AssertorHelper.formatParameters(l, msg, parameters);
            }
            if (ArrayUtils.isNotEmpty(arguments)) {
                msg = String.format(ObjectUtils.defaultIfNull(l, Assertor.getLocale()), msg, arguments);
            }
        } else {
            msg = defaultString.toString();
        }
        return new StringBuilder().append(Assertor.getAssertionPrefix()).append(msg).append(Assertor.getAssertionSuffix()).toString();
    }

    /**
     * Formats the parameter expressions without modification of standard
     * expressions
     * 
     * @param message
     *            The message where to replace the parameters
     * @param parameters
     *            The parameter values (ordered)
     * @return The formatted result
     */
    private static String formatParameters(final Locale locale, final String message, final Object[] parameters) {
        // detected character by {@link String#format}
        final String percent = "%";
        final String replacement = "\u00b6\u00b6\u00b6";

        final int paramIdLength = PARAM_ID.length();
        final int length = message.length();

        final StringBuilder newMessage = new StringBuilder();
        final java.util.regex.Matcher matcher = PATTERN.matcher(message);

        int start;
        int end;
        int previousEnd = 0;
        int move = 0;

        while (matcher.find()) {
            start = matcher.start();
            end = matcher.end();
            if (start - previousEnd - move > 0) {
                newMessage.append(message.substring(previousEnd + move, start));
            }
            if (end + paramIdLength > length || !PARAM_ID.equals(message.substring(end, end + paramIdLength))) {
                newMessage.append(StringUtils.replace(matcher.group(0), percent, replacement));
                move = 0;
            } else {
                newMessage.append(matcher.group(0));
                move = paramIdLength;
            }
            previousEnd = end;
        }

        if (previousEnd + move < length) {
            newMessage.append(message.substring(previousEnd + move, length));
        }

        return StringUtils.replace(String.format(locale, newMessage.toString(), convertParams(parameters)), replacement, percent);
    }

    private static Object[] convertParams(final Object[] parameters) {
        Object[] convertedParams = ArrayUtils.clone(parameters);
        for (int i = 0; i < convertedParams.length; i++) {
            if (convertedParams[i] != null) {
                if (Calendar.class.isAssignableFrom(convertedParams[i].getClass())) {
                    convertedParams[i] = ((Calendar) parameters[i]).getTime();
                } else if (Class.class.isAssignableFrom(convertedParams[i].getClass())) {
                    convertedParams[i] = ((Class<?>) parameters[i]).getSimpleName();
                }
            }
        }
        return convertedParams;
    }
}
