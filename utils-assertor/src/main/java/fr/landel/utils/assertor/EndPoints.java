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
 * End points of assertion. Manages the result as {@code Boolean},
 * {@code String} or as {@code Throwable}
 *
 * @since 28 juil. 2016
 * @author Gilles
 * 
 * @param <A>
 *            The assertor type
 * @param <T>
 *            The type of checked object
 */
public class EndPoints<A extends AbstractAssertObject<A, T>, T> extends Constants {

    /**
     * {@link java.util.Formatter}
     * 
     * %[argument_index$][flags][width][.precision][t]conversion
     */
    private static final String FORMAT_SPECIFIER = "%(\\d+\\$)?([-#+ 0,(\\<]*)?(\\d+)?(\\.\\d+)?([tT])?([a-zA-Z%])";
    private static final Pattern PATTERN = Pattern.compile(FORMAT_SPECIFIER);
    private static final String PARAM_ID = "*";

    private final A assertor;

    /**
     * Constructor
     *
     * @param assertor
     *            The linked assertor
     */
    public EndPoints(final A assertor) {
        this.assertor = assertor;
    }

    /**
     * @return The assertor
     */
    protected A getAssertor() {
        return this.assertor;
    }

    /**
     * Throws an {@code IllegalArgumentException} on assertions mismatch. The
     * current assertor is cleaned.
     * 
     * @throws IllegalArgumentException
     *             On assertions mismatch
     */
    public void toThrow() {
        this.toThrow(true);
    }

    /**
     * Throws the specified exception on assertions mismatch. The internal
     * exception is appended to the specified one as suppressed. The current
     * assertor is cleared.
     * 
     * @param exception
     *            The exception to throw on mismatch
     * @param <E>
     *            The exception type
     * @throws E
     *             The thrown exception
     * @throws IllegalArgumentException
     *             On assertions mismatch
     */
    public <E extends Throwable> void toThrow(final E exception) throws E {
        this.toThrow(true, exception);
    }

    /**
     * Throws an {@code IllegalArgumentException} on assertions mismatch. Set
     * the exception with the specified message (override all other messages).
     * 
     * @param message
     *            The message to throw on mismatch
     * @param arguments
     *            The message arguments
     * @throws IllegalArgumentException
     *             On assertions mismatch
     */
    public void toThrow(final CharSequence message, final Object... arguments) {
        this.toThrow(null, message, arguments);
    }

    /**
     * Throws an {@code IllegalArgumentException} on assertions mismatch. Set
     * the exception with the specified message (override all other messages).
     * The current assertor is cleaned.
     * 
     * @param locale
     *            The message locale
     * @param message
     *            The message to throw on mismatch
     * @param arguments
     *            The message arguments
     * @throws IllegalArgumentException
     *             On assertions mismatch
     */
    public void toThrow(final Locale locale, final CharSequence message, final Object... arguments) {
        this.toThrow(null, locale, message, arguments, true);
    }

    /**
     * Throws an {@code IllegalArgumentException} on assertions mismatch.
     * 
     * @param reset
     *            if true, the current assertor is cleared
     * @throws IllegalArgumentException
     *             On assertions mismatch
     */
    public void toThrow(final boolean reset) {
        this.toThrow(reset, this.assertor.getMessage().toString());
    }

    /**
     * Throws the specified exception on assertions mismatch. The internal
     * exception is appended to the specified one as suppressed.
     * 
     * @param reset
     *            if true, the current assertor is cleared
     * @param exception
     *            The exception to throw on mismatch
     * @param <E>
     *            The exception type
     * @throws E
     *             The thrown exception
     * @throws IllegalArgumentException
     *             On assertions mismatch
     */
    public <E extends Throwable> void toThrow(final boolean reset, final E exception) throws E {
        this.toThrow(exception, null, null, null, reset);
    }

    /**
     * Throws an {@code IllegalArgumentException} on assertions mismatch. Set
     * the exception with the specified message (override all other messages).
     * 
     * @param reset
     *            if true, the current assertor is cleared
     *
     * @param message
     *            The message to throw on mismatch
     * @param arguments
     *            The message arguments
     * @throws IllegalArgumentException
     *             On assertions mismatch
     */
    public void toThrow(final boolean reset, final CharSequence message, final Object... arguments) {
        this.toThrow(reset, null, message, arguments);
    }

    /**
     * Throws an {@code IllegalArgumentException} on assertions mismatch. Set
     * the exception with the specified message (override all other messages).
     * 
     * @param reset
     *            if true, the current assertor is cleared
     * @param locale
     *            The message locale
     * @param message
     *            The message to throw on mismatch
     * @param arguments
     *            The message arguments
     * @throws IllegalArgumentException
     *             On assertions mismatch
     */
    public void toThrow(final boolean reset, final Locale locale, final CharSequence message, final Object... arguments) {
        this.toThrow(null, locale, message, arguments, reset);
    }

    private <E extends Throwable> void toThrow(final E exception, final Locale locale, final CharSequence message, final Object[] arguments,
            final boolean reset) throws E {
        boolean valid = this.assertor.isValid();
        Object[] parameters;
        if (reset) {
            parameters = ArrayUtils.clone(this.assertor.getParameters());
            this.assertor.clear();
        } else {
            parameters = this.assertor.getParameters();
        }
        if (!valid) {
            if (exception != null) {
                EndPoints.manageExceptions(DEFAULT_ASSERTION, exception, null, null, null, null);
            } else {
                EndPoints.manageExceptions(DEFAULT_ASSERTION, null, locale, message, parameters, arguments);
            }
        }
    }

    /**
     * Returns the result of the assertion combining. The current assertor is
     * cleared.
     * 
     * @return The boolean result
     */
    public boolean isOK() {
        return this.isOK(true);
    }

    /**
     * Returns the result of the assertion combining.
     * 
     * @param reset
     *            if true, the current assertor is cleared
     * @return The boolean result
     */
    public boolean isOK(final boolean reset) {
        boolean valid = this.assertor.isValid();
        if (reset) {
            this.assertor.clear();
        }
        return valid;
    }

    /**
     * Returns the message with intermediate errors. The current assertor is
     * cleared.
     * 
     * @return The message of errors
     */
    public String getErrors() {
        return this.getErrors(true);
    }

    /**
     * Returns the message with intermediate errors.
     * 
     * @param reset
     *            if true, the current assertor is cleared
     * @return The message of errors
     */
    public String getErrors(final boolean reset) {
        final String errors = EndPoints.getMessage(DEFAULT_ASSERTION, null, this.assertor.getMessage().toString(),
                ArrayUtils.clone(this.assertor.getParameters()), null);
        if (reset) {
            this.assertor.clear();
        }
        return errors;
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
            throw new IllegalArgumentException(EndPoints.getMessage(defaultString, locale, message, parameters, arguments));
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
    protected static String getMessage(final CharSequence defaultString, final Locale locale, final CharSequence message,
            final Object[] parameters, final Object[] arguments) {
        String msg;

        Locale l = locale;
        if (locale == null) {
            l = Assertor.getLocale();
        }

        if (StringUtils.isNotEmpty(message)) {
            msg = message.toString();
            if (ArrayUtils.isNotEmpty(parameters)) {
                msg = EndPoints.formatParameters(l, msg, parameters);
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
