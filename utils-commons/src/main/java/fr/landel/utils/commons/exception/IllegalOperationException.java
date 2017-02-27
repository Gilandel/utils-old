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
package fr.landel.utils.commons.exception;

import java.util.Locale;

/**
 * Illegal operation exception, extends {@link RuntimeException}.
 *
 * @since Nov 27, 2015
 * @author Gilles Landel
 *
 */
public class IllegalOperationException extends AbstractRuntimeException {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 5900016009511270329L;

    /**
     * Constructor
     */
    public IllegalOperationException() {
        super();
    }

    /**
     * Constructor with message. To format the message, this method uses
     * {@link String#format} function.
     * 
     * @param message
     *            message
     * @param arguments
     *            message arguments
     */
    public IllegalOperationException(final CharSequence message, final Object... arguments) {
        super(message, arguments);
    }

    /**
     * Constructor with message. To format the message, this method uses
     * {@link String#format} function.
     * 
     * @param locale
     *            the message locale
     * @param message
     *            the message
     * @param arguments
     *            the message arguments
     */
    public IllegalOperationException(final Locale locale, final CharSequence message, final Object... arguments) {
        super(locale, message, arguments);
    }

    /**
     * Constructor.
     *
     * @param cause
     *            The cause
     */
    public IllegalOperationException(final Throwable cause) {
        super(cause);
    }

    /**
     * Constructor with class and exception.
     * 
     * @param clazz
     *            The super class
     * @param exception
     *            The exception
     */
    public IllegalOperationException(final Class<? extends IllegalOperationException> clazz, final Throwable exception) {
        super(clazz, exception);
    }

    /**
     * Constructor with message and exception.
     * 
     * @param message
     *            The message
     * @param exception
     *            The exception
     */
    public IllegalOperationException(final CharSequence message, final Throwable exception) {
        super(message, exception);
    }

    /**
     * Constructor with message and exception. To format the message, this
     * method uses {@link String#format} function.
     * 
     * @param exception
     *            The exception
     * @param message
     *            The message
     * @param arguments
     *            message arguments
     */
    public IllegalOperationException(final Throwable exception, final CharSequence message, final Object... arguments) {
        super(exception, message, arguments);
    }

    /**
     * Constructor with message and exception. To format the message, this
     * method uses {@link String#format} function.
     * 
     * @param exception
     *            The exception
     * @param locale
     *            message locale
     * @param message
     *            The message
     * @param arguments
     *            message arguments
     */
    public IllegalOperationException(final Throwable exception, final Locale locale, final CharSequence message,
            final Object... arguments) {
        super(exception, locale, message, arguments);
    }

    /**
     * Constructor.
     *
     * @param message
     *            the message
     * @param cause
     *            the cause
     * @param enableSuppression
     *            whether or not suppression is enabled or disabled
     * @param writableStackTrace
     *            whether or not the stack trace should be writable
     */
    protected IllegalOperationException(final CharSequence message, final Throwable cause, final boolean enableSuppression,
            final boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

    /**
     * Constructor. To format the message, this method uses
     * {@link String#format} function.
     *
     * @param cause
     *            the cause
     * @param enableSuppression
     *            whether or not suppression is enabled or disabled
     * @param writableStackTrace
     *            whether or not the stack trace should be writable
     * @param message
     *            the message
     * @param arguments
     *            the message arguments
     */
    protected IllegalOperationException(final Throwable cause, final boolean enableSuppression, final boolean writableStackTrace,
            final CharSequence message, final Object... arguments) {
        super(cause, enableSuppression, writableStackTrace, message, arguments);
    }

    /**
     * Constructor. To format the message, this method uses
     * {@link String#format} function.
     *
     * @param cause
     *            the cause
     * @param enableSuppression
     *            whether or not suppression is enabled or disabled
     * @param writableStackTrace
     *            whether or not the stack trace should be writable
     * @param locale
     *            the message locale
     * @param message
     *            the message
     * @param arguments
     *            the message arguments
     */
    protected IllegalOperationException(final Throwable cause, final boolean enableSuppression, final boolean writableStackTrace,
            final Locale locale, final CharSequence message, final Object... arguments) {
        super(cause, enableSuppression, writableStackTrace, locale, message, arguments);
    }
}
