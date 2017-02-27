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

import org.apache.commons.lang3.ArrayUtils;

/**
 * Abstract exception.
 *
 * @since Nov 27, 2015
 * @author Gilles Landel
 *
 */
public abstract class AbstractException extends Exception {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 3181979638754437384L;

    /**
     * Constructor
     */
    public AbstractException() {
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
    public AbstractException(final CharSequence message, final Object... arguments) {
        super(ArrayUtils.isNotEmpty(arguments) ? String.format(message.toString(), arguments) : message.toString());
    }

    /**
     * Constructor with message. To format the message, this method uses
     * {@link String#format} function.
     * 
     * @param locale
     *            message locale
     * @param message
     *            message
     * @param arguments
     *            message arguments
     */
    public AbstractException(final Locale locale, final CharSequence message, final Object... arguments) {
        super(ArrayUtils.isNotEmpty(arguments) ? String.format(locale, message.toString(), arguments) : message.toString());
    }

    /**
     * Constructor.
     *
     * @param cause
     *            The cause
     */
    public AbstractException(final Throwable cause) {
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
    public AbstractException(final Class<? extends AbstractException> clazz, final Throwable exception) {
        super(clazz.getSimpleName(), exception);
    }

    /**
     * Constructor with message and exception. (inverse parameter order to keep
     * compatibility with standard signature)
     * 
     * @param message
     *            The message
     * @param exception
     *            The exception
     */
    public AbstractException(final CharSequence message, final Throwable exception) {
        super(message.toString(), exception);
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
    public AbstractException(final Throwable exception, final CharSequence message, final Object... arguments) {
        super(ArrayUtils.isNotEmpty(arguments) ? String.format(message.toString(), arguments) : message.toString(), exception);
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
    public AbstractException(final Throwable exception, final Locale locale, final CharSequence message, final Object... arguments) {
        super(ArrayUtils.isNotEmpty(arguments) ? String.format(locale, message.toString(), arguments) : message.toString(), exception);
    }

    /**
     * Constructor. (inverse parameter order to keep compatibility with standard
     * signature)
     *
     * @param message
     *            The message
     * @param cause
     *            the cause
     * @param enableSuppression
     *            whether or not suppression is enabled or disabled
     * @param writableStackTrace
     *            whether or not the stack trace should be writable
     */
    protected AbstractException(final CharSequence message, final Throwable cause, final boolean enableSuppression,
            final boolean writableStackTrace) {
        super(message.toString(), cause, enableSuppression, writableStackTrace);
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
     *            message
     * @param arguments
     *            message arguments
     */
    protected AbstractException(final Throwable cause, final boolean enableSuppression, final boolean writableStackTrace,
            final CharSequence message, final Object... arguments) {
        super(ArrayUtils.isNotEmpty(arguments) ? String.format(message.toString(), arguments) : message.toString(), cause,
                enableSuppression, writableStackTrace);
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
     *            message locale
     * @param message
     *            message
     * @param arguments
     *            message arguments
     */
    protected AbstractException(final Throwable cause, final boolean enableSuppression, final boolean writableStackTrace,
            final Locale locale, final CharSequence message, final Object... arguments) {
        super(ArrayUtils.isNotEmpty(arguments) ? String.format(locale, message.toString(), arguments) : message.toString(), cause,
                enableSuppression, writableStackTrace);
    }
}
