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
     * Constructor with message.
     * 
     * @param message
     *            message
     */
    public AbstractException(final CharSequence message) {
        super(message.toString());
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
     * Constructor with message and exception.
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
     * 
     * Constructor.
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
}
