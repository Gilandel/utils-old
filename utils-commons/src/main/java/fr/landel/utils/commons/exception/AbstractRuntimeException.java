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
package fr.landel.utils.commons.exception;

/**
 * Abstract runtime exception.
 *
 * @since Nov 27, 2015
 * @author Gilles Landel
 *
 */
public abstract class AbstractRuntimeException extends RuntimeException {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 780825498990380553L;

    /**
     * Constructor
     */
    public AbstractRuntimeException() {
        super();
    }

    /**
     * Constructor with message.
     * 
     * @param message
     *            message
     */
    public AbstractRuntimeException(final CharSequence message) {
        super(message.toString());
    }

    /**
     * Constructor.
     *
     * @param cause
     *            The cause
     */
    public AbstractRuntimeException(final Throwable cause) {
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
    public AbstractRuntimeException(final Class<? extends AbstractRuntimeException> clazz, final Throwable exception) {
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
    public AbstractRuntimeException(final CharSequence message, final Throwable exception) {
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
    protected AbstractRuntimeException(final CharSequence message, final Throwable cause, final boolean enableSuppression,
            final boolean writableStackTrace) {
        super(message.toString(), cause, enableSuppression, writableStackTrace);
    }
}
