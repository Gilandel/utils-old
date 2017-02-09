/*
 * #%L
 * utils-aop
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.aop.exception;

import fr.landel.utils.commons.exception.AbstractException;

/**
 * The OAP exception.
 *
 * @since Nov 27, 2015
 * @author Gilles
 *
 */
public class AOPException extends AbstractException {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -5088326000492567608L;

    /**
     * Constructor.
     * 
     */
    public AOPException() {
        super();
    }

    /**
     * Constructor with message.
     * 
     * @param message
     *            message
     */
    public AOPException(final String message) {
        super(message);
    }

    /**
     * Constructor with exception.
     * 
     * @param exception
     *            The exception
     */
    public AOPException(final Throwable exception) {
        super(AOPException.class, exception);
    }

    /**
     * Constructor with message and exception.
     * 
     * @param message
     *            The message
     * @param exception
     *            The exception
     */
    public AOPException(final String message, final Throwable exception) {
        super(message, exception);
    }
}
