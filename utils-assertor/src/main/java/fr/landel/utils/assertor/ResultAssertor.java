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

import java.util.ArrayList;
import java.util.List;

/**
 * To store the assertor result
 *
 * @since Aug 10, 2016
 * @author Gilles
 *
 */
public class ResultAssertor {

    private final boolean precondition;
    private final boolean valid;
    private final String message;
    private final List<Parameter<?>> parameters;

    /**
     * Constructor
     *
     * @param precondition
     *            the precondition status
     * @param valid
     *            the validity status
     * @param message
     *            the result message
     * @param parameters
     *            the complete list of parameters
     */
    public ResultAssertor(boolean precondition, boolean valid, String message, final List<Parameter<?>> parameters) {
        this.precondition = precondition;
        this.valid = valid;
        this.message = message;
        this.parameters = new ArrayList<>(parameters);
    }

    /**
     * @return the precondition
     */
    public boolean isPrecondition() {
        return this.precondition;
    }

    /**
     * @return the valid
     */
    public boolean isValid() {
        return this.valid;
    }

    /**
     * @return the message
     */
    public String getMessage() {
        return this.message;
    }

    /**
     * @return the parameters
     */
    public List<Parameter<?>> getParameters() {
        return this.parameters;
    }
}