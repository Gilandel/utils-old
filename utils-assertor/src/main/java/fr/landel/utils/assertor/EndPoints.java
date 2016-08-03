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

import java.util.Locale;
import java.util.function.BiFunction;

import org.apache.commons.lang3.ArrayUtils;

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
public class EndPoints<A extends AbstractAssertObject<A, T>, T> extends AssertorConstants {

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
     * Throws the specified exception on assertions mismatch. The internal
     * exception is appended to the specified one as suppressed. The current
     * assertor is cleared.
     * 
     * @param exceptionBuilder
     *            The exception builder to throw on mismatch (receive two
     *            parameters: the errors message and the parameters)
     * @param <E>
     *            The exception type
     * @throws E
     *             The thrown exception
     * @throws IllegalArgumentException
     *             On assertions mismatch
     */
    public <E extends Throwable> void toThrow(final BiFunction<CharSequence, Object[], E> exceptionBuilder) throws E {
        this.toThrow(true, exceptionBuilder);
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
        this.toThrow(null, null, locale, message, arguments, true);
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
        this.toThrow(exception, null, null, null, null, reset);
    }

    /**
     * Throws the specified exception on assertions mismatch. The internal
     * exception is appended to the specified one as suppressed.
     * 
     * @param reset
     *            if true, the current assertor is cleared
     * @param exceptionBuilder
     *            The exception builder to throw on mismatch (receive two
     *            parameters: the errors message and the parameters)
     * @param <E>
     *            The exception type
     * @throws E
     *             The thrown exception
     * @throws IllegalArgumentException
     *             On assertions mismatch
     */
    public <E extends Throwable> void toThrow(final boolean reset, final BiFunction<CharSequence, Object[], E> exceptionBuilder) throws E {
        this.toThrow(null, exceptionBuilder, null, null, null, reset);
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
        this.toThrow(null, null, locale, message, arguments, reset);
    }

    private <E extends Throwable> void toThrow(final E exception, final BiFunction<CharSequence, Object[], E> exceptionBuilder,
            final Locale locale, final CharSequence message, final Object[] arguments, final boolean reset) throws E {
        boolean valid = this.assertor.isValid();
        if (!valid) {
            CharSequence errors;
            Object[] parameters;
            if (reset) {
                errors = this.assertor.getMessage();
                parameters = ArrayUtils.clone(this.assertor.getParameters());
                this.assertor.clear();
            } else {
                errors = this.assertor.getMessage();
                parameters = this.assertor.getParameters();
            }

            if (exception != null) {
                AssertorHelper.manageExceptions(DEFAULT_ASSERTION, exception, null, null, null, null);
            } else if (exceptionBuilder != null) {
                AssertorHelper.manageExceptions(DEFAULT_ASSERTION, exceptionBuilder.apply(errors, parameters), null, null, null, null);
            } else {
                AssertorHelper.manageExceptions(DEFAULT_ASSERTION, null, locale, message, parameters, arguments);
            }
        } else if (reset) {
            this.assertor.clear();
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
        final String errors = AssertorHelper.getMessage(DEFAULT_ASSERTION, null, this.assertor.getMessage().toString(),
                ArrayUtils.clone(this.assertor.getParameters()), null);
        if (reset) {
            this.assertor.clear();
        }
        return errors;
    }
}