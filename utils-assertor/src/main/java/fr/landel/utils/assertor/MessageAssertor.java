/*-
 * #%L
 * utils-assertor
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.assertor;

import java.util.Locale;

import fr.landel.utils.commons.ArrayUtils;
import fr.landel.utils.commons.StringUtils;

/**
 * Message DTO
 *
 * @since Aug 10, 2016
 * @author Gilles
 *
 */
public class MessageAssertor {

    private final Locale locale;
    private final CharSequence message;
    private final Object[] arguments;

    /**
     * Message constructor
     *
     * @param locale
     *            the message locale
     * @param message
     *            the message
     * @param arguments
     *            the message arguments
     */
    private MessageAssertor(final Locale locale, final CharSequence message, final Object[] arguments) {

        this.locale = locale;
        this.message = message;
        this.arguments = ArrayUtils.clone(arguments);
    }

    /**
     * @return the locale
     */
    public Locale getLocale() {
        return this.locale;
    }

    /**
     * @return the message
     */
    public CharSequence getMessage() {
        return this.message;
    }

    /**
     * @return the arguments clone
     */
    public Object[] getArguments() {
        return ArrayUtils.clone(this.arguments);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("[");
        if (this.locale != null) {
            sb.append("locale: ").append(this.locale);
        }
        if (this.message != null) {
            if (this.locale != null) {
                sb.append(StringUtils.SEPARATOR_COMMA);
            }
            sb.append("message: ").append(this.message);
        }
        if (ArrayUtils.isNotEmpty(this.arguments)) {
            if (this.locale != null || this.message != null) {
                sb.append(StringUtils.SEPARATOR_COMMA);
            }
            sb.append("arguments: ").append(StringUtils.join(this.arguments, StringUtils.SEPARATOR_COMMA));
        }
        return sb.append("]").toString();
    }

    /**
     * Creates an immutable message
     * 
     * @param locale
     *            the message locale
     * @param message
     *            the message
     * @param arguments
     *            the message arguments
     * @return the new message instance
     */
    public static MessageAssertor of(final Locale locale, final CharSequence message, final Object[] arguments) {
        return new MessageAssertor(locale, message, arguments);
    }
}
