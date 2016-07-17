/*
 * #%L
 * utils-asserts
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.asserts;

import fr.landel.utils.commons.StringUtils;

/**
 * Assertion utility class that assists in validating arguments for strings.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public class AssertCharSequence<S extends CharSequence> extends AssertObject<AssertCharSequence<S>, S> {

    /**
     * 
     * Constructor
     *
     * @param object
     *            The object to check
     */
    protected AssertCharSequence(final S object) {
        super(object);
    }

    /**
     * Asserts that the given String has the specified length. The
     * {@code String} cannot not be {@code null}.
     * 
     * <pre>
     * Assertor.that(name).hasLength(5).toThrow();
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> hasLength(final int length) {
        boolean condition = true;
        final StringBuilder message = new StringBuilder();

        if (length < 0) {
            condition = false;
            message.append("the length parameter cannot be lower than 0");
        } else if (this.get() == null) {
            condition = false;
            message.append("the checked string is null");
        } else if (this.get().length() != length) {
            condition = false;
            message.append("this String argument '").append(this.getParam()).append("' don't have the specified length '").append(length)
                    .append("'");
        }

        return this.combine(condition, message, length);
    }

    /**
     * Asserts that the given String has not the specified length. The
     * {@code String} cannot not be {@code null}.
     * 
     * <pre>
     * Assertor.that(name).hasNotLength(5).toThrow();
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> hasNotLength(final int length) {
        boolean condition = true;
        final StringBuilder message = new StringBuilder();

        if (length < 0) {
            condition = false;
            message.append("the length parameter cannot be lower than 0");
        } else if (this.get() == null) {
            condition = false;
            message.append("the checked string is null");
        } else if (this.get().length() == length) {
            condition = false;
            message.append("this String argument '").append(this.getParam()).append("' have the specified length '").append(length)
                    .append("'");
        }

        return this.combine(condition, message, length);
    }

    /**
     * Asserts that the given {@code String} is not empty; that is, it must not
     * be {@code null} and not the empty {@code String}.
     * 
     * <pre>
     * Assertor.that(name).isNotEmpty().toThrow();
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> isNotEmpty() {
        return this.combine(StringUtils.isNotEmpty(this.get()), new StringBuilder("this String argument '").append(this.getParam())
                .append("' must have length; it must not be null or empty"));
    }

    /**
     * Asserts that the given {@code String} is {@code null} or empty.
     * 
     * <pre>
     * Assertor.that(name).isEmpty().toThrow(&quot;Name must not be empty&quot;);
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> isEmpty() {
        return this.combine(StringUtils.isEmpty(this.get()),
                new StringBuilder("this String argument '").append(this.getParam()).append("' must be null or empty"));
    }

    /**
     * Asserts that the given {@code String} has valid text content; that is, it
     * must not be {@code null} and must contain at least one non-whitespace
     * character.
     * 
     * <pre>
     * Assertor.that(name).isNotBlank().toThrow();
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> isNotBlank() {
        return this.combine(StringUtils.isNotBlank(this.get()), new StringBuilder("this String argument '").append(this.getParam())
                .append("' must have text; it must not be null, empty, or blank"));
    }

    /**
     * Asserts that the given {@code String} is {@code null}, empty or has blank
     * text content.
     * 
     * <pre>
     * Assertor.that(name).isBlank().toThrow();
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> isBlank() {
        return this.combine(StringUtils.isBlank(this.get()),
                new StringBuilder("this String argument '").append(this.getParam()).append("' must be null, empty or blank"));
    }

    /**
     * Asserts that the given text contains the given substring.
     * 
     * <pre>
     * Assertor.that(fullName).contains(name).toThrow();
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> contains(final String substring) {
        return this.combine(
                StringUtils.isNotEmpty(this.get()) && StringUtils.isNotEmpty(substring) && containsCharSequence(this.get(), substring),
                new StringBuilder("this String argument '").append(this.getParam()).append("' must contain the substring '")
                        .append(AssertObject.getParam(this.getParamIndex() + 1)).append("'"),
                substring);
    }

    /**
     * Asserts that the given text does not contain the given substring.
     * 
     * <pre>
     * Assertor.that(fullName).doesNotContain(name).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> doesNotContain(final String substring) {
        return this.combine(
                StringUtils.isNotEmpty(this.get()) && StringUtils.isNotEmpty(substring) && !containsCharSequence(this.get(), substring),
                new StringBuilder("this String argument '").append(this.getParam()).append("' must not contain the substring '")
                        .append(AssertObject.getParam(this.getParamIndex() + 1)).append("'"),
                substring);
    }

    /**
     * Searches in char sequence, if the specified sub sequence exists in.
     * {@code null} values have to be checked before.
     * 
     * @param textToSearch
     *            where to search
     * @param substring
     *            chat to search
     * @return {@code true} if found, {@code false} otherwise
     */
    private static boolean containsCharSequence(final CharSequence textToSearch, final CharSequence substring) {
        int p = 0;
        int l = substring.length();
        for (int i = 0; i < textToSearch.length() & p < l; i++) {
            if (textToSearch.charAt(i) == substring.charAt(p)) {
                p++;
            }
        }
        return p == l;
    }
}