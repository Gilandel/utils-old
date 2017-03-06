/*-
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
package fr.landel.utils.commons.builder;

import java.text.DecimalFormat;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Supplier;

import org.apache.commons.lang3.builder.Builder;

/**
 * ToString builder
 *
 * @since Mar 5, 2017
 * @author Gilles
 *
 */
public class ToStringBuilder implements Builder<String> {

    private static final DecimalFormat FORMATTER = new DecimalFormat("###,###,###,###,###,###.###");

    /**
     * A formatter for numbers (ex: 126123.1246 =&gt; "126 123.125")
     */
    public static final Function<? super Number, CharSequence> NUMBER_FORMATTER = number -> {
        return FORMATTER.format(number);
    };

    private final ToStringStyle style;

    /**
     * Constructor
     *
     * @param object
     *            the main object, class or title
     * @param style
     *            the style to apply
     * @throws NullPointerException
     *             if object is {@code null}
     */
    public ToStringBuilder(final Object object, final ToStringStyle style) {
        if (style != null) {
            this.style = style.setObject(object);
        } else {
            this.style = new ToStringStyleDefault().setObject(object);
        }
    }

    /**
     * Constructor
     *
     * @param object
     *            the main object, class or title
     * @throws NullPointerException
     *             if object is {@code null}
     */
    public ToStringBuilder(final Object object) {
        this(object, null);
    }

    /**
     * Append the value to the builder
     * 
     * @param value
     *            the value to append
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder append(final T object) {
        this.style.append(object);
        return this;
    }

    /**
     * Append the value to the builder
     * 
     * @param value
     *            the value to append
     * @param formatter
     *            the formatter used to format the object before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder append(final T object, final Function<T, CharSequence> formatter) {
        this.style.append(object, formatter);
        return this;
    }

    /**
     * Append the pair key/value to the builder
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value to append
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder append(final CharSequence key, final T value) {
        this.style.append(key, value);
        return this;
    }

    /**
     * Append the pair key/value to the builder
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value to append
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the object type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder append(final CharSequence key, final T value, final Function<T, CharSequence> formatter) {
        this.style.append(key, value, formatter);
        return this;
    }

    /**
     * Append the value to the builder
     * 
     * @param value
     *            the value supplier
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder append(final Supplier<T> object) {
        this.style.append(object);
        return this;
    }

    /**
     * Append the value to the builder
     * 
     * @param value
     *            the value supplier
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder append(final Supplier<T> object, final Function<T, CharSequence> formatter) {
        this.style.append(object, formatter);
        return this;
    }

    /**
     * Append the pair key/value to the builder
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value supplier
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder append(final CharSequence key, final Supplier<T> value) {
        this.style.append(key, value);
        return this;
    }

    /**
     * Append the pair key/value to the builder
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value supplier
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder append(final CharSequence key, final Supplier<T> value, final Function<T, CharSequence> formatter) {
        this.style.append(key, value, formatter);
        return this;
    }

    /**
     * Append the value to the builder only if value is present. See
     * {@link Optional}.
     * 
     * @param value
     *            the optional value
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendIfPresent(final Optional<T> object) {
        this.style.appendIfPresent(object);
        return this;
    }

    /**
     * Append the value to the builder only if value is present. See
     * {@link Optional}.
     * 
     * @param value
     *            the optional value
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendIfPresent(final Optional<T> object, final Function<T, CharSequence> formatter) {
        this.style.appendIfPresent(object, formatter);
        return this;
    }

    /**
     * Append the pair key/value to the builder only if value is present. See
     * {@link Optional}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the optional value
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendIfPresent(final CharSequence key, final Optional<T> value) {
        this.style.appendIfPresent(key, value);
        return this;
    }

    /**
     * Append the pair key/value to the builder only if value is present. See
     * {@link Optional}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the optional value
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendIfPresent(final CharSequence key, final Optional<T> value, final Function<T, CharSequence> formatter) {
        this.style.appendIfPresent(key, value, formatter);
        return this;
    }

    /**
     * Build the toString content
     * 
     * @return the builder result following the style
     */
    @Override
    public String build() {
        return this.style.build();
    }

    @Override
    public String toString() {
        return this.build();
    }
}