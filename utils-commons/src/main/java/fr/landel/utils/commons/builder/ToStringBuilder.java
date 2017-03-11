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
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import org.apache.commons.lang3.builder.Builder;

import fr.landel.utils.commons.Default;
import fr.landel.utils.commons.ObjectUtils;
import fr.landel.utils.commons.Result;

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
     * @param style
     *            the style to apply
     */
    public ToStringBuilder(final ToStringStyles style) {
        this("", style);
    }

    /**
     * Constructor
     */
    public ToStringBuilder() {
        this("", (ToStringStyles) null);
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
        this(object, (ToStringStyles) null);
    }

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
    public ToStringBuilder(final Object object, final ToStringStyles style) {
        this(object, ObjectUtils.defaultIfNull(style, ToStringStyles.DEFAULT.getSupplier(), s -> s.getSupplier()));
    }

    /**
     * Constructor
     *
     * @param supplier
     *            the style supplier
     * @throws NullPointerException
     *             if object is {@code null}
     */
    public ToStringBuilder(final Supplier<? extends ToStringStyle> supplier) {
        this("", supplier);
    }

    /**
     * Constructor
     *
     * @param object
     *            the main object, class or title
     * @param supplier
     *            the style supplier
     * @throws NullPointerException
     *             if object is {@code null}
     */
    public ToStringBuilder(final Object object, final Supplier<? extends ToStringStyle> supplier) {
        this.style = Objects.requireNonNull(supplier, "ToStringStyle supplier cannot be null").get().setObject(object);
    }

    /**
     * Append the value to the builder
     * 
     * @param value
     *            the value to append
     * @return the {@link ToStringBuilder} instance
     */
    public ToStringBuilder append(final Object value) {
        this.style.append(value);
        return this;
    }

    /**
     * Append the pair key/value to the builder
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value to append
     * @return the {@link ToStringBuilder} instance
     */
    public ToStringBuilder append(final CharSequence key, final Object value) {
        this.style.append(key, value);
        return this;
    }

    /**
     * Append the value to the builder
     * 
     * @param value
     *            the value to append
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendIf(final T value, final Predicate<T> predicate) {
        this.style.append(value, predicate);
        return this;
    }

    /**
     * Append the pair key/value to the builder
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value to append
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendIf(final CharSequence key, final T value, final Predicate<T> predicate) {
        this.style.append(key, value, predicate);
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
    public <T> ToStringBuilder appendAndFormat(final T value, final Function<T, CharSequence> formatter) {
        this.style.append(value, formatter);
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
    public <T> ToStringBuilder appendAndFormat(final CharSequence key, final T value, final Function<T, CharSequence> formatter) {
        this.style.append(key, value, formatter);
        return this;
    }

    /**
     * Append the value to the builder
     * 
     * @param value
     *            the value to append
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param formatter
     *            the formatter used to format the object before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendAndFormatIf(final T value, final Predicate<T> predicate, final Function<T, CharSequence> formatter) {
        this.style.append(value, predicate, formatter);
        return this;
    }

    /**
     * Append the pair key/value to the builder
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value to append
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the object type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendAndFormatIf(final CharSequence key, final T value, final Predicate<T> predicate,
            final Function<T, CharSequence> formatter) {
        this.style.append(key, value, predicate, formatter);
        return this;
    }

    /**
     * Append the value to the builder only if value is not {@code null}.
     * 
     * @param value
     *            the value
     * @return the {@link ToStringBuilder} instance
     */
    public ToStringBuilder appendIfNotNull(final Object value) {
        this.style.appendIfNotNull(value);
        return this;
    }

    /**
     * Append the pair key/value to the builder only if value is not
     * {@code null}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value
     * @return the {@link ToStringBuilder} instance
     */
    public ToStringBuilder appendIfNotNull(final CharSequence key, final Object value) {
        this.style.appendIfNotNull(key, value);
        return this;
    }

    /**
     * Append the value to the builder only if value is not {@code null}.
     * 
     * @param value
     *            the value
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendIfNotNullIf(final T value, final Predicate<T> predicate) {
        this.style.appendIfNotNull(value, predicate);
        return this;
    }

    /**
     * Append the pair key/value to the builder only if value is not
     * {@code null}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendIfNotNullIf(final CharSequence key, final T value, final Predicate<T> predicate) {
        this.style.appendIfNotNull(key, value, predicate);
        return this;
    }

    /**
     * Append the value to the builder only if value is not {@code null}.
     * 
     * @param value
     *            the value
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendAndFormatIfNotNull(final T value, final Function<T, CharSequence> formatter) {
        this.style.appendIfNotNull(value, formatter);
        return this;
    }

    /**
     * Append the pair key/value to the builder only if value is not
     * {@code null}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendAndFormatIfNotNull(final CharSequence key, final T value, final Function<T, CharSequence> formatter) {
        this.style.appendIfNotNull(key, value, formatter);
        return this;
    }

    /**
     * Append the value to the builder only if value is not {@code null}.
     * 
     * @param value
     *            the value
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendAndFormatIfNotNullIf(final T value, final Predicate<T> predicate,
            final Function<T, CharSequence> formatter) {
        this.style.appendIfNotNull(value, predicate, formatter);
        return this;
    }

    /**
     * Append the pair key/value to the builder only if value is not
     * {@code null}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendAndFormatIfNotNullIf(final CharSequence key, final T value, final Predicate<T> predicate,
            final Function<T, CharSequence> formatter) {
        this.style.appendIfNotNull(key, value, predicate, formatter);
        return this;
    }

    /**
     * Append the value if present, otherwise the default value to the builder.
     * See {@link Default}.
     * 
     * @param value
     *            the value
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendDefault(final Default<T> value) {
        this.style.appendDefault(value);
        return this;
    }

    /**
     * Append the pair key/value if present, otherwise the pair key/default
     * value to the builder. See {@link Default}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendDefault(final CharSequence key, final Default<T> value) {
        this.style.appendDefault(key, value);
        return this;
    }

    /**
     * Append the value if present, otherwise the default value to the builder.
     * See {@link Default}.
     * 
     * @param value
     *            the value
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendDefaultIf(final Default<T> value, final Predicate<T> predicate) {
        this.style.appendDefault(value, predicate);
        return this;
    }

    /**
     * Append the pair key/value if present, otherwise the pair key/default
     * value to the builder. See {@link Default}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendDefaultIf(final CharSequence key, final Default<T> value, final Predicate<T> predicate) {
        this.style.appendDefault(key, value, predicate);
        return this;
    }

    /**
     * Append the value if present, otherwise the default value to the builder.
     * See {@link Default}.
     * 
     * @param value
     *            the value
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendAndFormatDefault(final Default<T> value, final Function<T, CharSequence> formatter) {
        this.style.appendDefault(value, formatter);
        return this;
    }

    /**
     * Append the pair key/value if present, otherwise the pair key/default
     * value to the builder. See {@link Default}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendAndFormatDefault(final CharSequence key, final Default<T> value,
            final Function<T, CharSequence> formatter) {
        this.style.appendDefault(key, value, formatter);
        return this;
    }

    /**
     * Append the value if present, otherwise the default value to the builder.
     * See {@link Default}.
     * 
     * @param value
     *            the value
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendAndFormatDefaultIf(final Default<T> value, final Predicate<T> predicate,
            final Function<T, CharSequence> formatter) {
        this.style.appendDefault(value, predicate, formatter);
        return this;
    }

    /**
     * Append the pair key/value if present, otherwise the pair key/default
     * value to the builder. See {@link Default}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendAndFormatDefaultIf(final CharSequence key, final Default<T> value, final Predicate<T> predicate,
            final Function<T, CharSequence> formatter) {
        this.style.appendDefault(key, value, predicate, formatter);
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
    public <T> ToStringBuilder appendIfPresent(final Optional<T> value) {
        this.style.appendIfPresent(value);
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
     * Append the value to the builder only if value is present. See
     * {@link Result}.
     * 
     * @param value
     *            the optional value
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendIfPresent(final Result<T> value) {
        this.style.appendIfPresent(value);
        return this;
    }

    /**
     * Append the pair key/value to the builder only if value is present. See
     * {@link Result}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the optional value
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendIfPresent(final CharSequence key, final Result<T> value) {
        this.style.appendIfPresent(key, value);
        return this;
    }

    /**
     * Append the value to the builder only if value is present. See
     * {@link Optional}.
     * 
     * @param value
     *            the optional value
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendIfPresentIf(final Optional<T> value, final Predicate<T> predicate) {
        this.style.appendIfPresent(value, predicate);
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
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendIfPresentIf(final CharSequence key, final Optional<T> value, final Predicate<T> predicate) {
        this.style.appendIfPresent(key, value, predicate);
        return this;
    }

    /**
     * Append the value to the builder only if value is present. See
     * {@link Result}.
     * 
     * @param value
     *            the optional value
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendIfPresentIf(final Result<T> value, final Predicate<T> predicate) {
        this.style.appendIfPresent(value, predicate);
        return this;
    }

    /**
     * Append the pair key/value to the builder only if value is present. See
     * {@link Result}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the optional value
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendIfPresentIf(final CharSequence key, final Result<T> value, final Predicate<T> predicate) {
        this.style.appendIfPresent(key, value, predicate);
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
    public <T> ToStringBuilder appendAndFormatIfPresent(final Optional<T> value, final Function<T, CharSequence> formatter) {
        this.style.appendIfPresent(value, formatter);
        return this;
    }

    /**
     * Append the value to the builder only if value is present. See
     * {@link Result}.
     * 
     * @param value
     *            the optional value
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendAndFormatIfPresent(final Result<T> value, final Function<T, CharSequence> formatter) {
        this.style.appendIfPresent(value, formatter);
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
    public <T> ToStringBuilder appendAndFormatIfPresent(final CharSequence key, final Optional<T> value,
            final Function<T, CharSequence> formatter) {
        this.style.appendIfPresent(key, value, formatter);
        return this;
    }

    /**
     * Append the pair key/value to the builder only if value is present. See
     * {@link Result}.
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
    public <T> ToStringBuilder appendAndFormatIfPresent(final CharSequence key, final Result<T> value,
            final Function<T, CharSequence> formatter) {
        this.style.appendIfPresent(key, value, formatter);
        return this;
    }

    /**
     * Append the value to the builder only if value is present. See
     * {@link Optional}.
     * 
     * @param value
     *            the optional value
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendAndFormatIfPresentIf(final Optional<T> value, final Predicate<T> predicate,
            final Function<T, CharSequence> formatter) {
        this.style.appendIfPresent(value, predicate, formatter);
        return this;
    }

    /**
     * Append the value to the builder only if value is present. See
     * {@link Result}.
     * 
     * @param value
     *            the optional value
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendAndFormatIfPresentIf(final Result<T> value, final Predicate<T> predicate,
            final Function<T, CharSequence> formatter) {
        this.style.appendIfPresent(value, predicate, formatter);
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
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendAndFormatIfPresentIf(final CharSequence key, final Optional<T> value, final Predicate<T> predicate,
            final Function<T, CharSequence> formatter) {
        this.style.appendIfPresent(key, value, predicate, formatter);
        return this;
    }

    /**
     * Append the pair key/value to the builder only if value is present. See
     * {@link Result}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the optional value
     * @param predicate
     *            to check if the value has to be appended (ignored if
     *            {@code null})
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     * @return the {@link ToStringBuilder} instance
     */
    public <T> ToStringBuilder appendAndFormatIfPresentIf(final CharSequence key, final Result<T> value, final Predicate<T> predicate,
            final Function<T, CharSequence> formatter) {
        this.style.appendIfPresent(key, value, predicate, formatter);
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