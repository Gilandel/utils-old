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

import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;

import fr.landel.utils.commons.Default;
import fr.landel.utils.commons.Result;

/**
 * ToString style
 *
 * @since Mar 5, 2017
 * @author Gilles
 *
 */
public interface ToStringStyle {

    /**
     * Define the main object, the class or a title
     * 
     * @param object
     *            the object
     * @return the current instance
     * @throws NullPointerException
     *             if object is {@code null}
     */
    ToStringStyle setObject(Object object);

    /**
     * Append the value to the builder
     * 
     * @param value
     *            the value to append
     */
    void append(Object value);

    /**
     * Append the pair key/value to the builder
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value to append
     */
    void append(CharSequence key, Object value);

    /**
     * Append the value to the builder
     * 
     * @param value
     *            the value to append
     * @param formatter
     *            the formatter used to format the object before appending
     * @param <T>
     *            the value type
     */
    <T> void append(T value, Function<T, CharSequence> formatter);

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
     */
    <T> void append(CharSequence key, T value, Function<T, CharSequence> formatter);

    /**
     * Append the value to the builder
     * 
     * @param value
     *            the value to append
     * @param predicate
     *            to check if the value has to be appended
     * @param <T>
     *            the value type
     */
    <T> void append(T value, Predicate<T> predicate);

    /**
     * Append the pair key/value to the builder
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value to append
     * @param predicate
     *            to check if the value has to be appended
     * @param <T>
     *            the value type
     */
    <T> void append(CharSequence key, T value, Predicate<T> predicate);

    /**
     * Append the value to the builder
     * 
     * @param value
     *            the value to append
     * @param predicate
     *            to check if the value has to be appended
     * @param formatter
     *            the formatter used to format the object before appending
     * @param <T>
     *            the value type
     */
    <T> void append(T value, Predicate<T> predicate, Function<T, CharSequence> formatter);

    /**
     * Append the pair key/value to the builder
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value to append
     * @param predicate
     *            to check if the value has to be appended
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the object type
     */
    <T> void append(CharSequence key, T value, Predicate<T> predicate, Function<T, CharSequence> formatter);

    /**
     * Append the value to the builder only if value is not {@code null}.
     * 
     * @param value
     *            the value
     */
    void appendIfNotNull(Object value);

    /**
     * Append the pair key/value to the builder only if value is not
     * {@code null}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value
     */
    void appendIfNotNull(CharSequence key, Object value);

    /**
     * Append the value to the builder only if value is not {@code null}.
     * 
     * @param value
     *            the value
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     */
    <T> void appendIfNotNull(T value, Function<T, CharSequence> formatter);

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
     */
    <T> void appendIfNotNull(CharSequence key, T value, Function<T, CharSequence> formatter);

    /**
     * Append the value to the builder only if value is not {@code null}.
     * 
     * @param value
     *            the value
     * @param predicate
     *            to check if the value has to be appended
     * @param <T>
     *            the value type
     */
    <T> void appendIfNotNull(T value, Predicate<T> predicate);

    /**
     * Append the pair key/value to the builder only if value is not
     * {@code null}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value
     * @param predicate
     *            to check if the value has to be appended
     * @param <T>
     *            the value type
     */
    <T> void appendIfNotNull(CharSequence key, T value, Predicate<T> predicate);

    /**
     * Append the value to the builder only if value is not {@code null}.
     * 
     * @param value
     *            the value
     * @param predicate
     *            to check if the value has to be appended
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     */
    <T> void appendIfNotNull(T value, Predicate<T> predicate, Function<T, CharSequence> formatter);

    /**
     * Append the pair key/value to the builder only if value is not
     * {@code null}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value
     * @param predicate
     *            to check if the value has to be appended
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     */
    <T> void appendIfNotNull(CharSequence key, T value, Predicate<T> predicate, Function<T, CharSequence> formatter);

    /**
     * Append the value if present, otherwise the default value to the builder.
     * See {@link Default}.
     * 
     * @param value
     *            the value
     * @param <T>
     *            the value type
     */
    <T> void appendDefault(Default<T> value);

    /**
     * Append the pair key/value, otherwise the pair key/default value to the
     * builder. See {@link Default}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value
     * @param <T>
     *            the value type
     */
    <T> void appendDefault(CharSequence key, Default<T> value);

    /**
     * Append the value, otherwise the default value to the builder. See
     * {@link Default}.
     * 
     * @param value
     *            the value
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     */
    <T> void appendDefault(Default<T> value, Function<T, CharSequence> formatter);

    /**
     * Append the pair key/value, otherwise the pair key/default value to the
     * builder. See {@link Default}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     */
    <T> void appendDefault(CharSequence key, Default<T> value, Function<T, CharSequence> formatter);

    /**
     * Append the value if present, otherwise the default value to the builder.
     * See {@link Default}.
     * 
     * @param value
     *            the value
     * @param predicate
     *            to check if the value has to be appended
     * @param <T>
     *            the value type
     */
    <T> void appendDefault(Default<T> value, Predicate<T> predicate);

    /**
     * Append the pair key/value, otherwise the pair key/default value to the
     * builder. See {@link Default}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value
     * @param predicate
     *            to check if the value has to be appended
     * @param <T>
     *            the value type
     */
    <T> void appendDefault(CharSequence key, Default<T> value, Predicate<T> predicate);

    /**
     * Append the value, otherwise the default value to the builder. See
     * {@link Default}.
     * 
     * @param value
     *            the value
     * @param predicate
     *            to check if the value has to be appended
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     */
    <T> void appendDefault(Default<T> value, Predicate<T> predicate, Function<T, CharSequence> formatter);

    /**
     * Append the pair key/value, otherwise the pair key/default value to the
     * builder. See {@link Default}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value
     * @param predicate
     *            to check if the value has to be appended
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     */
    <T> void appendDefault(CharSequence key, Default<T> value, Predicate<T> predicate, Function<T, CharSequence> formatter);

    /**
     * Append the value to the builder only if value is present. See
     * {@link Optional}.
     * 
     * @param value
     *            the optional value
     * @param <T>
     *            the value type
     */
    <T> void appendIfPresent(Optional<T> value);

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
     */
    <T> void appendIfPresent(CharSequence key, Optional<T> value);

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
     */
    <T> void appendIfPresent(Optional<T> value, Function<T, CharSequence> formatter);

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
     */
    <T> void appendIfPresent(CharSequence key, Optional<T> value, Function<T, CharSequence> formatter);

    /**
     * Append the value to the builder only if value is present. See
     * {@link Optional}.
     * 
     * @param value
     *            the optional value
     * @param predicate
     *            to check if the value has to be appended
     * @param <T>
     *            the value type
     */
    <T> void appendIfPresent(Optional<T> value, Predicate<T> predicate);

    /**
     * Append the pair key/value to the builder only if value is present. See
     * {@link Optional}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the optional value
     * @param predicate
     *            to check if the value has to be appended
     * @param <T>
     *            the value type
     */
    <T> void appendIfPresent(CharSequence key, Optional<T> value, Predicate<T> predicate);

    /**
     * Append the value to the builder only if value is present. See
     * {@link Optional}.
     * 
     * @param value
     *            the optional value
     * @param predicate
     *            to check if the value has to be appended
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     */
    <T> void appendIfPresent(Optional<T> value, Predicate<T> predicate, Function<T, CharSequence> formatter);

    /**
     * Append the pair key/value to the builder only if value is present. See
     * {@link Optional}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the optional value
     * @param predicate
     *            to check if the value has to be appended
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     */
    <T> void appendIfPresent(CharSequence key, Optional<T> value, Predicate<T> predicate, Function<T, CharSequence> formatter);

    /**
     * Append the value to the builder only if value is present. See
     * {@link Result}.
     * 
     * @param value
     *            the optional value
     * @param <T>
     *            the value type
     */
    <T> void appendIfPresent(Result<T> value);

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
     */
    <T> void appendIfPresent(CharSequence key, Result<T> value);

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
     */
    <T> void appendIfPresent(Result<T> value, Function<T, CharSequence> formatter);

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
     */
    <T> void appendIfPresent(CharSequence key, Result<T> value, Function<T, CharSequence> formatter);

    /**
     * Append the value to the builder only if value is present. See
     * {@link Result}.
     * 
     * @param value
     *            the optional value
     * @param predicate
     *            to check if the value has to be appended
     * @param <T>
     *            the value type
     */
    <T> void appendIfPresent(Result<T> value, Predicate<T> predicate);

    /**
     * Append the pair key/value to the builder only if value is present. See
     * {@link Result}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the optional value
     * @param predicate
     *            to check if the value has to be appended
     * @param <T>
     *            the value type
     */
    <T> void appendIfPresent(CharSequence key, Result<T> value, Predicate<T> predicate);

    /**
     * Append the value to the builder only if value is present. See
     * {@link Result}.
     * 
     * @param value
     *            the optional value
     * @param predicate
     *            to check if the value has to be appended
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     */
    <T> void appendIfPresent(Result<T> value, Predicate<T> predicate, Function<T, CharSequence> formatter);

    /**
     * Append the pair key/value to the builder only if value is present. See
     * {@link Result}.
     * 
     * @param key
     *            the key to append
     * @param value
     *            the optional value
     * @param predicate
     *            to check if the value has to be appended
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     */
    <T> void appendIfPresent(CharSequence key, Result<T> value, Predicate<T> predicate, Function<T, CharSequence> formatter);

    /**
     * Build the toString content
     * 
     * @return the builder result following the style
     */
    String build();
}
