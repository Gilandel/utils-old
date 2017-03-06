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
import java.util.function.Supplier;

/**
 * ToString style
 *
 * @since Mar 5, 2017
 * @author Gilles
 *
 */
public interface ToStringStyle {

    /**
     * The default toString style.
     * 
     * <pre>
     * test[java.awt.Color[r=0,g=0,b=0],0,blue=java.awt.Color[r=0,g=0,b=255],value=1 153 120 156,569,supplier,SUPPLIER,supplier=12,supplier=SUPPLIER,optional,OPTIONAL,optional=optional,optional=OPTIONAL]
     * </pre>
     */
    ToStringStyle DEFAULT = new ToStringStyleDefault();

    /**
     * The JSON toString style
     * 
     * <pre>
     * {"test":{"java.awt.Color[r=0,g=0,b=0]","0","blue":"java.awt.Color[r=0,g=0,b=255]","value":"1 153 120 156,569","supplier","SUPPLIER","supplier":"12","supplier":"SUPPLIER","optional","OPTIONAL","optional":"optional","optional":"OPTIONAL"}}
     * </pre>
     */
    ToStringStyle JSON = new ToStringStyleJSON();

    /**
     * The readable toString style
     * 
     * <pre>
     * test = 
     * ['java.awt.Color[r=0,g=0,b=0]',
     * '0',
     * 'blue' = 'java.awt.Color[r=0,g=0,b=255]',
     * 'value' = '1 153 120 156,569',
     * 'supplier',
     * 'SUPPLIER',
     * 'supplier' = '12',
     * 'supplier' = 'SUPPLIER',
     * 'optional',
     * 'OPTIONAL',
     * 'optional' = 'optional',
     * 'optional' = 'OPTIONAL']
     * </pre>
     */
    ToStringStyle READABLE = new ToStringStyleReadable();

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
     * @param <T>
     *            the value type
     */
    <T> void append(T value);

    /**
     * Append the pair key/value to the builder
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value to append
     * @param <T>
     *            the value type
     */
    <T> void append(CharSequence key, T value);

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
     *            the value supplier
     * @param <T>
     *            the value type
     */
    <T> void append(Supplier<T> value);

    /**
     * Append the pair key/value to the builder
     * 
     * @param key
     *            the key to append
     * @param value
     *            the value supplier
     * @param <T>
     *            the value type
     */
    <T> void append(CharSequence key, Supplier<T> value);

    /**
     * Append the value to the builder
     * 
     * @param value
     *            the value supplier
     * @param formatter
     *            the formatter used to format the value before appending
     * @param <T>
     *            the value type
     */
    <T> void append(Supplier<T> value, Function<T, CharSequence> formatter);

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
     */
    <T> void append(CharSequence key, Supplier<T> value, Function<T, CharSequence> formatter);

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
     * Build the toString content
     * 
     * @return the builder result following the style
     */
    String build();
}
