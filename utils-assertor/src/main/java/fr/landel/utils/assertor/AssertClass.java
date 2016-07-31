/*
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

/**
 * Assertion utility class that assists in validating arguments for numbers.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 * @param <T>
 *            the class type
 */
public class AssertClass<T> extends AssertObject<AssertClass<T>, Class<T>> {

    /**
     * 
     * Constructor
     *
     * @param clazz
     *            The class to check
     */
    protected AssertClass(final Class<T> clazz) {
        super(clazz, TYPE.CLASS);
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre>
     * Assertor.that(myClass).isAssignableFrom(Number.class).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param superType
     *            the type to check against
     * @return the operator
     */
    public Operator<AssertClass<T>, Class<T>> isAssignableFrom(final Class<?> superType) {
        return this.isAssignableFrom(superType, this.msg(MSG.CLASS.ASSIGNABLE, this.getParam(), this.getNextParam(1, TYPE.CLASS)));
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre>
     * Assertor.that(myClass).isAssignableFrom(Number.class).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param superType
     *            the type to check against
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertClass<T>, Class<T>> isAssignableFrom(final Class<?> superType, final CharSequence message,
            final Object... arguments) {
        return this.isAssignableFrom(superType, null, message, arguments);
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre>
     * Assertor.that(myClass).isAssignableFrom(Number.class).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param superType
     *            the type to check against
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertClass<T>, Class<T>> isAssignableFrom(final Class<?> superType, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(superType != null && this.get() != null, () -> superType.isAssignableFrom(this.get()),
                () -> this.msg(MSG.CLASS.ASSIGNABLE, true), message, arguments, locale, superType);
    }
}