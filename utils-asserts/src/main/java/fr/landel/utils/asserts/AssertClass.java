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

/**
 * Assertion utility class that assists in validating arguments for numbers.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 * @param <X>
 *            the class type
 */
public class AssertClass<X> extends AssertObject<AssertClass<X>, Class<X>> {

    /**
     * 
     * Constructor
     *
     * @param clazz
     *            The class to check
     */
    protected AssertClass(final Class<X> clazz) {
        super(clazz);
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre>
     * Assert.isAssignable(Number.class, myClass);
     * </pre>
     * 
     * @param type
     *            the type to check
     * @return this
     * @throws IllegalArgumentException
     *             if the classes are not assignable
     */
    public AssertClass<X> isAssignable(final Class<?> type) {
        return this.isAssignable(type, (CharSequence) null);
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre>
     * Assert.isAssignable(Number.class, myClass);
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format) message looks
     *            OK when appended to it.
     * @return this
     * @throws IllegalArgumentException
     *             if the classes are not assignable
     */
    public AssertClass<X> isAssignable(final Class<?> type, final CharSequence message, final Object... arguments) {
        isAssignable(type, this.get(), null, message, arguments);

        return this;
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre>
     * Assert.isAssignable(Number.class, myClass), exceptionToThrowOnError);
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @return this
     * @throws E
     *             if the classes are not assignable. The standard exception is
     *             appended as suppressed.
     */
    public <E extends Throwable> AssertClass<X> isAssignable(final Class<?> type, final E exception) throws E {
        isAssignable(type, this.get(), exception, null);

        return this;
    }

    protected static <E extends Throwable> void isAssignable(final Class<?> type, final Class<?> superType, final E exception,
            final CharSequence message, final Object... arguments) throws E {
        isNotNull(type, null, "Type must not be null");
        isNotNull(superType, null, "Super type must not be null");
        if (superType == null || !type.isAssignableFrom(superType)) {
            manageExceptions(superType + " is not assignable to " + type, exception, message, new Object[] {type, superType}, arguments);
        }
    }
}