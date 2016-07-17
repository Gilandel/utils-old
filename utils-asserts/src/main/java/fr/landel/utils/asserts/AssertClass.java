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
        super(clazz);
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
        return this.combine(
                isAssignable(superType, this.get()), new StringBuilder("the class '").append(this.getParam())
                        .append("' is not assignable from '").append(AssertObject.getParam(this.getParamIndex() + 1)).append("'"),
                superType);
    }

    /**
     * Check if the super type is assignable from the type
     * 
     * @param superType
     *            The super type
     * @param type
     *            The assignable type
     * @return true, if the superType is the same or a super type of type
     *         parameter
     */
    protected static boolean isAssignable(final Class<?> superType, final Class<?> type) {
        return superType != null && type != null && superType.isAssignableFrom(type);
    }
}