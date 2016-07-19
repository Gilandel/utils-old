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

import org.apache.commons.lang3.ArrayUtils;

/**
 * Assertion utility class that assists in validating arguments for arrays.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public class AssertArray<T> extends AssertObject<AssertArray<T>, T[]> {

    /**
     * 
     * Constructor
     *
     * @param array
     *            The array
     */
    protected AssertArray(final T[] array) {
        super(array);
    }

    /**
     * Assert that the array size is equal to the size
     * 
     * <pre>
     * Assertor.that(array).hasSize(size).toThrow(exception);
     * Assertor.that(array).hasSize(size).getResult();
     * </pre>
     * 
     * @param size
     *            The size
     * @return the operator
     */
    public Operator<AssertArray<T>, T[]> hasSize(final int size) {
        boolean condition = true;
        final StringBuilder message = new StringBuilder();
        if (size < 0) {
            condition = false;
            message.append("the size cannot be lower than 0");
        } else if (this.get() == null) {
            condition = false;
            message.append("the array is null");
        } else if (this.get().length != size) {
            condition = false;
            message.append("the array hasn't the expected size [").append(this.get().length).append(" != ").append(size).append("]");
        }
        return this.combine(condition, message, size);
    }

    /**
     * Assert that the array size is not equal to the size
     * 
     * <pre>
     * Assertor.that(array).hasNotSize(size).toThrow(exception);
     * Assertor.that(array).hasNotSize(size).getResult();
     * </pre>
     * 
     * @param size
     *            The size
     * @return the operator
     */
    public Operator<AssertArray<T>, T[]> hasNotSize(final int size) {
        boolean condition = true;
        final StringBuilder message = new StringBuilder();
        if (size < 0) {
            condition = false;
            message.append("the size cannot be lower than 0");
        } else if (this.get() == null) {
            condition = false;
            message.append("the array is null");
        } else if (this.get().length == size) {
            condition = false;
            message.append("the array hasn't the expected size [").append(this.get().length).append(" == ").append(size).append("]");
        }
        return this.combine(condition, message, size);
    }

    /**
     * Assert that an array has no elements; that is, it can be {@code null} or
     * must have no element.
     * 
     * <pre>
     * Assertor.that(array).isEmpty().toThrow(exception);
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertArray<T>, T[]> isEmpty() {
        return this.combine(ArrayUtils.isEmpty(this.get()), "this array must be empty and not null");
    }

    /**
     * Assert that an array has elements; that is, it must not be {@code null}
     * and must have at least one element.
     * 
     * <pre>
     * Assertor.that(array).isNotEmpty().toThrow(exception);
     * </pre>
     * 
     * @return this
     * @throws IllegalArgumentException
     *             if the object array is {@code null} or has no elements
     */
    public Operator<AssertArray<T>, T[]> isNotEmpty() {
        return this.combine(ArrayUtils.isNotEmpty(this.get()), "this array must not be empty: it must contain at least 1 element");
    }

    /**
     * Assert that an array has no null elements.
     * 
     * <pre>
     * Assertor.that(array).hasNoNullElements().toThrow(exception);
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertArray<T>, T[]> doesNotContainNull() {
        return this.combine(this.get() != null && !this.hasNullElements(), "this array must not contain any null elements");
    }

    /**
     * Assert that an array has null elements.
     * 
     * <pre>
     * Assertor.that(array).hasNullElements().toThrow(exception);
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertArray<T>, T[]> containsNull() {
        return this.combine(this.get() != null && this.hasNullElements(), "this array must contain at least null elements");
    }

    private boolean hasNullElements() {
        boolean nullFound = false;
        for (Object element : this.get()) {
            if (element == null) {
                nullFound = true;
                break;
            }
        }
        return nullFound;
    }

    /**
     * Assert that the array contains the object
     * 
     * <pre>
     * Assertor.that(array).contains(object).toThrow(exception);
     * </pre>
     * 
     * @param object
     *            The object
     * @return The operator
     */
    public Operator<AssertArray<T>, T[]> contains(final T object) {
        return this.combine(this.has(object), "this array must contain the object", object);
    }

    /**
     * Assert that the array contains all the objects
     * 
     * <pre>
     * Assertor.that(array1).contains(array2).toThrow(exception);
     * </pre>
     * 
     * @param array
     *            The array
     * @return The operator
     */
    public Operator<AssertArray<T>, T[]> contains(final T[] array) {
        return this.combine(this.has(array), "this array must contain all the objects", array);
    }

    /**
     * Assert that the array doesn't contain the object
     * 
     * <pre>
     * Assertor.that(array).doesNotContain(object).toThrow(exception);
     * </pre>
     * 
     * @param object
     *            The object
     * @return The operator
     */
    public Operator<AssertArray<T>, T[]> doesNotContain(final T object) {
        return this.combine(!this.has(object), new StringBuilder("this array must NOT contain the object '")
                .append(AssertObject.getParam(this.getParamIndex() + 1)).append("'"), object);
    }

    /**
     * Assert that the array doesn't contain any of the objects
     * 
     * <pre>
     * Assertor.that(array).doesNotContain(object).toThrow(exception);
     * </pre>
     * 
     * @param array
     *            The array
     * @return The operator
     */
    public Operator<AssertArray<T>, T[]> doesNotContain(final T[] array) {
        return this.combine(!this.has(array), new StringBuilder("this array must NOT contain any of the objects '")
                .append(AssertObject.getParam(this.getParamIndex() + 1)).append("'"), array);
    }

    private boolean has(final T object) {
        boolean found = false;
        if (this.get() != null) {
            if (object != null) {
                for (T objectArray : this.get()) {
                    if (object.equals(objectArray)) {
                        found = true;
                        break;
                    }
                }
            } else {
                for (T objectArray : this.get()) {
                    if (objectArray == null) {
                        found = true;
                        break;
                    }
                }
            }
        }
        return found;
    }

    private boolean has(final T[] array) {
        boolean foundAll = true;
        if (this.get() != null && array != null) {
            for (T objectArray : array) {
                if (!this.has(objectArray)) {
                    foundAll = false;
                    break;
                }
            }
        }
        return foundAll;
    }
}
