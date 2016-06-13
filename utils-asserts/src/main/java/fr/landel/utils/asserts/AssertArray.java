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

import java.util.Arrays;
import java.util.Comparator;

import org.apache.commons.lang3.ArrayUtils;

/**
 * Assertion utility class that assists in validating arguments for arrays.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public class AssertArray<A> extends AssertObject<AssertArray<A>, A[]> {

    protected AssertArray(final A[] object) {
        super(object);
    }

    public AssertArray<A> hasSize(final int size) {
        return this.hasSize(size, (String) null);
    }

    public AssertArray<A> hasSize(final int size, final String message, final Object... arguments) {
        hasSize(this.get(), size, null, message, arguments);

        return this;
    }

    public <E extends Throwable> AssertArray<A> hasSize(final int size, final E exception) throws E {
        hasSize(this.get(), size, exception, null);

        return this;
    }

    protected static <A, E extends Throwable> void hasSize(final A[] array, final int size, final E exception, final String message,
            final Object... arguments) throws E {

        AssertNumber.isGTE(size, 0, null, "The size parameter has to be greater than or equal to 0");

        if (array == null || array.length != size) {
            manageExceptions("the array hasn't the expected size", exception, message, new Object[] {array}, arguments);
        }
    }

    /**
     * Assert that an array has no elements; that is, it must not be
     * {@code null} and must have no element.
     * 
     * <pre>
     * AssertUtils.isEmpty(array);
     * </pre>
     * 
     * @return this
     * @throws IllegalArgumentException
     *             if the object array is {@code null} or has no elements
     */
    public AssertArray<A> isEmpty() {
        return this.isEmpty((String) null);
    }

    /**
     * Assert that an array has no elements; that is, it must not be
     * {@code null} and must have no element.
     * 
     * <pre>
     * AssertUtils.isEmpty(array, &quot;The array must have no elements&quot;);
     * </pre>
     * 
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if the object array is {@code null} or has no elements
     */
    public AssertArray<A> isEmpty(final String message, final Object... arguments) {
        isEmpty(this.get(), null, message, arguments);

        return this;
    }

    /**
     * Assert that an array has no elements; that is, it must not be
     * {@code null} and must have no element.
     * 
     * <pre>
     * AssertUtils.isEmpty(array, exceptionToThrowOnError);
     * </pre>
     * 
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the object array is {@code null} or has elements
     */
    public <E extends Throwable> AssertArray<A> isEmpty(final E exception) throws E {
        isEmpty(this.get(), exception, null);

        return this;
    }

    private static <E extends Throwable> void isEmpty(Object[] array, final E exception, final String message, final Object... arguments)
            throws E {
        if (array == null || !ArrayUtils.isEmpty(array)) {
            manageExceptions("this array must be empty and not null", exception, message, new Object[] {array}, arguments);
        }
    }

    /**
     * Assert that an array has elements; that is, it must not be {@code null}
     * and must have at least one element.
     * 
     * <pre>
     * AssertUtils.isNotEmpty(array);
     * </pre>
     * 
     * @return this
     * @throws IllegalArgumentException
     *             if the object array is {@code null} or has no elements
     */
    public AssertArray<A> isNotEmpty() {
        return this.isNotEmpty((String) null);
    }

    /**
     * Assert that an array has elements; that is, it must not be {@code null}
     * and must have at least one element.
     * 
     * <pre>
     * AssertUtils.isNotEmpty(array, &quot;The array must have elements&quot;);
     * </pre>
     * 
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if the object array is {@code null} or has no elements
     */
    public AssertArray<A> isNotEmpty(final String message, final Object... arguments) {
        isNotEmpty(this.get(), null, message, arguments);

        return this;
    }

    /**
     * Assert that an array has elements; that is, it must not be {@code null}
     * and must have at least one element.
     * 
     * <pre>
     * AssertUtils.isNotEmpty(array, exceptionToThrowOnError);
     * </pre>
     * 
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the object array is {@code null} or has no elements
     */
    public <E extends Throwable> AssertArray<A> isNotEmpty(final E exception) throws E {
        isNotEmpty(this.get(), exception, null);

        return this;
    }

    private static <E extends Throwable> void isNotEmpty(Object[] array, final E exception, final String message, final Object... arguments)
            throws E {
        if (ArrayUtils.isEmpty(array)) {
            manageExceptions("this array must not be empty: it must contain at least 1 element", exception, message, new Object[] {array},
                    arguments);
        }
    }

    /**
     * Assert that an array has no null elements.
     * 
     * <pre>
     * AssertUtils.hasNoNullElements(array);
     * </pre>
     * 
     * @return this
     * @throws IllegalArgumentException
     *             if the object array contains a {@code null} element
     */
    public AssertArray<A> hasNoNullElements() {
        return this.hasNoNullElements((String) null);
    }

    /**
     * Assert that an array has no null elements.
     * 
     * <pre>
     * AssertUtils.hasNoNullElements(array, &quot;The array must have non-null elements&quot;);
     * </pre>
     * 
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if the object array contains a {@code null} element
     */
    public AssertArray<A> hasNoNullElements(final String message, final Object... arguments) {
        hasNoNullElements(this.get(), null, message, arguments);

        return this;
    }

    /**
     * Assert that an array has no null elements.
     * 
     * <pre>
     * AssertUtils.hasNoNullElements(array, exceptionToThrowOnError);
     * </pre>
     * 
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if object array contains a {@code null} element. The standard
     *             exception is appended as suppressed.
     */
    public <E extends Throwable> AssertArray<A> hasNoNullElements(final E exception) throws E {
        hasNoNullElements(this.get(), exception, null);

        return this;
    }

    private static <T, E extends Throwable> void hasNoNullElements(T[] array, final E exception, final String message,
            final Object... arguments) throws E {

        isNotNull(array, null, null);

        for (Object element : array) {
            if (element == null) {
                manageExceptions("this array must not contain any null elements", exception, message, new Object[] {array}, arguments);
            }
        }
    }

    public AssertArray<A> contains(final A object) {
        return this.contains(object, (Comparator<A>) null);
    }

    public AssertArray<A> contains(final A object, final String message, final Object... arguments) {
        contains(this.get(), object, (Comparator<A>) null, null, message, arguments);

        return this;
    }

    public <E extends Throwable> void contains(A object, final E exception) throws E {
        contains(this.get(), object, (Comparator<A>) null, exception, null);
    }

    public AssertArray<A> contains(final A object, final Comparator<A> comparator) {
        return this.contains(object, comparator, null);
    }

    public AssertArray<A> contains(final A object, final Comparator<A> comparator, final String message, final Object... arguments) {
        contains(this.get(), object, comparator, null, message, arguments);

        return this;
    }

    public <E extends Throwable> AssertArray<A> contains(final A object, final Comparator<A> comparator, final E exception) throws E {
        contains(this.get(), object, comparator, exception, null);

        return this;
    }

    private static <T, E extends Throwable> void contains(T[] array, final T object, final Comparator<T> comparator, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(array, null, null);
        isNotNull(object, null, null);

        T[] inputArray = Arrays.copyOf(array, array.length);

        Arrays.sort(inputArray, comparator);

        if (Arrays.binarySearch(inputArray, object, comparator) < 0) {
            manageExceptions("The array must contain the object", exception, message, new Object[] {array, object}, arguments);
        }
    }

    public AssertArray<A> contains(A[] objects) {
        return this.contains(objects, (Comparator<A>) null, null);
    }

    public AssertArray<A> contains(A[] objects, final String message, final Object... arguments) {
        contains(this.get(), objects, (Comparator<A>) null, null, message, arguments);

        return this;
    }

    public <E extends Throwable> AssertArray<A> contains(A[] objects, final E exception) throws E {
        contains(this.get(), objects, (Comparator<A>) null, exception, null);

        return this;
    }

    public AssertArray<A> contains(A[] objects, final Comparator<A> comparator) {
        return this.contains(objects, comparator, null);
    }

    public AssertArray<A> contains(A[] objects, final Comparator<A> comparator, final String message, final Object... arguments) {
        contains(this.get(), objects, comparator, null, message, arguments);

        return this;
    }

    public <E extends Throwable> AssertArray<A> contains(A[] objects, final Comparator<A> comparator, final E exception) throws E {
        contains(this.get(), objects, comparator, exception, null);

        return this;
    }

    private static <T, E extends Throwable> void contains(T[] array, T[] objects, final Comparator<T> comparator, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(array, null, null);
        isNotEmpty(objects, null, null);

        T[] inputArray = Arrays.copyOf(array, array.length);
        T[] inputObjects = Arrays.copyOf(objects, objects.length);

        Arrays.sort(inputArray, comparator);
        Arrays.sort(inputObjects, comparator);

        for (T object : inputObjects) {
            if (Arrays.binarySearch(inputArray, object, comparator) < 0) {
                manageExceptions("The array must contain all objects", exception, message, new Object[] {array, objects}, arguments);
            }
        }
    }

    public AssertArray<A> doesNotContain(final A object) {
        return this.doesNotContain(object, (Comparator<A>) null, null);
    }

    public AssertArray<A> doesNotContain(final A object, final String message, final Object... arguments) {
        doesNotContain(this.get(), object, (Comparator<A>) null, null, message, arguments);

        return this;
    }

    public <E extends Throwable> AssertArray<A> doesNotContain(final A object, final E exception) throws E {
        doesNotContain(this.get(), object, (Comparator<A>) null, exception, null);

        return this;
    }

    public AssertArray<A> doesNotContain(final A object, final Comparator<A> comparator) {
        return this.doesNotContain(object, comparator, null);
    }

    public AssertArray<A> doesNotContain(final A object, final Comparator<A> comparator, final String message, final Object... arguments) {
        doesNotContain(this.get(), object, comparator, null, message, arguments);

        return this;
    }

    public <E extends Throwable> AssertArray<A> doesNotContain(final A object, final Comparator<A> comparator, final E exception) throws E {
        doesNotContain(this.get(), object, comparator, exception, null);

        return this;
    }

    private static <T, E extends Throwable> void doesNotContain(T[] array, final T object, final Comparator<T> comparator,
            final E exception, final String message, final Object... arguments) throws E {

        isNotEmpty(array, null, null);
        isNotNull(object, null, null);

        T[] inputArray = Arrays.copyOf(array, array.length);

        Arrays.sort(inputArray, comparator);

        if (Arrays.binarySearch(inputArray, object, comparator) > -1) {
            manageExceptions("The array contains the object", exception, message, new Object[] {array, object}, arguments);
        }
    }

    public AssertArray<A> doesNotContain(A[] objects) {
        return this.doesNotContain(objects, (Comparator<A>) null, null);
    }

    public AssertArray<A> doesNotContain(A[] objects, final String message, final Object... arguments) {
        doesNotContain(this.get(), objects, (Comparator<A>) null, null, message, arguments);

        return this;
    }

    public <E extends Throwable> AssertArray<A> doesNotContain(A[] objects, final E exception) throws E {
        doesNotContain(this.get(), objects, (Comparator<A>) null, exception, null);

        return this;
    }

    public AssertArray<A> doesNotContain(A[] objects, final Comparator<A> comparator) {
        return this.doesNotContain(objects, comparator, null);
    }

    public AssertArray<A> doesNotContain(A[] objects, final Comparator<A> comparator, final String message, final Object... arguments) {
        doesNotContain(this.get(), objects, comparator, null, message, arguments);

        return this;
    }

    public <E extends Throwable> AssertArray<A> doesNotContain(A[] objects, final Comparator<A> comparator, final E exception) throws E {
        doesNotContain(this.get(), objects, comparator, exception, null);

        return this;
    }

    private <T, E extends Throwable> void doesNotContain(T[] array, T[] objects, final Comparator<T> comparator, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(array, null, null);
        isNotEmpty(objects, null, null);

        T[] inputArray = Arrays.copyOf(array, array.length);
        T[] inputObjects = Arrays.copyOf(objects, objects.length);

        Arrays.sort(inputArray, comparator);
        Arrays.sort(inputObjects, comparator);

        for (T object : inputObjects) {
            if (Arrays.binarySearch(inputArray, object, comparator) > -1) {
                manageExceptions("The array must not contain any object", exception, message, new Object[] {array, objects}, arguments);
            }
        }
    }
}