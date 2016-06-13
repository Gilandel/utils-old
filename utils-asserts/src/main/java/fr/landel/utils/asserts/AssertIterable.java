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
import java.util.Collection;
import java.util.Iterator;

import org.apache.commons.collections4.IterableUtils;

import fr.landel.utils.commons.CollectionUtils2;

/**
 * Assertion utility class that assists in validating arguments for iterables.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public class AssertIterable<I extends Iterable<T>, T> extends AssertObject<AssertIterable<I, T>, I> {

    protected AssertIterable(I object) {
        super(object);
    }

    public AssertIterable<I, T> hasSize(final int size) {
        return this.hasSize(size, (String) null);
    }

    public AssertIterable<I, T> hasSize(final int size, final String message, final Object... arguments) {
        hasSize(this.get(), size, null, message, arguments);

        return this;
    }

    public <E extends Throwable> AssertIterable<I, T> hasSize(final int size, final E exception) throws E {
        hasSize(this.get(), size, exception, null);

        return this;
    }

    protected static <X, E extends Throwable> void hasSize(final Iterable<X> iterable, final int size, final E exception,
            final String message, final Object... arguments) throws E {

        AssertNumber.isGTE(size, 0, null, "The size parameter has to be greater than or equal to 0");

        if (iterable == null) {
            manageExceptions("the iterable is null", exception, message, new Object[] {iterable}, arguments);
        } else if (Collection.class.isAssignableFrom(iterable.getClass()) && ((Collection<X>) iterable).size() != size) {
            manageExceptions("the collection hasn't the expected size", exception, message, new Object[] {iterable}, arguments);
        } else {
            int count = 0;
            final Iterator<X> iterator = iterable.iterator();
            while (iterator.hasNext()) {
                iterator.next();
                count++;
            }
            if (count != size) {
                manageExceptions("the iterable hasn't the expected size", exception, message, new Object[] {iterable}, arguments);
            }
        }
    }

    /**
     * Assert that a iterable has no elements and not {@code null}.
     * 
     * <pre>
     * AssertUtils.isEmpty(iterable, &quot;Iterable must have no elements&quot;);
     * </pre>
     * 
     * @return this
     * @throws IllegalArgumentException
     *             if the iterable is {@code null} or has elements
     */
    public AssertIterable<I, T> isEmpty() {
        return this.isEmpty((String) null);
    }

    /**
     * Assert that a iterable has no elements and not {@code null}.
     * 
     * <pre>
     * AssertUtils.isEmpty(iterable, &quot;Iterable must have elements&quot;);
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
     *             if the iterable is {@code null} or has elements
     */
    public AssertIterable<I, T> isEmpty(final String message, final Object... arguments) {
        isEmpty(this.get(), null, message, arguments);

        return this;
    }

    /**
     * Assert that a iterable has no elements and not {@code null}.
     * 
     * <pre>
     * AssertUtils.isEmpty(iterable, exceptionToThrowOnError);
     * </pre>
     * 
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the iterable is {@code null} or has elements
     */
    public <E extends Throwable> AssertIterable<I, T> isEmpty(final E exception) throws E {
        isEmpty(this.get(), exception, null);

        return this;
    }

    private static <E extends Throwable> void isEmpty(final Iterable<?> iterable, final E exception, final String message,
            final Object... arguments) throws E {
        if (iterable == null || !IterableUtils.isEmpty(iterable)) {
            manageExceptions("this iterable must be empty and not null", exception, message, new Object[] {iterable}, arguments);
        }
    }

    /**
     * Assert that an iterable has elements (not be {@code null} and have at
     * least one element).
     * 
     * <pre>
     * AssertUtils.isNotEmpty(iterable, &quot;Iterable must have elements&quot;);
     * </pre>
     * 
     * @return this
     * @throws IllegalArgumentException
     *             if the iterable is {@code null} or has no elements
     */
    public AssertIterable<I, T> isNotEmpty() {
        return this.isNotEmpty((String) null);
    }

    /**
     * Assert that an iterable has elements (not be {@code null} and have at
     * least one element).
     * 
     * <pre>
     * AssertUtils.isNotEmpty(iterable, &quot;Iterable must have elements&quot;);
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
     *             if the iterable is {@code null} or has no elements
     */
    public AssertIterable<I, T> isNotEmpty(final String message, final Object... arguments) {
        isNotEmpty(this.get(), null, message, arguments);

        return this;
    }

    /**
     * Assert that an iterable has elements (not be {@code null} and have at
     * least one element).
     * 
     * <pre>
     * AssertUtils.isNotEmpty(iterable, exceptionToThrowOnError);
     * </pre>
     * 
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the iterable is {@code null} or has no elements
     */
    public <E extends Throwable> AssertIterable<I, T> isNotEmpty(final E exception) throws E {
        isNotEmpty(this.get(), exception, null);

        return this;
    }

    private static <E extends Throwable> void isNotEmpty(final Iterable<?> iterable, final E exception, final String message,
            final Object... arguments) throws E {
        if (IterableUtils.isEmpty(iterable)) {
            manageExceptions("this iterable must not be empty: it must contain at least 1 element", exception, message,
                    new Object[] {iterable}, arguments);
        }
    }

    public AssertIterable<I, T> contains(final T object) {
        return this.contains(object, null);
    }

    public AssertIterable<I, T> contains(final T object, final String message, final Object... arguments) {
        contains(this.get(), object, null, message, arguments);

        return this;
    }

    public <E extends Throwable> AssertIterable<I, T> contains(final T object, final E exception) throws E {
        contains(this.get(), object, exception, null);

        return this;
    }

    private static <T, E extends Throwable> void contains(final Iterable<T> iterable, final T object, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(iterable, null, null);
        isNotNull(object, null, null);

        boolean found = false;

        Iterator<T> iterator = iterable.iterator();
        while (iterator.hasNext() && !found) {
            if (object.equals(iterator.next())) {
                found = true;
            }
        }

        if (!found) {
            manageExceptions("The iterable must contain the object", exception, message, new Object[] {iterable, object}, arguments);
        }
    }

    public AssertIterable<I, T> contains(final Iterable<T> objects) {
        return this.contains(objects, (String) null);
    }

    public AssertIterable<I, T> contains(final Iterable<T> objects, final String message, final Object... arguments) {
        contains(this.get(), objects, null, message, arguments);

        return this;
    }

    public <E extends Throwable> AssertIterable<I, T> contains(final Iterable<T> objects, final E exception) throws E {
        contains(this.get(), objects, exception, null);

        return this;
    }

    private static <T, E extends Throwable> void contains(final Iterable<T> iterable, final Iterable<T> objects, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(iterable, null, null);
        isNotEmpty(objects, null, null);

        T[] array = CollectionUtils2.toArray(iterable);

        for (T object : objects) {
            if (Arrays.binarySearch(array, object) < 0) {
                manageExceptions("The iterable must contain all objects", exception, message, new Object[] {array, objects}, arguments);
            }
        }
    }

    public AssertIterable<I, T> doesNotContain(final T object) {
        return this.doesNotContain(object, (String) null);
    }

    public AssertIterable<I, T> doesNotContain(final T object, final String message, final Object... arguments) {
        doesNotContain(this.get(), object, null, message, arguments);

        return this;
    }

    public <E extends Throwable> AssertIterable<I, T> doesNotContain(final T object, final E exception) throws E {
        doesNotContain(this.get(), object, exception, null);

        return this;
    }

    private static <T, E extends Throwable> void doesNotContain(final Iterable<T> iterable, final T object, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(iterable, null, null);
        isNotNull(object, null, null);

        boolean found = false;

        Iterator<T> iterator = iterable.iterator();
        while (iterator.hasNext() && !found) {
            if (object.equals(iterator.next())) {
                found = true;
            }
        }

        if (found) {
            manageExceptions("The iterable must not contain the object", exception, message, new Object[] {iterable, object}, arguments);
        }
    }

    public AssertIterable<I, T> doesNotContain(final Iterable<T> objects) {
        return this.doesNotContain(objects, (String) null);
    }

    public AssertIterable<I, T> doesNotContain(final Iterable<T> objects, final String message, final Object... arguments) {
        doesNotContain(this.get(), objects, null, message, arguments);

        return this;
    }

    public <E extends Throwable> AssertIterable<I, T> doesNotContain(final Iterable<T> objects, final E exception) throws E {
        doesNotContain(this.get(), objects, exception, null);

        return this;
    }

    private static <T, E extends Throwable> void doesNotContain(final Iterable<T> iterable, final Iterable<T> objects, final E exception,
            final String message, final Object... arguments) throws E {

        isNotEmpty(iterable, null, null);
        isNotEmpty(objects, null, null);

        T[] array = CollectionUtils2.toArray(iterable);

        for (T object : objects) {
            if (Arrays.binarySearch(array, object) > -1) {
                manageExceptions("The iterable must not contain any object", exception, message, new Object[] {array, objects}, arguments);
            }
        }
    }
}