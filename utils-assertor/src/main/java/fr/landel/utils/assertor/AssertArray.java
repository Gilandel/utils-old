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
        super(array, TYPE.ARRAY);
    }

    /**
     * Assert that the array length is equal to the size
     * 
     * <pre>
     * Assertor.that(array).hasLength(length).toThrow(exception);
     * Assertor.that(array).hasLength(length).isOK();
     * </pre>
     * 
     * @param length
     *            The length
     * @return the operator
     */
    public Operator<AssertArray<T>, T[]> hasLength(final int length) {
        return this.hasLength(length, this.msg(MSG.ARRAY.LENGTH, this.getParam(), this.getNextParam(1, TYPE.NUMBER_INTEGER)));
    }

    /**
     * Assert that the array length is equal to the size
     * 
     * <pre>
     * Assertor.that(array).hasLength(length, "Incorrect number of entries").toThrow(exception);
     * Assertor.that(array).hasLength(length, "Incorrect number of entries").isOK();
     * </pre>
     * 
     * @param length
     *            The length
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertArray<T>, T[]> hasLength(final int length, final CharSequence message, final Object... arguments) {
        return this.hasLength(length, null, message, arguments);
    }

    /**
     * Assert that the array length is equal to the size
     * 
     * <pre>
     * Assertor.that(array).hasLength(length, Locale.US, "Incorrect number of entries").toThrow(exception);
     * Assertor.that(array).hasLength(length, Locale.US, "Incorrect number of entries").isOK();
     * </pre>
     * 
     * @param length
     *            The length
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertArray<T>, T[]> hasLength(final int length, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(length >= 0 && this.get() != null, () -> this.get().length == length, () -> this.msg(MSG.ARRAY.LENGTH, true),
                message, arguments, locale, length);
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
        return this.isEmpty(this.msg(MSG.ARRAY.EMPTY));
    }

    /**
     * Assert that an array has no elements; that is, it can be {@code null} or
     * must have no element.
     * 
     * <pre>
     * Assertor.that(array).isEmpty().toThrow(exception);
     * </pre>
     * 
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertArray<T>, T[]> isEmpty(final CharSequence message, final Object... arguments) {
        return this.isEmpty(null, message, arguments);
    }

    /**
     * Assert that an array has no elements; that is, it can be {@code null} or
     * must have no element.
     * 
     * <pre>
     * Assertor.that(array).isEmpty().toThrow(exception);
     * </pre>
     * 
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertArray<T>, T[]> isEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> ArrayUtils.isEmpty(this.get()), null, message, arguments, locale);
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
        return this.isNotEmpty(this.msg(MSG.ARRAY.EMPTY + MSG.NOT));
    }

    /**
     * Assert that an array has elements; that is, it must not be {@code null}
     * and must have at least one element.
     * 
     * <pre>
     * Assertor.that(array).isNotEmpty().toThrow(exception);
     * </pre>
     * 
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return this
     * @throws IllegalArgumentException
     *             if the object array is {@code null} or has no elements
     */
    public Operator<AssertArray<T>, T[]> isNotEmpty(final CharSequence message, final Object... arguments) {
        return this.isNotEmpty(null, message, arguments);
    }

    /**
     * Assert that an array has elements; that is, it must not be {@code null}
     * and must have at least one element.
     * 
     * <pre>
     * Assertor.that(array).isNotEmpty().toThrow(exception);
     * </pre>
     * 
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return this
     * @throws IllegalArgumentException
     *             if the object array is {@code null} or has no elements
     */
    public Operator<AssertArray<T>, T[]> isNotEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> ArrayUtils.isNotEmpty(this.get()), null, message, arguments, locale);
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
        return this.contains(object, this.msg(MSG.ARRAY.CONTAINS_OBJECT));
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
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return The operator
     */
    public Operator<AssertArray<T>, T[]> contains(final T object, final CharSequence message, final Object... arguments) {
        return this.contains(object, null, message, arguments);
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
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return The operator
     */
    public Operator<AssertArray<T>, T[]> contains(final T object, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(this.get() != null, () -> this.has(object), () -> this.msg(MSG.ARRAY.CONTAINS_OBJECT, true), message, arguments,
                locale, object);
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
    public Operator<AssertArray<T>, T[]> containsAll(final T[] array) {
        return this.containsAll(array, this.msg(MSG.ARRAY.CONTAINS_ALL));
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
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return The operator
     */
    public Operator<AssertArray<T>, T[]> containsAll(final T[] array, final CharSequence message, final Object... arguments) {
        return this.containsAll(array, null, message, arguments);
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
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return The operator
     */
    public Operator<AssertArray<T>, T[]> containsAll(final T[] array, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(this.get() != null && array != null, () -> this.has(array, true), () -> this.msg(MSG.ARRAY.CONTAINS_ALL, true),
                message, arguments, locale, array);
    }

    /**
     * Assert that the array contains any the objects
     * 
     * <pre>
     * Assertor.that(array1).containsAny(array2).toThrow(exception);
     * </pre>
     * 
     * @param array
     *            The array
     * @return The operator
     */
    public Operator<AssertArray<T>, T[]> containsAny(final T[] array) {
        return this.containsAny(array, this.msg(MSG.ARRAY.CONTAINS_ANY));
    }

    /**
     * Assert that the array contains any the objects
     * 
     * <pre>
     * Assertor.that(array1).containsAny(array2).toThrow(exception);
     * </pre>
     * 
     * @param array
     *            The array
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return The operator
     */
    public Operator<AssertArray<T>, T[]> containsAny(final T[] array, final CharSequence message, final Object... arguments) {
        return this.containsAny(array, null, message, arguments);
    }

    /**
     * Assert that the array contains any the objects
     * 
     * <pre>
     * Assertor.that(array1).containsAny(array2).toThrow(exception);
     * </pre>
     * 
     * @param array
     *            The array
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return The operator
     */
    public Operator<AssertArray<T>, T[]> containsAny(final T[] array, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(this.get() != null && array != null, () -> this.has(array, false), () -> this.msg(MSG.ARRAY.CONTAINS_ANY, true),
                message, arguments, locale, array);
    }

    private boolean has(final T object) {
        boolean found = false;
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
        return found;
    }

    private boolean has(final T[] array, final boolean all) {
        int found = 0;
        for (T objectArray : array) {
            if (this.has(objectArray)) {
                found++;
            }
        }
        if (all ^ this.isNot()) {
            return found == array.length;
        } else {
            return found > 0;
        }
    }
}