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

import org.apache.commons.collections4.IterableUtils;

/**
 * Assertion utility class that assists in validating arguments for iterables.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public class AssertIterable<I extends Iterable<T>, T> extends AssertObject<AssertIterable<I, T>, I> {

    /**
     * 
     * Constructor
     *
     * @param object
     *            The object to check
     */
    protected AssertIterable(final I object) {
        super(object, TYPE.ITERABLE);
    }

    /**
     * Asserts that an iterable has the expected size.
     * 
     * <pre>
     * Assertor.that(iterable).hasSize(5).toThrow(&quot;Iterable must have the expected size&quot;);
     * </pre>
     * 
     * @param size
     *            the expected size
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> hasSize(final int size) {
        return this.hasSize(size, this.msg(MSG.ITERABLE.SIZE, this.getParam(), this.getNextParam(1, TYPE.NUMBER_INTEGER)));
    }

    /**
     * Asserts that an iterable has the expected size.
     * 
     * <pre>
     * Assertor.that(iterable).hasSize(5).toThrow(&quot;Iterable must have the expected size&quot;);
     * </pre>
     * 
     * @param size
     *            the expected size
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> hasSize(final int size, final CharSequence message, final Object... arguments) {
        return this.hasSize(size, null, message, arguments);
    }

    /**
     * Asserts that an iterable has the expected size.
     * 
     * <pre>
     * Assertor.that(iterable).hasSize(5).toThrow(&quot;Iterable must have the expected size&quot;);
     * </pre>
     * 
     * @param size
     *            the expected size
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> hasSize(final int size, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(size >= 0 && this.get() != null, () -> IterableUtils.size(this.get()) == size,
                () -> this.msg(MSG.ITERABLE.SIZE, true), message, arguments, locale, size);
    }

    /**
     * Asserts that a iterable has no elements and not {@code null}.
     * 
     * <pre>
     * Assertor.that(iterable).isEmpty().toThrow(&quot;Iterable must have no elements&quot;);
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> isEmpty() {
        return this.isEmpty(this.msg(MSG.ITERABLE.EMPTY, this.getParam()));
    }

    /**
     * Asserts that a iterable has no elements and not {@code null}.
     * 
     * <pre>
     * Assertor.that(iterable).isEmpty().toThrow(&quot;Iterable must have no elements&quot;);
     * </pre>
     * 
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> isEmpty(final CharSequence message, final Object... arguments) {
        return this.isEmpty(null, message, arguments);
    }

    /**
     * Asserts that a iterable has no elements and not {@code null}.
     * 
     * <pre>
     * Assertor.that(iterable).isEmpty().toThrow(&quot;Iterable must have no elements&quot;);
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
    public Operator<AssertIterable<I, T>, I> isEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> IterableUtils.isEmpty(this.get()), null, message, arguments, locale);
    }

    /**
     * Asserts that an iterable has elements (not be {@code null} and have at
     * least one element).
     * 
     * <pre>
     * Assertor.that(iterable).isNotEmpty().toThrow(&quot;Iterable must have elements&quot;);
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> isNotEmpty() {
        return this.isNotEmpty(this.msg(MSG.ITERABLE.EMPTY + MSG.NOT, this.getParam()));
    }

    /**
     * Asserts that an iterable has elements (not be {@code null} and have at
     * least one element).
     * 
     * <pre>
     * Assertor.that(iterable).isNotEmpty().toThrow(&quot;Iterable must have elements&quot;);
     * </pre>
     * 
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> isNotEmpty(final CharSequence message, final Object... arguments) {
        return this.isNotEmpty(null, message, arguments);
    }

    /**
     * Asserts that an iterable has elements (not be {@code null} and have at
     * least one element).
     * 
     * <pre>
     * Assertor.that(iterable).isNotEmpty().toThrow(&quot;Iterable must have elements&quot;);
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
    public Operator<AssertIterable<I, T>, I> isNotEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> !IterableUtils.isEmpty(this.get()), null, message, arguments, locale);
    }

    /**
     * Asserts that an iterable contains the object.
     * 
     * <pre>
     * Assertor.that(iterable).contains(object).toThrow(&quot;Iterable must contain the element&quot;);
     * </pre>
     * 
     * @param object
     *            the object to find in the iterable
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> contains(final T object) {
        return this.contains(object, this.msg(MSG.ITERABLE.CONTAINS_OBJECT, this.getParam(), this.getNextParam(1, TYPE.ITERABLE)));
    }

    /**
     * Asserts that an iterable contains the object.
     * 
     * <pre>
     * Assertor.that(iterable).contains(object).toThrow(&quot;Iterable must contain the element&quot;);
     * </pre>
     * 
     * @param object
     *            the object to find in the iterable
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> contains(final T object, final CharSequence message, final Object... arguments) {
        return this.contains(object, null, message, arguments);
    }

    /**
     * Asserts that an iterable contains the object.
     * 
     * <pre>
     * Assertor.that(iterable).contains(object).toThrow(&quot;Iterable must contain the element&quot;);
     * </pre>
     * 
     * @param object
     *            the object to find in the iterable
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> contains(final T object, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(this.get() != null, () -> this.has(object), () -> this.msg(MSG.ITERABLE.CONTAINS_OBJECT, true), message,
                arguments, locale, object);
    }

    /**
     * Asserts that an iterable contains all objects.
     * 
     * <pre>
     * Assertor.that(iterable).containsAll(objects).toThrow(&quot;Iterable must contain all the elements&quot;);
     * </pre>
     * 
     * @param objects
     *            the objects to find in the iterable
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> containsAll(final Iterable<T> objects) {
        return this.containsAll(objects, this.msg(MSG.ITERABLE.CONTAINS_ALL, this.getParam(), this.getNextParam(1, TYPE.ITERABLE)));
    }

    /**
     * Asserts that an iterable contains all objects.
     * 
     * <pre>
     * Assertor.that(iterable).containsAll(objects).toThrow(&quot;Iterable must contain all the elements&quot;);
     * </pre>
     * 
     * @param objects
     *            the objects to find in the iterable
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> containsAll(final Iterable<T> objects, final CharSequence message, final Object... arguments) {
        return this.containsAll(objects, null, message, arguments);
    }

    /**
     * Asserts that an iterable contains all objects.
     * 
     * <pre>
     * Assertor.that(iterable).containsAll(objects).toThrow(&quot;Iterable must contain all the elements&quot;);
     * </pre>
     * 
     * @param objects
     *            the objects to find in the iterable
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> containsAll(final Iterable<T> objects, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(this.get() != null && objects != null, () -> this.has(objects, true),
                () -> this.msg(MSG.ITERABLE.CONTAINS_ALL, true), message, arguments, locale, objects);
    }

    /**
     * Asserts that an iterable contains any objects.
     * 
     * <pre>
     * Assertor.that(iterable).containsAny(objects).toThrow(&quot;Iterable must contain all the elements&quot;);
     * </pre>
     * 
     * @param objects
     *            the objects to find in the iterable
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> containsAny(final Iterable<T> objects) {
        return this.containsAny(objects, this.msg(MSG.ITERABLE.CONTAINS_ANY, this.getParam(), this.getNextParam(1, TYPE.ITERABLE)));
    }

    /**
     * Asserts that an iterable contains any objects.
     * 
     * <pre>
     * Assertor.that(iterable).containsAny(objects).toThrow(&quot;Iterable must contain all the elements&quot;);
     * </pre>
     * 
     * @param objects
     *            the objects to find in the iterable
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> containsAny(final Iterable<T> objects, final CharSequence message, final Object... arguments) {
        return this.containsAny(objects, null, message, arguments);
    }

    /**
     * Asserts that an iterable contains any objects.
     * 
     * <pre>
     * Assertor.that(iterable).containsAny(objects).toThrow(&quot;Iterable must contain all the elements&quot;);
     * </pre>
     * 
     * @param objects
     *            the objects to find in the iterable
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> containsAny(final Iterable<T> objects, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(this.get() != null && objects != null, () -> this.has(objects, false),
                () -> this.msg(MSG.ITERABLE.CONTAINS_ANY, true), message, arguments, locale, objects);
    }

    private boolean has(final T object) {
        boolean found = false;
        if (object != null) {
            for (T objectRef : this.get()) {
                if (object.equals(objectRef)) {
                    found = true;
                    break;
                }
            }
        } else {
            for (T objectRef : this.get()) {
                if (objectRef == null) {
                    found = true;
                    break;
                }
            }
        }
        return found;
    }

    private boolean has(final Iterable<T> iterable, final boolean all) {
        int found = 0;
        for (T objectRef : iterable) {
            if (this.has(objectRef)) {
                found++;
            }
        }
        if (all ^ this.isNot()) {
            return found == IterableUtils.size(iterable);
        } else {
            return found > 0;
        }
    }
}