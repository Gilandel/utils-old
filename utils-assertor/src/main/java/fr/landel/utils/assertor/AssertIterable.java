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

    /**
     * 
     * Constructor
     *
     * @param object
     *            The object to check
     */
    protected AssertIterable(final I object) {
        super(object);
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
        final StringBuilder message = new StringBuilder();
        boolean condition = this.isNull(message);

        if (condition) {
            if (size < 0) {
                condition = false;
                message.append("the size parameter has to be greater than or equal to 0");
            } else if (Collection.class.isAssignableFrom(this.get().getClass())) {
                if (((Collection<T>) this.get()).size() != size) {
                    condition = false;
                    message.append("the collection hasn't the expected size");
                }
            } else {
                int count = 0;
                final Iterator<T> iterator = this.get().iterator();
                while (iterator.hasNext()) {
                    iterator.next();
                    count++;
                }
                if (count != size) {
                    condition = false;
                    message.append("the iterable hasn't the expected size");
                }
            }
        }

        return this.combine(condition, message, size);
    }

    /**
     * Asserts that an iterable has not the expected size.
     * 
     * <pre>
     * Assertor.that(iterable).hasNotSize(5).toThrow(&quot;Iterable must not have the expected size&quot;);
     * </pre>
     * 
     * @param size
     *            the expected size
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> hasNotSize(final int size) {
        final StringBuilder message = new StringBuilder();
        boolean condition = this.isNull(message);

        if (condition) {
            if (size < 0) {
                condition = false;
                message.append("the size parameter has to be greater than or equal to 0");
            } else if (Collection.class.isAssignableFrom(this.get().getClass())) {
                if (((Collection<T>) this.get()).size() == size) {
                    condition = false;
                    message.append("the collection hasn't the expected size");
                }
            } else {
                int count = 0;
                final Iterator<T> iterator = this.get().iterator();
                while (iterator.hasNext()) {
                    iterator.next();
                    count++;
                }
                if (count == size) {
                    condition = false;
                    message.append("the iterable hasn't the expected size");
                }
            }
        }

        return this.combine(condition, message, size);
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
        return this.combine(IterableUtils.isEmpty(this.get()), "this iterable must be empty or null");
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
        return this.combine(!IterableUtils.isEmpty(this.get()), "this iterable must be not empty");
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
        final StringBuilder message = new StringBuilder();
        boolean condition = this.isNull(message);

        if (object == null) {
            condition = false;
            message.append("the object to find must be not null");
        } else if (condition) {
            boolean found = false;

            final Iterator<T> iterator = this.get().iterator();
            while (iterator.hasNext() && !found) {
                if (object.equals(iterator.next())) {
                    found = true;
                }
            }

            if (!found) {
                condition = false;
                message.append("the iterable must contain the object '").append(AssertObject.getParam(this.getParamIndex() + 1))
                        .append("'");
            }
        }

        return this.combine(condition, message, object);
    }

    /**
     * Asserts that an iterable contains the objects.
     * 
     * <pre>
     * Assertor.that(iterable).contains(objects).toThrow(&quot;Iterable must contain all the elements&quot;);
     * </pre>
     * 
     * @param objects
     *            the objects to find in the iterable
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> contains(final Iterable<T> objects) {
        final StringBuilder message = new StringBuilder();
        boolean condition = this.isNull(message);

        if (IterableUtils.isEmpty(objects)) {
            condition = false;
            message.append("the objects to find must be not null and not empty");
        } else if (condition) {
            final T[] array = CollectionUtils2.toArray(this.get());

            if (array != null) {
                for (T object : objects) {
                    if (Arrays.binarySearch(array, object) < 0) {
                        condition = false;
                        break;
                    }
                }
            } else {
                condition = false;
            }

            if (!condition) {
                message.append("the iterable must contain all objects");
            }
        }

        return this.combine(condition, message, objects);
    }

    /**
     * Asserts that an iterable does not contain the object.
     * 
     * <pre>
     * Assertor.that(iterable).doesNotContain(object).toThrow(&quot;Iterable must NOT contain the element&quot;);
     * </pre>
     * 
     * @param object
     *            the object to find in the iterable
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> doesNotContain(final T object) {
        final StringBuilder message = new StringBuilder();
        boolean condition = this.isNull(message);

        if (object == null) {
            condition = false;
            message.append("the object to find must be not null");
        } else if (condition) {
            boolean found = false;

            final Iterator<T> iterator = this.get().iterator();
            while (iterator.hasNext() && !found) {
                if (object.equals(iterator.next())) {
                    found = true;
                }
            }

            if (found) {
                condition = false;
                message.append("the iterable must not contain the object '").append(AssertObject.getParam(this.getParamIndex() + 1))
                        .append("'");
            }
        }

        return this.combine(condition, message, object);
    }

    /**
     * Asserts that an iterable does not contain any objects.
     * 
     * <pre>
     * Assertor.that(iterable).doesNotContain(objects).toThrow(&quot;Iterable must NOT contain any elements&quot;);
     * </pre>
     * 
     * @param objects
     *            the objects to find in the iterable
     * @return the operator
     */
    public Operator<AssertIterable<I, T>, I> doesNotContain(final Iterable<T> objects) {
        final StringBuilder message = new StringBuilder();
        boolean condition = this.isNull(message);

        if (IterableUtils.isEmpty(objects)) {
            condition = false;
            message.append("the objects to find must be not null and not empty");
        } else if (condition) {
            T[] array = CollectionUtils2.toArray(this.get());

            if (array != null) {
                for (T object : objects) {
                    if (Arrays.binarySearch(array, object) > -1) {
                        condition = false;
                        break;
                    }
                }
            }
            if (!condition) {
                message.append("the iterable must not contain any object");
            }
        }

        return this.combine(condition, message, objects);
    }

    private boolean isNull(final StringBuilder message) {
        boolean condition = true;
        if (this.get() == null) {
            condition = false;
            message.append("the iterable must be not null");
        }
        return condition;
    }
}
