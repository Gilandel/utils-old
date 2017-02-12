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

import java.util.Objects;
import java.util.function.BiPredicate;
import java.util.function.Function;

import org.apache.commons.lang3.builder.Builder;

/**
 * Create an equals builder.
 * 
 * <p>
 * Beside the {@link EqualsBuilder}, this check from the constructor both
 * objects. Check if the objects are {@code null} or not, if instances are
 * identical and if both has the same {@code Class} or super {@code Class}.
 * </p>
 *
 * <p>
 * This class allow to directly compare properties of objects, by specifying a
 * {@code getter} method. Also for specific properties, a predicate method can
 * be defined.
 * </p>
 * 
 * <p>
 * Typical use for the code is as follows:
 * </p>
 * 
 * <pre>
 * public boolean equals(Object obj) {
 *     return new EqualsBuilder(this, obj).append(o -> o.field1, (f1, f2) -> f1.getId().).append(o -> o.field2).append(o -> o.field3).isEqual();
 * }
 * </pre>
 *
 * @since Feb 11, 2017
 * @author Gilles
 *
 * @param <T>
 *            the type of reference object
 * @param <O>
 *            the type of compare object
 */
public class EqualsBuilder2<T, O> implements Builder<Boolean> {

    private final EqualsBuilder builder;
    private final T current;
    private final O other;
    private final Class<T> superClass;
    private T casted = null;
    private boolean isEqual = true;

    /**
     * Constructor
     *
     * @param current
     *            the reference object (cannot be {@code null})
     * @param other
     *            the compare object (may be {@code null})
     */
    public EqualsBuilder2(final T current, final O other) {
        this(current, other, null);
    }

    /**
     * Constructor
     *
     * @param current
     *            the reference object (cannot be {@code null})
     * @param other
     *            the compare object (may be {@code null})
     * @param superClass
     *            the common class of both checked object
     */
    public EqualsBuilder2(final T current, final O other, final Class<T> superClass) {
        Objects.requireNonNull(current);
        this.builder = new EqualsBuilder();
        this.current = current;
        this.other = other;
        this.superClass = superClass;
        this.check();
    }

    @SuppressWarnings("unchecked")
    private void check() {
        if (this.other == null) {
            this.isEqual = false;
        } else if (this.current == this.other) {
            this.isEqual = true;
        } else if (this.superClass == null && !this.current.getClass().equals(this.other.getClass())) {
            this.isEqual = false;
        }
        // not check if superClass != null, because if a super class doesn't
        // match the checked values classes, a compiler error is thrown
        this.casted = (T) this.other;
    }

    /**
     * Append an equality check of specific properties.
     * 
     * @param getter
     *            the get parameter method
     * @param <V>
     *            the type of the property
     * @return the current builder
     */
    public <V> EqualsBuilder2<T, O> append(final Function<T, V> getter) {
        return this.append(getter, null);
    }

    /**
     * Append an equality check of specific properties and compare them with the
     * given predicate function. The predicate function is only applied if both
     * values are not {@code null}.
     * 
     * @param getter
     *            the get parameter method
     * @param predicate
     *            the parameter checker method
     * @param <V>
     *            The type of the property
     * @return the current builder
     */
    public <V> EqualsBuilder2<T, O> append(final Function<T, V> getter, final BiPredicate<V, V> predicate) {
        return this.append(this.current, this.casted, getter, predicate);
    }

    /**
     * Append an equality check of specific objects.
     * 
     * @param lhs
     *            the first object
     * @param rhs
     *            the second object
     * @param <X>
     *            the object type
     * @return the current builder
     */
    public <X> EqualsBuilder2<T, O> append(final X lhs, final X rhs) {
        if (this.isEqual) {
            this.builder.append(lhs, rhs);
        }
        return this;
    }

    /**
     * Append an equality check of specific properties of objects.
     * 
     * @param lhs
     *            the first object
     * @param rhs
     *            the second object
     * @param getter
     *            the get parameter method
     * @param <X>
     *            the object type
     * @return the current builder
     */
    public <X, V> EqualsBuilder2<T, O> append(final X lhs, final X rhs, final Function<X, V> getter) {
        return this.append(lhs, rhs, getter, null);
    }

    /**
     * Append an equality check of specific properties of objects and compare
     * them with the given predicate function. The predicate function is only
     * applied if both values are not {@code null}.
     * 
     * @param lhs
     *            the first object
     * @param rhs
     *            the second object
     * @param getter
     *            the get parameter method
     * @param predicate
     *            the parameter checker method
     * @param <X>
     *            the object type
     * @return the current builder
     */
    public <X, V> EqualsBuilder2<T, O> append(final X lhs, final X rhs, final Function<X, V> getter, final BiPredicate<V, V> predicate) {
        if (this.isEqual) {
            this.builder.append(lhs, rhs, getter, predicate);
        }
        return this;
    }

    /**
     * Returns {@code true} if the fields that have been checked are all equal.
     * 
     * @return {@code true}, if all are equal, {@code false} otherwise
     */
    public boolean isEqual() {
        return this.isEqual && this.builder.isEquals();
    }

    /**
     * Returns {@code true} if the fields that have been checked are all equal.
     * 
     * @return {@code true}, if all are equal, {@code false} otherwise
     */
    @Override
    public Boolean build() {
        return Boolean.valueOf(this.isEqual());
    }
}
