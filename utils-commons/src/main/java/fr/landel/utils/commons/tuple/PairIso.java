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
package fr.landel.utils.commons.tuple;

import java.io.Serializable;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.lang3.builder.CompareToBuilder;
import org.apache.commons.lang3.builder.EqualsBuilder;

/**
 * <p>
 * A pair consisting of two elements of same type.
 * </p>
 * 
 * <p>
 * This class is an abstract implementation defining the basic API. It refers to
 * the elements as 'left' and 'right'. It also implements the {@code Map.Entry}
 * interface where the key is 'left' and the value is 'right'.
 * </p>
 * 
 * <p>
 * Subclass implementations may be mutable or immutable. However, there is no
 * restriction on the type of the stored objects that may be stored. If mutable
 * objects are stored in the pair, then the pair itself effectively becomes
 * mutable.
 * </p>
 *
 * @param <T>
 *            the element type
 *
 * @see org.apache.commons.lang3.tuple.Pair
 *
 * @since Jul 26, 2016
 * @author Gilles
 *
 */
public abstract class PairIso<T> implements Map.Entry<T, T>, Comparable<PairIso<T>>, Serializable {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 4591070940611991136L;

    /**
     * <p>
     * Gets the left element from this pair.
     * </p>
     * 
     * <p>
     * When treated as a key-value pair, this is the key.
     * </p>
     * 
     * @return the left element, may be null
     */
    public abstract T getLeft();

    /**
     * <p>
     * Gets the right element from this pair.
     * </p>
     * 
     * <p>
     * When treated as a key-value pair, this is the value.
     * </p>
     * 
     * @return the right element, may be null
     */
    public abstract T getRight();

    /**
     * <p>
     * Gets the key from this pair.
     * </p>
     * 
     * <p>
     * This method implements the {@code Map.Entry} interface returning the left
     * element as the key.
     * </p>
     * 
     * @return the left element as the key, may be null
     */
    @Override
    public final T getKey() {
        return getLeft();
    }

    /**
     * <p>
     * Gets the value from this pair.
     * </p>
     * 
     * <p>
     * This method implements the {@code Map.Entry} interface returning the
     * right element as the value.
     * </p>
     * 
     * @return the right element as the value, may be null
     */
    @Override
    public T getValue() {
        return getRight();
    }

    /**
     * <p>
     * Compares the pair based on the left element followed by the right
     * element. The types must be {@code Comparable}.
     * </p>
     * 
     * @param other
     *            the other pair, may be null
     * @return negative if this is less, zero if equal, positive if greater and
     *         if other is {@code null}, returns {@link Integer#MAX_VALUE}
     */
    @Override
    public int compareTo(final PairIso<T> other) {
        if (other == null) {
            return Integer.MAX_VALUE;
        }

        return new CompareToBuilder().append(this.getLeft(), other.getLeft()).append(this.getRight(), other.getRight()).toComparison();
    }

    /**
     * <p>
     * Compares this pair to another based on the two elements.
     * </p>
     * 
     * @param obj
     *            the object to compare to, null returns false
     * @return true if the elements of the pair are equal
     */
    @Override
    public boolean equals(final Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj instanceof Map.Entry<?, ?>) {
            final Map.Entry<?, ?> other = (Map.Entry<?, ?>) obj;
            return new EqualsBuilder().append(this.getKey(), other.getKey()).append(this.getValue(), other.getValue()).isEquals();
        }
        return false;
    }

    /**
     * <p>
     * Returns a suitable hash code. The hash code follows the definition in
     * {@code Map.Entry}.
     * </p>
     * 
     * @return the hash code
     */
    @Override
    public int hashCode() {
        // see Map.Entry API specification
        return Objects.hashCode(this.getKey()) ^ Objects.hashCode(this.getValue());
    }

    /**
     * <p>
     * Returns a String representation of this pair using the format
     * {@code ($left,$right)}.
     * </p>
     * 
     * @return a string describing this object, not null
     */
    @Override
    public String toString() {
        return new StringBuilder().append('(').append(getLeft()).append(", ").append(getRight()).append(')').toString();
    }

    /**
     * <p>
     * Formats the receiver using the given format.
     * </p>
     * 
     * <p>
     * This uses {@link java.util.Formattable} to perform the formatting. Two
     * variables may be used to embed the left and right elements. Use
     * {@code %1$s} for the left element (key) and {@code %2$s} for the right
     * element (value). The default format used by {@code toString()} is
     * {@code (%1$s,%2$s)}.
     * </p>
     * 
     * @param format
     *            the format string, optionally containing {@code %1$s} and
     *            {@code %2$s}, not null
     * @return the formatted string, not null
     */
    public String toString(final String format) {
        return String.format(format, getLeft(), getRight());
    }

    /**
     * <p>
     * Obtains an immutable pair of from two objects inferring the generic
     * types.
     * </p>
     * 
     * <p>
     * This factory allows the pair to be created using inference to obtain the
     * generic types.
     * </p>
     * 
     * @param <T>
     *            the element type
     * @param left
     *            the left element, may be null
     * @param right
     *            the right element, may be null
     * @return a pair formed from the two parameters, not null
     */
    public static <T> PairIso<T> of(final T left, final T right) {
        return new ImmutablePairIso<T>(left, right);
    }

    /**
     * <p>
     * Obtains a mutable pair of from two objects inferring the generic type.
     * </p>
     * 
     * <p>
     * This factory allows the pair to be created using inference to obtain the
     * generic types.
     * </p>
     * 
     * @param <T>
     *            the element type
     * @param left
     *            the left element, may be null
     * @param right
     *            the right element, may be null
     * @return a pair formed from the two parameters, not null
     */
    public static <T> MutablePairIso<T> ofMutable(final T left, final T right) {
        return new MutablePairIso<T>(left, right);
    }
}
