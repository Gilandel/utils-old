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
import java.util.Objects;

import org.apache.commons.lang3.builder.CompareToBuilder;
import org.apache.commons.lang3.builder.EqualsBuilder;

/**
 * <p>
 * A triple consisting of three elements of same type.
 * </p>
 * 
 * <p>
 * This class is an abstract implementation defining the basic API. It refers to
 * the elements as 'left', 'middle' and 'right'.
 * </p>
 * 
 * <p>
 * Subclass implementations may be mutable or immutable. However, there is no
 * restriction on the type of the stored objects that may be stored. If mutable
 * objects are stored in the triple, then the triple itself effectively becomes
 * mutable.
 * </p>
 *
 * @param <T>
 *            the element type
 *
 * @see org.apache.commons.lang3.tuple.Triple
 *
 * @since Jul 26, 2016
 * @author Gilles
 *
 */
public abstract class TripleIso<T> implements Comparable<TripleIso<T>>, Serializable {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 4591070140611991136L;

    /**
     * <p>
     * Gets the left element from this triple.
     * </p>
     *
     * @return the left element, may be null
     */
    public abstract T getLeft();

    /**
     * <p>
     * Gets the middle element from this triple.
     * </p>
     *
     * @return the middle element, may be null
     */
    public abstract T getMiddle();

    /**
     * <p>
     * Gets the right element from this triple.
     * </p>
     *
     * @return the right element, may be null
     */
    public abstract T getRight();

    /**
     * <p>
     * Compares the triple based on the left element followed by the middle
     * element and the right element. The types must be {@code Comparable}.
     * </p>
     * 
     * @param other
     *            the other triple, not null
     * @return negative if this is less, zero if equal, positive if greater and
     *         if other is {@code null}, returns {@link Integer#MAX_VALUE}
     */
    @Override
    public int compareTo(final TripleIso<T> other) {
        if (other == null) {
            return Integer.MAX_VALUE;
        }

        return new CompareToBuilder().append(this.getLeft(), other.getLeft()).append(this.getMiddle(), other.getMiddle())
                .append(this.getRight(), other.getRight()).toComparison();
    }

    /**
     * <p>
     * Compares this triple to another based on the three elements.
     * </p>
     * 
     * @param obj
     *            the object to compare to, null returns false
     * @return true if the elements of the triple are equal
     */
    @Override
    public boolean equals(final Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj instanceof TripleIso<?>) {
            final TripleIso<?> other = (TripleIso<?>) obj;
            return new EqualsBuilder().append(this.getLeft(), other.getLeft()).append(this.getMiddle(), other.getMiddle())
                    .append(this.getRight(), other.getRight()).isEquals();
        }
        return false;
    }

    /**
     * <p>
     * Returns a suitable hash code.
     * </p>
     * 
     * @return the hash code
     */
    @Override
    public int hashCode() {
        return Objects.hash(this.getLeft(), this.getMiddle(), this.getRight());
    }

    /**
     * <p>
     * Returns a String representation of this triple using the format
     * {@code ($left,$middle,$right)}.
     * </p>
     * 
     * @return a string describing this object, not null
     */
    @Override
    public String toString() {
        return new StringBuilder().append('(').append(this.getLeft()).append(", ").append(this.getMiddle()).append(", ")
                .append(this.getRight()).append(')').toString();
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
        return String.format(format, this.getLeft(), this.getMiddle(), this.getRight());
    }

    /**
     * <p>
     * Obtains an immutable triple of from three objects inferring the generic
     * types.
     * </p>
     * 
     * <p>
     * This factory allows the triple to be created using inference to obtain
     * the generic types.
     * </p>
     * 
     * @param <T>
     *            the element type
     * @param left
     *            the left element, may be null
     * @param middle
     *            the middle element, may be null
     * @param right
     *            the right element, may be null
     * @return a triple formed from the three parameters, not null
     */
    public static <T> TripleIso<T> of(final T left, final T middle, final T right) {
        return new ImmutableTripleIso<T>(left, middle, right);
    }

    /**
     * <p>
     * Obtains a mutable triple of from three objects inferring the generic
     * type.
     * </p>
     * 
     * <p>
     * This factory allows the triple to be created using inference to obtain
     * the generic types.
     * </p>
     * 
     * @param <T>
     *            the element type
     * @param left
     *            the left element, may be null
     * @param middle
     *            the middle value, may be null
     * @param right
     *            the right element, may be null
     * @return a triple formed from the three parameters, not null
     */
    public static <T> MutableTripleIso<T> ofMutable(final T left, final T middle, final T right) {
        return new MutableTripleIso<T>(left, middle, right);
    }
}
