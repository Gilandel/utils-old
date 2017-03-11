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
 * A quad consisting of four elements.
 * </p>
 * 
 * <p>
 * This class is an abstract implementation defining the basic API. It refers to
 * the elements as 'first', 'second', 'third' and 'fourth'.
 * </p>
 * 
 * <p>
 * Subclass implementations may be mutable or immutable. However, there is no
 * restriction on the type of the stored objects that may be stored. If mutable
 * objects are stored in the quad, then the quad itself effectively becomes
 * mutable.
 * </p>
 *
 * @param <A>
 *            the first element type
 * @param <B>
 *            the second element type
 * @param <C>
 *            the third element type
 * @param <D>
 *            the fourth element type
 *
 * @since Jul 26, 2016
 * @author Gilles
 *
 */
public abstract class AbstractQuad<A, B, C, D> implements Comparable<AbstractQuad<A, B, C, D>>, Serializable {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -7359128872551227845L;

    /**
     * <p>
     * Gets the first element from this quad.
     * </p>
     *
     * @return the first element, may be null
     */
    public abstract A getFirst();

    /**
     * <p>
     * Gets the second element from this quad.
     * </p>
     *
     * @return the second element, may be null
     */
    public abstract B getSecond();

    /**
     * <p>
     * Gets the third element from this quad.
     * </p>
     *
     * @return the third element, may be null
     */
    public abstract C getThird();

    /**
     * <p>
     * Gets the fourth element from this quad.
     * </p>
     *
     * @return the fourth element, may be null
     */
    public abstract D getFourth();

    /**
     * <p>
     * Compares the quad based on the first element followed by the second, the
     * third and the fourth element. The types must be {@code Comparable}.
     * </p>
     * 
     * @param other
     *            the other quad, not null
     * @return negative if this is less, zero if equal, positive if greater and
     *         if other is {@code null}, returns {@link Integer#MAX_VALUE}
     */
    @Override
    public int compareTo(final AbstractQuad<A, B, C, D> other) {
        if (other == null) {
            return Integer.MAX_VALUE;
        }

        return new CompareToBuilder().append(this.getFirst(), other.getFirst()).append(this.getSecond(), other.getSecond())
                .append(this.getThird(), other.getThird()).append(this.getFourth(), other.getFourth()).toComparison();
    }

    /**
     * <p>
     * Compares this quad to another based on the four elements.
     * </p>
     * 
     * @param obj
     *            the object to compare to, null returns false
     * @return true if the elements of the quad are equal
     */
    @Override
    public boolean equals(final Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj instanceof AbstractQuad<?, ?, ?, ?>) {
            final AbstractQuad<?, ?, ?, ?> other = (AbstractQuad<?, ?, ?, ?>) obj;
            return new EqualsBuilder().append(this.getFirst(), other.getFirst()).append(this.getSecond(), other.getSecond())
                    .append(this.getThird(), other.getThird()).append(this.getFourth(), other.getFourth()).isEquals();
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
        return Objects.hash(this.getFirst(), this.getSecond(), this.getThird(), this.getFourth());
    }

    /**
     * <p>
     * Returns a String representation of this quad using the format
     * {@code ($first,$second,$third,$fourth)}.
     * </p>
     * 
     * @return a string describing this object, not null
     */
    @Override
    public String toString() {
        // @formatter:off
        return new StringBuilder()
                .append('(')
                .append(this.getFirst()).append(",")
                .append(this.getSecond()).append(",")
                .append(this.getThird()).append(",")
                .append(this.getFourth()).append(')')
                .toString();
     // @formatter:on
    }

    /**
     * <p>
     * Formats the receiver using the given format.
     * </p>
     * 
     * <p>
     * This uses {@link java.util.Formattable} to perform the formatting. Two
     * variables may be used to embed the left and right elements. Use
     * {@code %1$s} for the first element (key) and {@code %2$s} for the second
     * element (value)... The default format used by {@code toString()} is
     * {@code (%1$s,%2$s,%3$s,%4$s)}.
     * </p>
     * 
     * @param format
     *            the format string, optionally containing {@code %1$s} and
     *            {@code %2$s}, not null
     * @return the formatted string, not null
     */
    public String toString(final String format) {
        return String.format(format, this.getFirst(), this.getSecond(), this.getThird(), this.getFourth());
    }
}
