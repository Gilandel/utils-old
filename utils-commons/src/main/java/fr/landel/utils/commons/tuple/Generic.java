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
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.CompareToBuilder;
import org.apache.commons.lang3.builder.EqualsBuilder;

/**
 * <p>
 * A generic consisting of elements of same type (may also be different if the
 * result is a generic of {@code Object} or a generic of common implementation).
 * </p>
 * 
 * <p>
 * This class is an abstract implementation defining the basic API. It refers to
 * the elements through methods like 'get', 'set', 'getList' or 'size'.
 * </p>
 * 
 * <p>
 * Subclass implementations may be mutable or immutable. However, there is no
 * restriction on the type of the stored objects that may be stored. If mutable
 * objects are stored in the generic, then the generic itself effectively
 * becomes mutable.
 * </p>
 * 
 * <p>
 * Prefer to create a Data Transfer Object to be more comprehensible and
 * readable. The purpose of this is to create an entity Object, in Proof Of
 * Concept context, not a list.
 * </p>
 *
 * @param <T>
 *            the element type
 *
 * @since Jul 27, 2016
 * @author Gilles
 *
 */
public abstract class Generic<T> implements Comparable<Generic<T>>, Serializable {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 2432145156034640731L;

    /**
     * Returns the element at the specified position in this list.
     * 
     * @param index
     *            index of the element to return
     * @return the element at the specified position in this list
     * @throws IndexOutOfBoundsException
     *             if the index is out of range <tt>(index &lt; 0 || index &gt;=
     *             size())</tt>
     */
    public abstract T get(int index);

    /**
     * Returns an immutable list of parameters
     * 
     * @return The list of elements
     */
    public abstract List<T> getList();

    /**
     * Returns the number of parameters
     * 
     * @return The count of elements
     */
    public abstract int size();

    /**
     * Returns the internal list of parameters
     * 
     * @return The list of elements
     */
    protected abstract List<T> getAll();

    /**
     * <p>
     * Compares the generic based on the elements in order from left to right.
     * The types must be {@code Comparable}.
     * </p>
     * 
     * @param other
     *            the other generic, not null
     * @return negative if this is less, zero if equal, positive if greater,
     *         {@link Integer#MAX_VALUE} if other is {@code null} or has less
     *         elements and {@link Integer#MIN_VALUE} if other has more elements
     */
    @Override
    public int compareTo(final Generic<T> other) {
        if (other == null) {
            return Integer.MAX_VALUE;
        } else if (other == this) {
            return 0;
        }

        int length = this.getAll().size();
        int otherLength = other.getAll().size();

        if (length > otherLength) {
            return Integer.MAX_VALUE;
        } else if (length < otherLength) {
            return Integer.MIN_VALUE;
        }

        final CompareToBuilder compareBuilder = new CompareToBuilder();

        final Iterator<T> iterator = this.getAll().iterator();
        final Iterator<T> otherIterator = other.getAll().iterator();
        while (iterator.hasNext() && otherIterator.hasNext()) {
            compareBuilder.append(iterator.next(), otherIterator.next());
        }

        return compareBuilder.toComparison();
    }

    /**
     * <p>
     * Compares this generic to another. Each sub-objects are checked.
     * </p>
     * 
     * @param obj
     *            the object to compare to, null returns false
     * @return true if the elements of the generic are equal
     */
    @Override
    public boolean equals(final Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj instanceof Generic) {
            final Generic<?> other = (Generic<?>) obj;

            if (this.getAll().size() != other.getAll().size()) {
                return false;
            }

            final EqualsBuilder equalsBuilder = new EqualsBuilder();

            final Iterator<T> iterator = this.getAll().iterator();
            final Iterator<?> otherIterator = other.getAll().iterator();
            while (iterator.hasNext() && otherIterator.hasNext()) {
                equalsBuilder.append(iterator.next(), otherIterator.next());
            }

            return equalsBuilder.isEquals();
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
        return Objects.hash(this.getAll().toArray());
    }

    /**
     * <p>
     * Returns a String representation of this generic using the format
     * {@code ($left,$right)}.
     * </p>
     * 
     * @return a string describing this object, not null
     */
    @Override
    public String toString() {
        return new StringBuilder("(").append(StringUtils.join(this.getAll(), ", ")).append(')').toString();
    }

    /**
     * <p>
     * Formats the receiver using the given format.
     * </p>
     * 
     * <p>
     * This uses {@link java.util.Formattable} to perform the formatting. All
     * variables may be used to embed. Use {@code %1$s} for the first element
     * (key), {@code %2$s} for the second element (value)... The default format
     * used by {@code toString()} is {@code (%1$s,%2$s...)}.
     * </p>
     * 
     * @param format
     *            the format string, optionally containing {@code %1$s},
     *            {@code %2$s}... but not null
     * @return the formatted string, not null
     */
    public String toString(final String format) {
        return String.format(format, this.getAll().toArray());
    }

    /**
     * <p>
     * Obtains an immutable generic of from objects inferring the generic types.
     * </p>
     * 
     * <p>
     * This factory allows the generic to be created using inference to obtain
     * the generic types.
     * </p>
     * 
     * @param <T>
     *            the element type
     * @param objects
     *            the elements, may be null
     * @return a generic formed from the parameters, not null
     */
    @SafeVarargs
    public static <T> ImmutableGeneric<T> of(final T... objects) {
        return new ImmutableGeneric<T>(objects);
    }

    /**
     * <p>
     * Obtains a mutable generic of from objects inferring the generic type.
     * </p>
     * 
     * <p>
     * This factory allows the generic to be created using inference to obtain
     * the generic types.
     * </p>
     * 
     * @param <T>
     *            the element type
     * @param objects
     *            the elements, may be null
     * @return a generic formed from the parameters, not null
     */
    @SafeVarargs
    public static <T> MutableGeneric<T> ofMutable(final T... objects) {
        return new MutableGeneric<T>(objects);
    }
}
