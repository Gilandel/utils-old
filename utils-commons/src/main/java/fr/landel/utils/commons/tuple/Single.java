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
 * A single consisting of one element.
 * </p>
 * 
 * <p>
 * This class is an abstract implementation defining the basic API. It refers to
 * the element as 'single'.
 * </p>
 * 
 * <p>
 * Subclass implementations may be mutable or immutable. However, there is no
 * restriction on the type of the stored objects that may be stored. If mutable
 * objects are stored in the single, then the single itself effectively becomes
 * mutable.
 * </p>
 *
 * @param <T>
 *            the element type
 *
 * @since Jul 26, 2016
 * @author Gilles
 *
 */
public abstract class Single<T> implements Comparable<Single<T>>, Serializable {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 7769585576006841244L;

    /**
     * <p>
     * Gets the element from this single.
     * </p>
     * 
     * @return the element, may be null
     */
    public abstract T get();

    /**
     * <p>
     * Sets the element into this single.
     * </p>
     * 
     * @param element
     *            the new element, may be null
     * @return the previous element, may be null
     */
    protected abstract T set(T element);

    /**
     * <p>
     * Compares the single based on the element. The types must be
     * {@code Comparable}.
     * </p>
     * 
     * @param other
     *            the other single, may be null
     * @return negative if this is less, zero if equal, positive if greater and
     *         if other is {@code null}, returns {@link Integer#MAX_VALUE}
     */
    @Override
    public int compareTo(final Single<T> other) {
        if (other == null) {
            return Integer.MAX_VALUE;
        }

        return new CompareToBuilder().append(this.get(), other.get()).toComparison();
    }

    /**
     * <p>
     * Compares this single to another based on the element.
     * </p>
     * 
     * @param obj
     *            the object to compare to, null returns false
     * @return true if the elements of the single are equal
     */
    @Override
    public boolean equals(final Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj instanceof Single<?>) {
            final Single<?> other = (Single<?>) obj;
            return new EqualsBuilder().append(this.get(), other.get()).isEquals();
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
        return Objects.hashCode(this.get());
    }

    /**
     * <p>
     * Returns a String representation of this single using the format
     * {@code ($single)}.
     * </p>
     * 
     * @return a string describing this object, not null
     */
    @Override
    public String toString() {
        return new StringBuilder().append('(').append(get()).append(')').toString();
    }

    /**
     * <p>
     * Formats the receiver using the given format.
     * </p>
     * 
     * <p>
     * This uses {@link java.util.Formattable} to perform the formatting. One
     * variable may be used to embed the element. Use {@code %1$s} for the
     * element. The default format used by {@code toString()} is {@code (%1$s)}.
     * </p>
     * 
     * @param format
     *            the format string, optionally containing {@code %1$s}, not
     *            null
     * @return the formatted string, not null
     */
    public String toString(final String format) {
        return String.format(format, get());
    }

    /**
     * <p>
     * Obtains an immutable single of from one object inferring the generic
     * types.
     * </p>
     * 
     * <p>
     * This factory allows the single to be created using inference to obtain
     * the generic types.
     * </p>
     * 
     * @param <T>
     *            the element type
     * @param element
     *            the element, may be null
     * @return a single formed from the parameter, not null
     */
    public static <T> Single<T> of(final T element) {
        return new ImmutableSingle<T>(element);
    }

    /**
     * <p>
     * Obtains a mutable single of from one object inferring the generic type.
     * </p>
     * 
     * <p>
     * This factory allows the single to be created using inference to obtain
     * the generic types.
     * </p>
     * 
     * @param <T>
     *            the element type
     * @param element
     *            the element, may be null
     * @return a single formed from the parameter, not null
     */
    public static <T> MutableSingle<T> ofMutable(final T element) {
        return new MutableSingle<T>(element);
    }
}
