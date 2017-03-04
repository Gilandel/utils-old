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
package fr.landel.utils.commons;

import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import fr.landel.utils.commons.builder.EqualsBuilder2;

/**
 * Based on {@link java.util.Optional}. The aim is to define a default value in
 * case of the main value is {@code null}.
 * 
 * <p>
 * {@link #empty(Object)} method create an empty object, so {@link #isPresent()}
 * returns {@code false} (unchanged logic).
 * </p>
 * <p>
 * {@link #of(Object)} unsupports {@code null} and {@link #isPresent()} returns
 * {@code true} (unchanged logic). Here, the default value equals the value.
 * </p>
 * <p>
 * {@link #ofNullable(Object, Object)} supports {@code null} and
 * {@link #isPresent()} returns in following if value is {@code null};
 * {@code true} or {@code false}.
 * </p>
 * <p>
 * {@link #get()} returns in all cases a value not {@code null}. The value is
 * not {@code null}, otherwise returns the default one.
 * </p>
 *
 * @since Mar 1, 2017
 * @author Gilles
 *
 * @param <T>
 *            the type of result object
 */
public final class Default<T> {

    /**
     * If non-null, the value; if null, indicates no value is present.
     */
    private final T value;

    /**
     * The value by default, only used if value is null.
     */
    private final T defaultValue;

    /**
     * Constructs an empty instance.
     *
     * @param defaultValue
     *            the default value
     */
    private Default(final T defaultValue) {
        this(null, defaultValue);
    }

    /**
     * Constructs an instance with the value present.
     *
     * @param value
     *            the non-null value to be present
     * @param defaultValue
     *            the default value
     */
    private Default(final T value, final T defaultValue) {
        this.value = value;
        this.defaultValue = Objects.requireNonNull(defaultValue);
    }

    /**
     * Returns an empty {@link Default} instance. No value is present for this
     * {@link Default}.
     *
     * @param defaultValue
     *            the default value
     * @param <T>
     *            Type of the non-existent value
     * @return an empty {@code Optional}
     */
    public static <T> Default<T> empty(final T defaultValue) {
        return new Default<>(defaultValue);
    }

    /**
     * Returns an {@link Default} with the specified present non-null value. The
     * default value is automatically defined with the main value.
     *
     * @param value
     *            the value to be present, which must be non-null
     * @param <T>
     *            the class of the value
     * @return an {@link Default} with the value present
     * @throws NullPointerException
     *             if value is null
     */
    public static <T> Default<T> of(final T value) {
        return new Default<>(Objects.requireNonNull(value), value);
    }

    /**
     * Returns an {@link Default} describing the specified value, if non-null,
     * otherwise returns an empty {@link Default}.
     *
     * @param value
     *            the possibly-null value to describe
     * @param defaultValue
     *            the default value
     * @param <T>
     *            the class of the value
     * @return an {@link Default} with a present value if the specified value is
     *         non-null, otherwise an empty {@link Default}
     */
    public static <T> Default<T> ofNullable(final T value, final T defaultValue) {
        return new Default<>(value, defaultValue);
    }

    /**
     * If a value is present in this {@link Default}, returns the value,
     * otherwise throws {@code NoSuchElementException}.
     *
     * @return the non-null value held by this {@link Default}
     * @throws NoSuchElementException
     *             if there is no value present
     *
     * @see Default#isPresent()
     */
    public T get() {
        return ObjectUtils.defaultIfNull(this.value, defaultValue);
    }

    /**
     * Return the default value
     * 
     * @return the default value
     */
    public T getDefault() {
        return this.defaultValue;
    }

    /**
     * Return {@code true} if there is a value present, otherwise {@code false}.
     *
     * @return {@code true} if there is a value present, otherwise {@code false}
     */
    public boolean isPresent() {
        return this.value != null;
    }

    /**
     * If a value is present, invoke the specified consumer with the value,
     * otherwise do nothing.
     *
     * @param consumer
     *            block to be executed if a value is present
     * @throws NullPointerException
     *             if {@code consumer} is null
     */
    public void ifPresent(final Consumer<? super T> consumer) {
        Objects.requireNonNull(consumer);
        if (this.value != null)
            consumer.accept(this.value);
    }

    /**
     * If a value is not present, invoke the specified consumer with the default
     * value, otherwise do nothing.
     *
     * @param consumer
     *            block to be executed if a value is not present
     * @throws NullPointerException
     *             if {@code consumer} is null
     */
    public void ifAbsent(final Consumer<? super T> consumer) {
        Objects.requireNonNull(consumer);
        if (this.value == null)
            consumer.accept(this.defaultValue);
    }

    /**
     * If a value is present, and the value matches the given predicate, return
     * an {@link Default} describing the value, otherwise return an empty
     * {@link Default} with the current default value. The value may be
     * {@code null}.
     *
     * @param predicate
     *            a predicate to apply to the value, if present
     * @return a {@link Default} describing the value of this {@link Default} if
     *         a value is present and the value matches the given predicate,
     *         otherwise an empty {@link Default}
     * @throws NullPointerException
     *             if the predicate is null
     */
    public Default<T> filter(final Predicate<? super T> predicate) {
        Objects.requireNonNull(predicate);
        if (!isPresent())
            return this;
        else
            return predicate.test(this.value) ? this : empty(this.defaultValue);
    }

    /**
     * If a value is present, apply the provided mapping function to it, and
     * return an {@link Default} describing the result, the mapper is also
     * applied on the default value. Otherwise return an empty {@link Default}
     * with the default value mapped by the provided function.
     *
     * @param <U>
     *            The type of the result of the mapping function
     * @param mapper
     *            a mapping function to apply to values
     * @return a {@link Default} describing the result of applying a mapping
     *         function to the value of this {@link Default}, if a value is
     *         present, otherwise an empty {@link Default} of the default value
     *         mapped
     * @throws NullPointerException
     *             if the mapping function is null
     */
    public <U> Default<U> map(final Function<? super T, ? extends U> mapper) {
        Objects.requireNonNull(mapper);
        if (!isPresent())
            return empty(mapper.apply(this.defaultValue));
        else {
            return Default.ofNullable(mapper.apply(this.value), mapper.apply(this.defaultValue));
        }
    }

    /**
     * If a value is present, apply the provided {@link Default}-bearing mapping
     * function to it, return that result, otherwise return an empty
     * {@link Default} with default value. This method is similar to
     * {@link #map(Function)}, but the provided mapper is one whose result is
     * already an {@link Default}, and if invoked, {@code flatMap} does not wrap
     * it with an additional {@link Default}.
     *
     * @param <U>
     *            The type parameter to the {@link Default} returned by
     * @param mapper
     *            a mapping function to apply to the values
     * @return the result of applying a {@link Default}-bearing mapping function
     *         to the value of this {@link Default}, if a value is present,
     *         otherwise an empty {@link Default} of the default value mapped
     * @throws NullPointerException
     *             if the mapping function is {@code null} or returns a
     *             {@code null} result
     */
    public <U> Default<U> flatMap(final Function<? super T, Default<U>> mapper) {
        Objects.requireNonNull(mapper);
        if (!isPresent())
            return Objects.requireNonNull(mapper.apply(this.defaultValue));
        else {
            return Objects.requireNonNull(mapper.apply(this.value));
        }
    }

    /**
     * Return the value if present, otherwise return {@code other}.
     *
     * @param other
     *            the value to be returned may be {@code null}
     * @return the value, if present, otherwise {@code other}
     */
    public T orElse(final T other) {
        return ObjectUtils.defaultIfNull(this.value, other);
    }

    /**
     * Return the value if present, otherwise invoke {@code other} and return
     * the result of that invocation.
     *
     * @param other
     *            a {@code Supplier} whose result is returned if no value is
     *            present
     * @return the value if present otherwise the result of {@code other.get()}
     * @throws NullPointerException
     *             if {@code other} is {@code null}
     */
    public T orElseGet(final Supplier<? extends T> other) {
        return ObjectUtils.defaultIfNull(this.value, other);
    }

    /**
     * Return the contained value, if present, otherwise throw an exception to
     * be created by the provided supplier.
     *
     * <p>
     * A method reference to the exception constructor with an empty argument
     * list can be used as the supplier. For example,
     * {@code IllegalStateException::new}
     * </p>
     *
     * @param <X>
     *            Type of the exception to be thrown
     * @param exceptionSupplier
     *            The supplier which will return the exception to be thrown
     * @return the present value
     * @throws X
     *             if there is no value present
     * @throws NullPointerException
     *             if no value is present and {@code exceptionSupplier} is
     *             {@code null}
     */
    public <X extends Throwable> T orElseThrow(final Supplier<? extends X> exceptionSupplier) throws X {
        Objects.requireNonNull(exceptionSupplier);
        if (this.value != null) {
            return this.value;
        } else {
            throw exceptionSupplier.get();
        }
    }

    /**
     * Indicates whether some other object is "equal to" this Default. The other
     * object is considered equal if:
     * <ul>
     * <li>it is also an {@link Default} and;
     * <li>both instances have no value present or;
     * <li>the present values are "equal to" each other via {@code equals()};
     * <li>the default values are "equal to" each other via {@code equals()}.
     * </ul>
     *
     * @param obj
     *            an object to be tested for equality
     * @return {code true} if the other object is "equal to" this object
     *         otherwise {@code false}
     */
    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }

        if (!(obj instanceof Default)) {
            return false;
        }

        Default<?> other = (Default<?>) obj;
        return new EqualsBuilder2<>(this, other).append(o -> o.value).append(o -> o.defaultValue).build();
    }

    /**
     * Returns the hash code value of the present value and the default value,
     * if any, or only the hash code of the default value if no value is
     * present.
     *
     * @return hash code value of the present value and the default value
     */
    @Override
    public int hashCode() {
        return Objects.hash(this.value, this.defaultValue);
    }

    /**
     * Returns a non-empty string representation of this Default suitable for
     * debugging. The exact presentation format is unspecified and may vary
     * between implementations and versions.
     *
     * <p>
     * If a value is present the result must include its string representation
     * in the result. Empty and present Defaults must be unambiguously
     * differentiable.
     * </p>
     *
     * @return the string representation of this instance
     */
    @Override
    public String toString() {
        return StringUtils.inject("Default: {} otherwise {}", this.value, this.defaultValue);
    }
}
