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
 * Based on {@link java.util.Optional}. The aim is to know if a functional
 * method returns a result (even null) or not.
 * 
 * <p>
 * {@link #empty()} method create an empty object, so {@link #isPresent()}
 * returns {@code false} (unchanged).
 * </p>
 * <p>
 * {@link #of(Object)} unsupports {@code null} and {@link #isPresent()} returns
 * {@code true} (unchanged).
 * </p>
 * <p>
 * {@link #ofNullable(Object)} supports {@code null} and {@link #isPresent()}
 * returns in all cases {@code true} (changed).
 * </p>
 *
 * @since Jan 26, 2017
 * @author Gilles
 *
 * @param <T>
 *            the type of result object
 */
public final class Result<T> {
    /**
     * Common instance for {@code empty()}.
     */
    private static final Result<?> EMPTY = new Result<>();

    /**
     * If the current is empty or not
     */
    private final boolean empty;

    /**
     * If non-null, the value; if null, indicates no value is present
     */
    private final T value;

    /**
     * Constructs an empty instance.
     *
     * @implNote Generally only one empty instance, {@link Result#EMPTY}, should
     *           exist per VM.
     */
    private Result() {
        this.empty = true;
        this.value = null;
    }

    /**
     * Constructs an instance with the value present.
     *
     * @param value
     *            the non-null value to be present
     */
    private Result(final T value) {
        this.value = value;
        this.empty = false;
    }

    /**
     * Returns an empty {@link Result} instance. No value is present for this
     * {@link Result}.
     *
     * <p>
     * Though it may be tempting to do so, avoid testing if an object is empty
     * by comparing with {@code ==} against instances returned by
     * {@code Result.empty()}. There is no guarantee that it is a singleton.
     * Instead, use {@link #isPresent()}.
     * </p>
     *
     * @param <T>
     *            Type of the non-existent value
     * @return an empty {@code Optional}
     */
    public static <T> Result<T> empty() {
        @SuppressWarnings("unchecked")
        Result<T> t = (Result<T>) EMPTY;
        return t;
    }

    /**
     * Returns an {@link Result} with the specified present non-null value.
     *
     * @param <T>
     *            the class of the value
     * @param value
     *            the value to be present, which must be non-null
     * @return an {@link Result} with the value present
     * @throws NullPointerException
     *             if value is null
     */
    public static <T> Result<T> of(final T value) {
        return new Result<>(Objects.requireNonNull(value));
    }

    /**
     * Returns an {@link Result} describing the specified value, if non-null,
     * otherwise returns an empty {@link Result}.
     *
     * @param <T>
     *            the class of the value
     * @param value
     *            the possibly-null value to describe
     * @return an {@link Result} with a present value if the specified value is
     *         non-null, otherwise an empty {@link Result}
     */
    public static <T> Result<T> ofNullable(final T value) {
        return new Result<>(value);
    }

    /**
     * If a value is present in this {@link Result}, returns the value,
     * otherwise throws {@code NoSuchElementException}.
     *
     * @return the non-null value held by this {@link Result}
     * @throws NoSuchElementException
     *             if there is no value present
     *
     * @see Result#isPresent()
     */
    public T get() {
        if (this.empty) {
            throw new NoSuchElementException("No value present");
        }
        return this.value;
    }

    /**
     * Return {@code true} if the value is empty or {@code null}, otherwise
     * {@code false}.
     *
     * @return {@code true} if the value is empty or {@code null}, otherwise
     *         {@code false}
     */
    public boolean isNull() {
        return this.empty || this.value == null;
    }

    /**
     * Return {@code true} if the value is not empty and not {@code null},
     * otherwise {@code false}.
     *
     * @return {@code true} if the value is not empty and not {@code null},
     *         otherwise {@code false}
     */
    public boolean isNotNull() {
        return !this.isNull();
    }

    /**
     * If a value is present and not {@code null}, invoke the specified consumer
     * with the value, otherwise do nothing.
     *
     * @param consumer
     *            block to be executed if a value is present
     * @throws NullPointerException
     *             if value is present and {@code consumer} is null
     */
    public void ifNotNull(final Consumer<? super T> consumer) {
        if (this.isNotNull())
            consumer.accept(this.value);
    }

    /**
     * Return {@code true} if there is a value present, otherwise {@code false}.
     * The value can be {@code null}.
     *
     * @return {@code true} if there is a value present, otherwise {@code false}
     */
    public boolean isPresent() {
        return !this.empty;
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
        if (!this.empty)
            consumer.accept(this.value);
    }

    /**
     * If a value is present, and the value matches the given predicate, return
     * an {@link Result} describing the value, otherwise return an empty
     * {@link Result}. The value may be {@code null}.
     *
     * @param predicate
     *            a predicate to apply to the value, if present
     * @return an {@link Result} describing the value of this {@link Result} if
     *         a value is present and the value matches the given predicate,
     *         otherwise an empty {@link Result}
     * @throws NullPointerException
     *             if the predicate is null
     */
    public Result<T> filter(final Predicate<? super T> predicate) {
        Objects.requireNonNull(predicate);
        if (!isPresent())
            return this;
        else
            return predicate.test(this.value) ? this : empty();
    }

    /**
     * If a value is present, apply the provided mapping function to it, and
     * return an {@link Result} describing the result. Otherwise return an empty
     * {@link Result}.
     *
     * @param <U>
     *            The type of the result of the mapping function
     * @param mapper
     *            a mapping function to apply to the value, if present
     * @return an {@link Result} describing the result of applying a mapping
     *         function to the value of this {@link Result}, if a value is
     *         present, otherwise an empty {@link Result}
     * @throws NullPointerException
     *             if the mapping function is null
     */
    public <U> Result<U> map(final Function<? super T, ? extends U> mapper) {
        Objects.requireNonNull(mapper);
        if (!isPresent())
            return empty();
        else {
            return Result.ofNullable(mapper.apply(this.value));
        }
    }

    /**
     * If a value is present, apply the provided {@link Result}-bearing mapping
     * function to it, return that result, otherwise return an empty
     * {@link Result}. This method is similar to {@link #map(Function)}, but the
     * provided mapper is one whose result is already an {@link Result}, and if
     * invoked, {@code flatMap} does not wrap it with an additional
     * {@link Result}.
     *
     * @param <U>
     *            The type parameter to the {@link Result} returned by
     * @param mapper
     *            a mapping function to apply to the value, if present the
     *            mapping function
     * @return the result of applying an {@link Result}-bearing mapping function
     *         to the value of this {@link Result}, if a value is present,
     *         otherwise an empty {@link Result}
     * @throws NullPointerException
     *             if the mapping function is null or returns a null result
     */
    public <U> Result<U> flatMap(final Function<? super T, Result<U>> mapper) {
        Objects.requireNonNull(mapper);
        if (!isPresent())
            return empty();
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
        return !this.empty ? this.value : other;
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
     *             if value is not present and {@code other} is {@code null}
     */
    public T orElseGet(final Supplier<? extends T> other) {
        Objects.requireNonNull(other);
        return !this.empty ? this.value : other.get();
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
        if (!this.empty) {
            return this.value;
        } else {
            throw exceptionSupplier.get();
        }
    }

    /**
     * Indicates whether some other object is "equal to" this Result. The other
     * object is considered equal if:
     * <ul>
     * <li>it is also an {@link Result} and;
     * <li>both instances have no value present or;
     * <li>the present values are "equal to" each other via {@code equals()}.
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

        if (!(obj instanceof Result)) {
            return false;
        }

        Result<?> other = (Result<?>) obj;
        return new EqualsBuilder2<>(this, other).append(o -> o.empty).append(o -> o.value).build();
    }

    /**
     * Returns the hash code value of the present value, if any, or 0 (zero) if
     * no value is present.
     *
     * @return hash code value of the present value or 0 if no value is present
     */
    @Override
    public int hashCode() {
        return Objects.hashCode(this.value);
    }

    /**
     * Returns a non-empty string representation of this Result suitable for
     * debugging. The exact presentation format is unspecified and may vary
     * between implementations and versions.
     *
     * <p>
     * If a value is present the result must include its string representation
     * in the result. Empty and present Results must be unambiguously
     * differentiable.
     * </p>
     *
     * @return the string representation of this instance
     */
    @Override
    public String toString() {
        return !this.empty ? StringUtils.inject("Result: {}", this.value) : "Result.empty";
    }
}
