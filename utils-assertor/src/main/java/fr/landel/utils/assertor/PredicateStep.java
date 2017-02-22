/*-
 * #%L
 * utils-assertor
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.assertor;

import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Supplier;

import fr.landel.utils.commons.Result;
import fr.landel.utils.commons.StringUtils;

/**
 * This class is an intermediate or final link in chain, see
 * {@link PredicateAssertor}.
 * 
 * <p>
 * Intermediate by using operators:
 * </p>
 * <ul>
 * <li>{@link PredicateStep#and}: applied the AND operator between the previous
 * and the next assertion</li>
 * <li>{@link PredicateStep#or}: applied the OR operator between the previous
 * and the next assertion</li>
 * <li>{@link PredicateStep#xor}: applied the XOR operator between the previous
 * and the next assertion</li>
 * </ul>
 * 
 * <p>
 * Final by using methods:
 * </p>
 * <ul>
 * <li>{@link PredicateStep#toThrow}: to throw an exception if assertion is
 * false, or to get the checked value otherwise.</li>
 * <li>{@link PredicateStep#isOK}: to get the boolean result of the assertion
 * {@code true} or {@code false}.</li>
 * <li>{@link PredicateStep#getErrors}: to get the error message (precondition
 * message or message depending of error type).</li>
 * <li>{@link PredicateStep#get}: to get the result as an {@link Optional}
 * object. The result is set to empty if the assertion failed or if the checked
 * value is {@code null}.</li>
 * <li>{@link PredicateStep#result}: to get the result as a {@link Result}
 * object. The result is set to empty only if the assertion failed.</li>
 * </ul>
 *
 * @since Aug 7, 2016
 * @author Gilles
 *
 * @param <S>
 *            the type of predicate step
 * @param <T>
 *            the type of checked object
 */
@FunctionalInterface
public interface PredicateStep<S extends PredicateStep<S, T>, T> {

    /**
     * @return the step result
     */
    StepAssertor<T> getStep();

    /**
     * The only purpose is to avoid the copy of basic methods into children
     * interfaces. This is an indirect way to create specific
     * {@link PredicateStep} by overriding this interface. All children class
     * has to override this method
     * 
     * @param result
     *            the result
     * @return the predicate step
     */
    @SuppressWarnings("unchecked")
    default S get(final StepAssertor<T> result) {
        return (S) (PredicateStep<S, T>) () -> result;
    }

    /**
     * Returns if the assertion is valid or not
     * 
     * @return true, if valid
     */
    default boolean isOK() {
        final ResultAssertor result = HelperAssertor.combine(this.getStep(), false);
        return result.isPrecondition() && result.isValid();
    }

    /**
     * Returns the errors. If preconditions are invalid, all others errors in
     * checked steps are ignored.
     * 
     * <pre>
     * Assertor.that("toto part en vacances").contains("toto").and().contains("voyage")
     *         .and(Assertor.that("text").isBlank().or().not().contains("text")).getErrors();
     * 
     * // the message -&gt; "the char sequence 'toto part en vacances' should
     * // contain 'voyage' AND (the char sequence 'text' should be null, empty
     * // or blank OR the char sequence 'text' should NOT contain 'text')"
     * </pre>
     * 
     * @return an {@link Optional} containing the errors message
     */
    default Optional<String> getErrors() {
        final String message = HelperAssertor.combine(this.getStep(), true).getMessage();
        if (StringUtils.isNotEmpty(message)) {
            return Optional.of(message);
        }
        return Optional.empty();
    }

    /**
     * Throw an {@link IllegalArgumentException} with assertion errors as
     * message, only if assertion is wrong.
     * 
     * <pre>
     * Assertor.that("text").isBlank().toThrow(); // throws an exception
     * Assertor.that("text").isNotBlank().toThrow(); // returns "text"
     * </pre>
     * 
     * @return the last checked object
     */
    default T toThrow() {
        return this.toThrow((CharSequence) null);
    }

    /**
     * Throw an {@link IllegalArgumentException} with message and arguments as
     * message, only if assertion is wrong. The arguments will be injected throw
     * the {@link String#format(String, Object...)} method. Parameters can also
     * be injected by using the same syntax, just add an asterisk/star at the
     * end.
     * 
     * <pre>
     * Assertor.that("text").isBlank().toThrow("text should be blank");
     * // -&gt; throw an exception with message: text should be blank
     * 
     * Assertor.that("text").isBlank().toThrow("param '%1$s*' should be %s", "blank");
     * // -&gt; throw an exception with message: param 'text' should be blank
     * 
     * Assertor.that("text").isNotBlank().toThrow("text should be blank");
     * // -&gt; do nothing
     * </pre>
     * 
     * @param message
     *            the message to thrown
     * @param arguments
     *            the messages arguments
     * @return the last checked object
     */
    default T toThrow(final CharSequence message, final Object... arguments) {
        return this.toThrow(null, message, arguments);
    }

    /**
     * Throw an {@link IllegalArgumentException} with message and arguments as
     * message, only if assertion is wrong. The arguments will be injected throw
     * the {@link String#format(Locale, String, Object...)} method. Parameters
     * can also be injected by using the same syntax, just add an asterisk/star
     * at the end.
     * 
     * <pre>
     * Assertor.that("text").isBlank().toThrow(Locale.US, "text should be blank");
     * // -&gt; throws an exception with message: text should be blank
     * 
     * Assertor.that(26.354f).isGT(27f).toThrow(Locale.US, "param '%1$.2f*' should be %s than '%2$.2f'", "greater");
     * // -&gt; throws an exception with message: param '26.35' should be greater
     * // than '27.00'
     * 
     * Assertor.that(26.354f).isGT(27f).toThrow(Locale.FRANCE, "param '%1$.2f*' should be %s than '%2$.2f'", "greater");
     * // -&gt; throws an exception with message: param '26,35' should be greater
     * // than '27,00'
     * 
     * Assertor.that("text").isNotBlank().toThrow(Locale.US, "text should be blank");
     * // -&gt; returns "text"
     * </pre>
     * 
     * @param locale
     *            the message locale
     * @param message
     *            the message to thrown
     * @param arguments
     *            the messages arguments
     * @return the last checked object
     */
    default T toThrow(final Locale locale, final CharSequence message, final Object... arguments) {
        final ResultAssertor result = HelperAssertor.combine(this.getStep(), message == null);

        if (!result.isPrecondition() || !result.isValid()) {
            final String error;
            if (message != null) {
                error = HelperMessage.getMessage(ConstantsAssertor.DEFAULT_ASSERTION, locale, message, result.getParameters(), arguments);
            } else {
                error = result.getMessage();
            }
            throw new IllegalArgumentException(error);
        }

        return HelperAssertor.getLastChecked(result.getParameters());
    }

    /**
     * Calls the function and throw the specific exception, only if assertion is
     * wrong. The function provide two data:
     * <ul>
     * <li>first: the current error message</li>
     * <li>second: the list of parameters as {@link ParameterAssertor} of:
     * <ul>
     * <li>object to check</li>
     * <li>the type of object</li>
     * <li>if it's a checked object or a parameter</li>
     * </ul>
     * </li>
     * </ul>
     * 
     * <p>
     * The function inject original exception as suppressed
     * </p>
     * 
     * <pre>
     * Assertor.that("text").isBlank().toThrow((errors, parameters) -&gt; new MyException("text should be blank"));
     * // -&gt; throws a MyException with message: text should be blank
     * 
     * Assertor.that("text").isBlank().toThrow((errors, parameters) -&gt; new MyException(errors));
     * // -&gt; throws a MyException with errors message: the char sequence 'text'
     * // should be null, empty or blank
     * 
     * Assertor.that("text").isNotBlank().toThrow((errors, parameters) -&gt; new MyException("text should be blank"));
     * // -&gt; returns "text"
     * </pre>
     * 
     * @param function
     *            the function to apply if assertion is wrong (required, cannot
     *            be {@code null}, throw a {@link NullPointerException})
     * @return the last checked object
     * @param <E>
     *            the generic exception type
     * @throws E
     *             The type of exception to throw
     */
    default <E extends Throwable> T toThrow(final BiFunction<String, List<ParameterAssertor<?>>, E> function) throws E {
        Objects.requireNonNull(function);

        final ResultAssertor result = HelperAssertor.combine(this.getStep(), true);

        if (!result.isPrecondition() || !result.isValid()) {
            final E exception = function.apply(result.getMessage(), result.getParameters());
            exception.addSuppressed(new IllegalArgumentException(result.getMessage()));

            throw exception;
        }

        return HelperAssertor.getLastChecked(result.getParameters());
    }

    /**
     * Calls the function and throw the specific exception, only if assertion is
     * wrong. The function provide any data (back side, no message is generated,
     * so this cost less performance).
     * 
     * <pre>
     * Assertor.that("text").isBlank().toThrow((s) -&gt; new MyException("text should be blank"));
     * // -&gt; throw a MyException with message: text should be blank
     * 
     * Assertor.that("text").isNotBlank().toThrow(() -&gt; new MyException("text should be blank"));
     * // -&gt; do nothing
     * </pre>
     * 
     * @param supplier
     *            the supplier to call if assertion is wrong (required, cannot
     *            be {@code null}, throw a {@link NullPointerException})
     * @return the last checked object
     * @param <E>
     *            the generic exception type
     * @throws E
     *             The type of exception to throw
     */
    default <E extends Throwable> T toThrow(final Supplier<E> supplier) throws E {
        Objects.requireNonNull(supplier);

        final ResultAssertor result = HelperAssertor.combine(this.getStep(), false);

        if (!result.isPrecondition() || !result.isValid()) {
            throw supplier.get();
        }

        return HelperAssertor.getLastChecked(result.getParameters());
    }

    /**
     * Throw the specific exception, only if assertion is wrong and inject or
     * not the internal exception to the specified one as suppressed. The
     * suppressed can be read through the method
     * {@link Throwable#getSuppressed()}.
     * 
     * <pre>
     * Assertor.that("text").isBlank().toThrow(new MyException("text should be blank"), true);
     * // -&gt; throw a MyException with message: text should be blank and inject
     * // the original exception as suppressed
     * 
     * Assertor.that("text").isNotBlank().toThrow(new MyException("text should be blank"), true);
     * // -&gt; do nothing
     * </pre>
     * 
     * @param exception
     *            the specific exception
     * @param injectSuppressed
     *            if internal exception is added to the specific exception as
     *            suppressed
     * @return the last checked object
     * @param <E>
     *            the generic exception type
     * @throws E
     *             the type of exception to throw
     */
    default <E extends Throwable> T toThrow(final E exception, final boolean injectSuppressed) throws E {
        final ResultAssertor result = HelperAssertor.combine(this.getStep(), exception == null || injectSuppressed);

        if (!result.isPrecondition() || !result.isValid()) {
            if (exception != null) {
                if (injectSuppressed) {
                    exception.addSuppressed(new IllegalArgumentException(result.getMessage()));
                }
                throw exception;
            } else {
                this.toThrow();
            }
        }

        return HelperAssertor.getLastChecked(result.getParameters());
    }

    /**
     * Get the object as {@link Result}. This method is similar to
     * {@link #get()}. The difference is even if the result is {@code null} it's
     * not considered as empty.
     * 
     * <pre>
     * Assertor.that("text").isBlank().result().isPresent();
     * // = false
     * Assertor.that("text").isBlank().result().get();
     * // =&gt; throw a NoSuchElementException exception
     * Assertor.that("text").isBlank().result().orElse("default");
     * // = "default"
     * 
     * Assertor.that("text").isNotBlank().result().isPresent();
     * // = true
     * Assertor.that("text").isNotBlank().result().get();
     * // = "text"
     * Assertor.that("text").isNotBlank().result().orElse("default");
     * // = "text"
     * 
     * // Difference with get() method which returns an Optional object
     * Assertor.that((String) null).isBlank().result().orElse("default");
     * // = null
     * Assertor.that((String) null).isNotBlank().result().orElse("default");
     * // = "default"
     * </pre>
     * 
     * @return The result
     */
    default Result<T> result() {
        final ResultAssertor result = HelperAssertor.combine(this.getStep(), false);

        if (!result.isPrecondition() || !result.isValid()) {
            return Result.empty();
        }

        return Result.ofNullable(HelperAssertor.getLastChecked(result.getParameters()));
    }

    /**
     * Get the object as {@link Optional}. This method is similar to
     * {@link #result()}. The difference is if the result is {@code null} it's
     * considered as empty.
     * 
     * <pre>
     * Assertor.that("text").isBlank().get().isPresent();
     * // = false
     * Assertor.that("text").isBlank().get().get();
     * // =&gt; throw a NoSuchElementException exception
     * Assertor.that("text").isBlank().get().orElse("default");
     * // = "default"
     * 
     * Assertor.that("text").isNotBlank().get().isPresent();
     * // = true
     * Assertor.that("text").isNotBlank().get().get();
     * // = "text"
     * Assertor.that("text").isNotBlank().get().orElse("default");
     * // = "text"
     * 
     * // Difference with result() method which returns a Result object
     * Assertor.that((String) null).isBlank().get().orElse("default");
     * // = "default"
     * Assertor.that((String) null).isNotBlank().get().orElse("default");
     * // = "default"
     * </pre>
     * 
     * @return The optional
     */
    default Optional<T> get() {
        final ResultAssertor result = HelperAssertor.combine(this.getStep(), false);

        if (!result.isPrecondition() || !result.isValid()) {
            return Optional.empty();
        }

        return Optional.ofNullable(HelperAssertor.getLastChecked(result.getParameters()));
    }

    /**
     * Applies a predicate step in the current one with the operator AND. The
     * aim of this is to provide the equivalence of parenthesis in condition
     * expressions.
     * 
     * <pre>
     * // '' null or not empty and 'text' null or not empty
     * Assertor.that("").isNull().or().isNotEmpty().and("text").isNull().or().isNotEmpty().isOK();
     * // -&gt; true (because: false or false and false or true, false or false =
     * // false =&gt; false and false = false =&gt; false or true = true)
     * 
     * // ('' null or not empty) and ('text' null or not empty)
     * Assertor.that("").isNull().or().isNotEmpty().and(Assertor.that("text").isNull().or().isNotEmpty()).isOK();
     * // -&gt; false (because: (false or false) and (false or true) =&gt; false and
     * // true = false)
     * 
     * </pre>
     * 
     * @param other
     *            the other predicate step
     * @param <X>
     *            The type of other checked object
     * @param <R>
     *            The {@linkplain PredicateStep} type
     * @return this predicate step with the other injected
     */
    default <X, R extends PredicateStep<R, X>> S and(final PredicateStep<R, X> other) {
        return this.get(HelperAssertor.and(this.getStep(), other.getStep()));
    }

    /**
     * Append an operator 'AND' on the current step with the ability to check
     * another object.
     * 
     * @param other
     *            the other or next checked object to check
     * @param <X>
     *            the object type
     * @param <R>
     *            the type of predicate
     * @return the predicate assertor
     */
    default <X, R extends PredicateStep<R, X>> PredicateAssertor<R, X> and(final X other) {
        return () -> HelperAssertor.and(this.getStep(), other, EnumType.getType(other));
    }

    /**
     * Append an operator 'AND' on the current step with the ability to check a
     * {@link Boolean}.
     * 
     * @param other
     *            the other or next checked {@link Boolean} to check
     * @return the predicate assertor
     */
    default PredicateAssertorBoolean and(final Boolean other) {
        return () -> HelperAssertor.and(this.getStep(), other, EnumType.BOOLEAN);
    }

    /**
     * Append an operator 'AND' on the current step with the ability to check a
     * {@link CharSequence}.
     * 
     * @param other
     *            the other or next checked {@link CharSequence} to check
     * @param <X>
     *            the {@link CharSequence} type
     * @return the predicate assertor
     */
    default <X extends CharSequence> PredicateAssertorCharSequence<X> and(final X other) {
        return () -> HelperAssertor.and(this.getStep(), other, EnumType.CHAR_SEQUENCE);
    }

    /**
     * Append an operator 'AND' on the current step with the ability to check a
     * {@link Number}.
     * 
     * @param other
     *            the other or next checked {@link Number} to check
     * @param <N>
     *            the {@link Number} type
     * @return the predicate assertor
     */
    default <N extends Number & Comparable<N>> PredicateAssertorNumber<N> and(final N other) {
        return () -> HelperAssertor.and(this.getStep(), other, EnumType.getType(other));
    }

    /**
     * Append an operator 'AND' on the current step with the ability to check an
     * {@code array}.
     * 
     * @param other
     *            the other or next checked {@code array} to check
     * @param <X>
     *            the array elements type
     * @return the predicate assertor
     */
    default <X> PredicateAssertorArray<X> and(final X[] other) {
        return () -> HelperAssertor.and(this.getStep(), other, EnumType.ARRAY);
    }

    /**
     * Append an operator 'AND' on the current step with the ability to check a
     * {@link Class}.
     * 
     * @param other
     *            the other or next checked {@link Class} to check
     * @param <X>
     *            the {@link Class} type
     * @return the predicate assertor
     */
    default <X> PredicateAssertorClass<X> and(final Class<X> other) {
        return () -> HelperAssertor.and(this.getStep(), other, EnumType.CLASS);
    }

    /**
     * Append an operator 'AND' on the current step with the ability to check a
     * {@link Map}.
     * 
     * @param other
     *            the other or next checked {@link Map} to check
     * @param <K>
     *            the {@link Map} key elements type
     * @param <V>
     *            the {@link Map} value elements type
     * @return the predicate assertor
     */
    default <K, V> PredicateAssertorMap<K, V> and(final Map<K, V> other) {
        return () -> HelperAssertor.and(this.getStep(), other, EnumType.MAP);
    }

    /**
     * Append an operator 'AND' on the current step with the ability to check an
     * {@link Iterable}.
     * 
     * @param other
     *            the other or next checked {@link Iterable} to check
     * @param <X>
     *            the {@link Iterable} elements type
     * @return the predicate assertor
     */
    default <X> PredicateAssertorIterable<X> and(final Iterable<X> other) {
        return () -> HelperAssertor.and(this.getStep(), other, EnumType.ITERABLE);
    }

    /**
     * Append an operator 'AND' on the current step with the ability to check a
     * {@link Date}.
     * 
     * @param other
     *            the other or next checked {@link Date} to check
     * @return the predicate assertor
     */
    default PredicateAssertorDate and(final Date other) {
        return () -> HelperAssertor.and(this.getStep(), other, EnumType.DATE);
    }

    /**
     * Append an operator 'AND' on the current step with the ability to check a
     * {@link Calendar}.
     * 
     * @param other
     *            the other or next checked {@link Calendar} to check
     * @return the predicate assertor
     */
    default PredicateAssertorCalendar and(final Calendar other) {
        return () -> HelperAssertor.and(this.getStep(), other, EnumType.CALENDAR);
    }

    /**
     * Append an operator 'AND' on the current step with the ability to check an
     * {@link Enum}.
     * 
     * @param other
     *            the other or next checked {@link Enum} to check
     * @param <X>
     *            the type of the {@link Enum}
     * @return the predicate assertor
     */
    default <X extends Enum<X>> PredicateAssertorEnum<X> and(final X other) {
        return () -> HelperAssertor.and(this.getStep(), other, EnumType.ENUMERATION);
    }

    /**
     * Append an operator 'AND' on the current step.
     * 
     * @return the predicate assertor
     */
    default PredicateAssertor<S, T> and() {
        return () -> HelperAssertor.and(this.getStep());
    }

    /**
     * Applies a predicate step in the current one with the operator OR. The aim
     * of this is to provide the equivalence of parenthesis in condition
     * expressions.
     * 
     * <pre>
     * // '' empty or 'text' not empty and contains 'r'
     * Assertor.that("").isEmpty().or("text").isNotEmpty().and().contains("r").isOK();
     * // -&gt; false (because: true or true and false =&gt; true or true = true =&gt;
     * // true
     * // and false = false)
     * 
     * // '' empty or ('text' not empty and contains 'r')
     * Assertor.that("").isEmpty().or(Assertor.that("text").isNotEmpty().and().contains("r")).isOK();
     * // -&gt; true (because: true or (true and false) =&gt; (true and false) =
     * // false =&gt; true or false = true)
     * 
     * </pre>
     * 
     * @param other
     *            the other predicate step
     * @param <X>
     *            The type of other checked object
     * @param <R>
     *            The {@linkplain PredicateStep} type
     * @return this predicate step with the other injected
     */
    default <X, R extends PredicateStep<R, X>> S or(final PredicateStep<R, X> other) {
        return this.get(HelperAssertor.or(this.getStep(), other.getStep()));
    }

    /**
     * Append an operator 'OR' on the current step with the ability to check
     * another object.
     * 
     * @param other
     *            the other or next checked object to check
     * @param <X>
     *            the object type
     * @param <R>
     *            the type of predicate
     * @return the predicate assertor
     */
    default <X, R extends PredicateStep<R, X>> PredicateAssertor<R, X> or(final X other) {
        return () -> HelperAssertor.or(this.getStep(), other, EnumType.getType(other));
    }

    /**
     * Append an operator 'OR' on the current step with the ability to check a
     * {@link Boolean}.
     * 
     * @param other
     *            the other or next checked {@link Boolean} to check
     * @return the predicate assertor
     */
    default PredicateAssertorBoolean or(final Boolean other) {
        return () -> HelperAssertor.or(this.getStep(), other, EnumType.BOOLEAN);
    }

    /**
     * Append an operator 'OR' on the current step with the ability to check a
     * {@link CharSequence}.
     * 
     * @param other
     *            the other or next checked {@link CharSequence} to check
     * @param <X>
     *            the {@link CharSequence} type
     * @return the predicate assertor
     */
    default <X extends CharSequence> PredicateAssertorCharSequence<X> or(final X other) {
        return () -> HelperAssertor.or(this.getStep(), other, EnumType.CHAR_SEQUENCE);
    }

    /**
     * Append an operator 'OR' on the current step with the ability to check a
     * {@link Number}.
     * 
     * @param other
     *            the other or next checked {@link Number} to check
     * @param <N>
     *            the {@link Number} type
     * @return the predicate assertor
     */
    default <N extends Number & Comparable<N>> PredicateAssertorNumber<N> or(final N other) {
        return () -> HelperAssertor.or(this.getStep(), other, EnumType.getType(other));
    }

    /**
     * Append an operator 'OR' on the current step with the ability to check an
     * {@code array}.
     * 
     * @param other
     *            the other or next checked {@code array} to check
     * @param <X>
     *            the array elements type
     * @return the predicate assertor
     */
    default <X> PredicateAssertorArray<X> or(final X[] other) {
        return () -> HelperAssertor.or(this.getStep(), other, EnumType.ARRAY);
    }

    /**
     * Append an operator 'OR' on the current step with the ability to check a
     * {@link Class}.
     * 
     * @param other
     *            the other or next checked {@link Class} to check
     * @param <X>
     *            the {@link Class} type
     * @return the predicate assertor
     */
    default <X> PredicateAssertorClass<X> or(final Class<X> other) {
        return () -> HelperAssertor.or(this.getStep(), other, EnumType.CLASS);
    }

    /**
     * Append an operator 'OR' on the current step with the ability to check a
     * {@link Map}.
     * 
     * @param other
     *            the other or next checked {@link Map} to check
     * @param <K>
     *            the {@link Map} key elements type
     * @param <V>
     *            the {@link Map} value elements type
     * @return the predicate assertor
     */
    default <K, V> PredicateAssertorMap<K, V> or(final Map<K, V> other) {
        return () -> HelperAssertor.or(this.getStep(), other, EnumType.MAP);
    }

    /**
     * Append an operator 'OR' on the current step with the ability to check an
     * {@link Iterable}.
     * 
     * @param other
     *            the other or next checked {@link Iterable} to check
     * @param <X>
     *            the {@link Iterable} elements type
     * @return the predicate assertor
     */
    default <X> PredicateAssertorIterable<X> or(final Iterable<X> other) {
        return () -> HelperAssertor.or(this.getStep(), other, EnumType.ITERABLE);
    }

    /**
     * Append an operator 'OR' on the current step with the ability to check a
     * {@link Date}.
     * 
     * @param other
     *            the other or next checked {@link Date} to check
     * @return the predicate assertor
     */
    default PredicateAssertorDate or(final Date other) {
        return () -> HelperAssertor.or(this.getStep(), other, EnumType.DATE);
    }

    /**
     * Append an operator 'OR' on the current step with the ability to check a
     * {@link Calendar}.
     * 
     * @param other
     *            the other or next checked {@link Calendar} to check
     * @return the predicate assertor
     */
    default PredicateAssertorCalendar or(final Calendar other) {
        return () -> HelperAssertor.or(this.getStep(), other, EnumType.CALENDAR);
    }

    /**
     * Append an operator 'OR' on the current step with the ability to check an
     * {@link Enum}.
     * 
     * @param other
     *            the other or next checked {@link Enum} to check
     * @param <X>
     *            the type of the {@link Enum}
     * @return the predicate assertor
     */
    default <X extends Enum<X>> PredicateAssertorEnum<X> or(final X other) {
        return () -> HelperAssertor.or(this.getStep(), other, EnumType.ENUMERATION);
    }

    /**
     * Append an operator 'OR' on the current step.
     * 
     * @return the predicate assertor
     */
    default PredicateAssertor<S, T> or() {
        return () -> HelperAssertor.or(this.getStep());
    }

    /**
     * Applies a predicate step in the current one with the operator XOR. The
     * aim of this is to provide the equivalence of parenthesis in condition
     * expressions.
     * 
     * <pre>
     * // '' empty xor 'text' not empty and contains 'r'
     * Assertor.that("").isEmpty().xor("text").isNotEmpty().and().contains("r").isOK();
     * // -&gt; false (because: true xor true and false =&gt; true xor true = false =&gt;
     * // false and false = false)
     * 
     * // '' empty xor ('text' not empty and contains 'r')
     * Assertor.that("").isEmpty().xor(Assertor.that("text").isNotEmpty().and().contains("r")).isOK();
     * // -&gt; true (because: true xor (true and false) =&gt; (true and false) =
     * // false =&gt; true xor false = true)
     * </pre>
     * 
     * @param other
     *            the other predicate step
     * @param <X>
     *            The type of other checked object
     * @param <R>
     *            The {@linkplain PredicateStep} type
     * @return this predicate step with the other injected
     */
    default <X, R extends PredicateStep<R, X>> S xor(final PredicateStep<R, X> other) {
        return this.get(HelperAssertor.xor(this.getStep(), other.getStep()));
    }

    /**
     * Append an operator 'XOR' on the current step with the ability to check
     * another object.
     * 
     * @param other
     *            the other or next checked object to check
     * @param <X>
     *            the object type
     * @param <R>
     *            the type of predicate
     * @return the predicate assertor
     */
    default <X, R extends PredicateStep<R, X>> PredicateAssertor<R, X> xor(final X other) {
        return () -> HelperAssertor.xor(this.getStep(), other, EnumType.getType(other));
    }

    /**
     * Append an operator 'XOR' on the current step with the ability to check a
     * {@link Boolean}.
     * 
     * @param other
     *            the other or next checked {@link Boolean} to check
     * @return the predicate assertor
     */
    default PredicateAssertorBoolean xor(final Boolean other) {
        return () -> HelperAssertor.xor(this.getStep(), other, EnumType.BOOLEAN);
    }

    /**
     * Append an operator 'XOR' on the current step with the ability to check a
     * {@link CharSequence}.
     * 
     * @param other
     *            the other or next checked {@link CharSequence} to check
     * @param <X>
     *            the {@link CharSequence} type
     * @return the predicate assertor
     */
    default <X extends CharSequence> PredicateAssertorCharSequence<X> xor(final X other) {
        return () -> HelperAssertor.xor(this.getStep(), other, EnumType.CHAR_SEQUENCE);
    }

    /**
     * Append an operator 'XOR' on the current step with the ability to check a
     * {@link Number}.
     * 
     * @param other
     *            the other or next checked {@link Number} to check
     * @param <N>
     *            the {@link Number} type
     * @return the predicate assertor
     */
    default <N extends Number & Comparable<N>> PredicateAssertorNumber<N> xor(final N other) {
        return () -> HelperAssertor.xor(this.getStep(), other, EnumType.getType(other));
    }

    /**
     * Append an operator 'XOR' on the current step with the ability to check an
     * {@code array}.
     * 
     * @param other
     *            the other or next checked {@code array} to check
     * @param <X>
     *            the array elements type
     * @return the predicate assertor
     */
    default <X> PredicateAssertorArray<X> xor(final X[] other) {
        return () -> HelperAssertor.xor(this.getStep(), other, EnumType.ARRAY);
    }

    /**
     * Append an operator 'XOR' on the current step with the ability to check a
     * {@link Class}.
     * 
     * @param other
     *            the other or next checked {@link Class} to check
     * @param <X>
     *            the {@link Class} type
     * @return the predicate assertor
     */
    default <X> PredicateAssertorClass<X> xor(final Class<X> other) {
        return () -> HelperAssertor.xor(this.getStep(), other, EnumType.CLASS);
    }

    /**
     * Append an operator 'XOR' on the current step with the ability to check a
     * {@link Map}.
     * 
     * @param other
     *            the other or next checked {@link Map} to check
     * @param <K>
     *            the {@link Map} key elements type
     * @param <V>
     *            the {@link Map} value elements type
     * @return the predicate assertor
     */
    default <K, V> PredicateAssertorMap<K, V> xor(final Map<K, V> other) {
        return () -> HelperAssertor.xor(this.getStep(), other, EnumType.MAP);
    }

    /**
     * Append an operator 'XOR' on the current step with the ability to check an
     * {@link Iterable}.
     * 
     * @param other
     *            the other or next checked {@link Iterable} to check
     * @param <X>
     *            the {@link Iterable} elements type
     * @return the predicate assertor
     */
    default <X> PredicateAssertorIterable<X> xor(final Iterable<X> other) {
        return () -> HelperAssertor.xor(this.getStep(), other, EnumType.ITERABLE);
    }

    /**
     * Append an operator 'XOR' on the current step with the ability to check a
     * {@link Date}.
     * 
     * @param other
     *            the other or next checked {@link Date} to check
     * @return the predicate assertor
     */
    default PredicateAssertorDate xor(final Date other) {
        return () -> HelperAssertor.xor(this.getStep(), other, EnumType.DATE);
    }

    /**
     * Append an operator 'XOR' on the current step with the ability to check a
     * {@link Calendar}.
     * 
     * @param other
     *            the other or next checked {@link Calendar} to check
     * @return the predicate assertor
     */
    default PredicateAssertorCalendar xor(final Calendar other) {
        return () -> HelperAssertor.xor(this.getStep(), other, EnumType.CALENDAR);
    }

    /**
     * Append an operator 'XOR' on the current step with the ability to check an
     * {@link Enum}.
     * 
     * @param other
     *            the other or next checked {@link Enum} to check
     * @param <X>
     *            the type of the {@link Enum}
     * @return the predicate assertor
     */
    default <X extends Enum<X>> PredicateAssertorEnum<X> xor(final X other) {
        return () -> HelperAssertor.xor(this.getStep(), other, EnumType.ENUMERATION);
    }

    /**
     * Append an operator 'XOR' on the current step.
     * 
     * @return the predicate assertor
     */
    default PredicateAssertor<S, T> xor() {
        return () -> HelperAssertor.xor(this.getStep());
    }
}
