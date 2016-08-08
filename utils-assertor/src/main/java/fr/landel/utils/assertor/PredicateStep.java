/*-
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

import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Supplier;

import org.apache.commons.lang3.tuple.Triple;

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
 * false</li>
 * <li>{@link PredicateStep#isOK}: to get the boolean result of the assertion
 * {@code true} or {@code false}</li>
 * <li>{@link PredicateStep#getErrors}: to get the error message (precondition
 * message or message depending of error type)</li>
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
    Supplier<AssertorResult<T>> getStep();

    /**
     * The only purpose is to avoid the copy of basic methods into children
     * interfaces. This is an indirect way to create specific
     * {@link PredicateStep} by overriding this interface. All children class
     * has to override this method
     * 
     * @param supplier
     *            the supplier
     * @return the predicate step
     */
    @SuppressWarnings("unchecked")
    default S get(final Supplier<AssertorResult<T>> supplier) {
        return (S) (PredicateStep<S, T>) () -> supplier;
    }

    /**
     * Returns if the assertion is valid or not
     * 
     * @return true, if valid
     */
    default boolean isOK() {
        final AssertorResult<T> step = this.getStep().get();
        return step.isPreconditionOK() && step.isValid();
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
     * @return a {@code String} containing the errors
     */
    default String getErrors() {
        final AssertorResult<T> step = this.getStep().get();
        if (!step.isPreconditionOK()) {
            return AssertorHelper.getMessage(AssertorConstants.DEFAULT_ASSERTION, Assertor.getLocale(), step.getPreconditionMessage(),
                    step.getParameters(), null);
        } else if (!step.isValid()) {
            return AssertorHelper.getMessage(AssertorConstants.DEFAULT_ASSERTION, Assertor.getLocale(), step.getMessage(),
                    step.getParameters(), null);
        }
        return "";
    }

    /**
     * Throw an {@link IllegalArgumentException} with assertion errors as
     * message, only if assertion is wrong.
     * 
     * <pre>
     * Assertor.that("text").isBlank().toThrow(); // throw an exception
     * Assertor.that("text").isNotBlank().toThrow(); // do nothing
     * </pre>
     */
    default void toThrow() {
        this.toThrow((CharSequence) null);
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
     */
    default void toThrow(final CharSequence message, final Object... arguments) {
        this.toThrow(null, message, arguments);
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
     * // -&gt; throw an exception with message: text should be blank
     * 
     * Assertor.that(26.354f).isGT(27f).toThrow(Locale.US, "param '%1$.2f*' should be %s than '%2$.2f'", "greater");
     * // -&gt; throw an exception with message: param '26.35' should be greater
     * // than '27.00'
     * 
     * Assertor.that(26.354f).isGT(27f).toThrow(Locale.FRANCE, "param '%1$.2f*' should be %s than '%2$.2f'", "greater");
     * // -&gt; throw an exception with message: param '26,35' should be greater
     * // than '27,00'
     * 
     * Assertor.that("text").isNotBlank().toThrow(Locale.US, "text should be blank");
     * // -&gt; do nothing
     * </pre>
     * 
     * @param locale
     *            the message locale
     * @param message
     *            the message to thrown
     * @param arguments
     *            the messages arguments
     */
    default void toThrow(final Locale locale, final CharSequence message, final Object... arguments) {
        final AssertorResult<T> step = this.getStep().get();
        if (!step.isPreconditionOK() || !step.isValid()) {
            final String error;
            if (message != null) {
                error = AssertorHelper.getMessage(AssertorConstants.DEFAULT_ASSERTION, locale, message, step.getParameters(), arguments);
            } else if (!step.isPreconditionOK()) {
                error = AssertorHelper.getMessage(AssertorConstants.DEFAULT_ASSERTION, locale, step.getPreconditionMessage(),
                        step.getParameters(), null);
            } else {
                error = AssertorHelper.getMessage(AssertorConstants.DEFAULT_ASSERTION, locale, step.getMessage(), step.getParameters(),
                        null);
            }
            throw new IllegalArgumentException(error);
        }
    }

    /**
     * Calls the function and throw the specific exception, only if assertion is
     * wrong. The function provide two data:
     * <ul>
     * <li>first: the current error message</li>
     * <li>second: the list of parameters as {@link Triple} of:
     * <ul>
     * <li>object to check</li>
     * <li>the type of object</li>
     * <li>if it's a checked object or a parameter</li>
     * </ul>
     * </li>
     * </ul>
     * 
     * <pre>
     * Assertor.that("text").isBlank().toThrow((errors, parameters) -&gt; new MyException("text should be blank"));
     * // -&gt; throw a MyException with message: text should be blank
     * 
     * Assertor.that("text").isBlank().toThrow((errors, parameters) -&gt; new MyException(errors));
     * // -&gt; throw a MyException with errors message: the char sequence 'text'
     * // should be null, empty or blank
     * 
     * Assertor.that("text").isNotBlank().toThrow((errors, parameters) -&gt; new MyException("text should be blank"));
     * // -&gt; do nothing
     * </pre>
     * 
     * @param function
     *            the function to apply if assertion is wrong
     * @param <E>
     *            the generic exception type
     * @throws E
     *             The type of exception to throw
     */
    default <E extends Throwable> void toThrow(final BiFunction<String, List<Triple<Object, EnumType, Boolean>>, E> function) throws E {
        final AssertorResult<T> step = this.getStep().get();
        if (!step.isPreconditionOK() || !step.isValid()) {
            final E exception;
            if (!step.isPreconditionOK()) {
                exception = function.apply(String.valueOf(step.getPreconditionMessage()), step.getParameters());
            } else {
                exception = function.apply(String.valueOf(step.getMessage()), step.getParameters());
            }
            exception.addSuppressed(new IllegalArgumentException(AssertorHelper.getMessage(AssertorConstants.DEFAULT_ASSERTION,
                    Assertor.getLocale(), step.getMessage(), step.getParameters(), null)));
            throw exception;
        }
    }

    /**
     * Throw the specific exception, only if assertion is wrong.
     * 
     * <pre>
     * Assertor.that("text").isBlank().toThrow(new MyException("text should be blank"));
     * // -&gt; throw a MyException with message: text should be blank
     * 
     * Assertor.that("text").isNotBlank().toThrow(new MyException("text should be blank"));
     * // -&gt; do nothing
     * </pre>
     * 
     * @param exception
     *            the specific exception
     * @param injectSuppressed
     *            if internal exception is added to the specific exception as
     *            suppressed
     * @param <E>
     *            the generic exception type
     * @throws E
     *             the type of exception to throw
     */
    default <E extends Throwable> void toThrow(final E exception, final boolean injectSuppressed) throws E {
        final AssertorResult<T> step = this.getStep().get();
        if (!step.isPreconditionOK() || !step.isValid()) {
            if (exception != null) {
                if (injectSuppressed) {
                    exception.addSuppressed(new IllegalArgumentException(AssertorHelper.getMessage(AssertorConstants.DEFAULT_ASSERTION,
                            Assertor.getLocale(), step.getMessage(), step.getParameters(), null)));
                }
                throw exception;
            } else {
                this.toThrow();
            }
        }
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
     * // false &gt; false and false = false &gt; false or true = true)
     * 
     * // ('' null or not empty) and ('text' null or not empty)
     * Assertor.that("").isNull().or().isNotEmpty().and(Assertor.that("text").isNull().or().isNotEmpty()).isOK();
     * // -&gt; false (because: (false or false) and (false or true) &gt; false and
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
        return this.get(AssertorHelper.and(this.getStep(), other.getStep()));
    }

    default <X, R extends PredicateStep<R, X>> PredicateAssertor<R, X> and(final X other) {
        return () -> AssertorHelper.and(this.getStep(), other);
    }

    default PredicateAssertorBoolean and(final Boolean other) {
        return () -> AssertorHelper.and(this.getStep(), other);
    }

    default <X extends CharSequence> PredicateAssertorCharSequence<X> and(final X other) {
        return () -> AssertorHelper.and(this.getStep(), other);
    }

    default <N extends Number & Comparable<N>> PredicateAssertorNumber<N> and(final N other) {
        return () -> AssertorHelper.and(this.getStep(), other);
    }

    default <X> PredicateAssertorArray<X> and(final X[] other) {
        return () -> AssertorHelper.and(this.getStep(), other);
    }

    default <X> PredicateAssertorClass<X> and(final Class<X> other) {
        return () -> AssertorHelper.and(this.getStep(), other);
    }

    default <K, V> PredicateAssertorMap<K, V> and(final Map<K, V> other) {
        return () -> AssertorHelper.and(this.getStep(), other);
    }

    default <X> PredicateAssertorIterable<X> and(final Iterable<X> other) {
        return () -> AssertorHelper.and(this.getStep(), other);
    }

    default PredicateAssertorDate and(final Date other) {
        return () -> AssertorHelper.and(this.getStep(), other);
    }

    default PredicateAssertorCalendar and(final Calendar other) {
        return () -> AssertorHelper.and(this.getStep(), other);
    }

    default PredicateAssertor<S, T> and() {
        return () -> AssertorHelper.and(this.getStep());
    }

    /**
     * Applies a predicate step in the current one with the operator OR. The aim
     * of this is to provide the equivalence of parenthesis in condition
     * expressions.
     * 
     * <pre>
     * // '' empty or 'text' not empty and contains 'r'
     * Assertor.that("").isEmpty().or("text").isNotEmpty().and().contains("r").isOK();
     * // -&gt; false (because: true or true and false &gt; true or true = true &gt; true
     * // and false = false)
     * 
     * // '' empty or ('text' not empty and contains 'r')
     * Assertor.that("").isEmpty().or(Assertor.that("text").isNotEmpty().and().contains("r")).isOK();
     * // -&gt; true (because: true or (true and false) &gt; (true and false) =
     * // false &gt; true or false = true)
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
        return this.get(AssertorHelper.or(this.getStep(), other.getStep()));
    }

    default <X, R extends PredicateStep<R, X>> PredicateAssertor<R, X> or(final X other) {
        return () -> AssertorHelper.or(this.getStep(), other);
    }

    default PredicateAssertorBoolean or(final Boolean other) {
        return () -> AssertorHelper.or(this.getStep(), other);
    }

    default <X extends CharSequence> PredicateAssertorCharSequence<X> or(final X other) {
        return () -> AssertorHelper.or(this.getStep(), other);
    }

    default <N extends Number & Comparable<N>> PredicateAssertorNumber<N> or(final N other) {
        return () -> AssertorHelper.or(this.getStep(), other);
    }

    default <X> PredicateAssertorArray<X> or(final X[] other) {
        return () -> AssertorHelper.or(this.getStep(), other);
    }

    default <X> PredicateAssertorClass<X> or(final Class<X> other) {
        return () -> AssertorHelper.or(this.getStep(), other);
    }

    default <K, V> PredicateAssertorMap<K, V> or(final Map<K, V> other) {
        return () -> AssertorHelper.or(this.getStep(), other);
    }

    default <X> PredicateAssertorIterable<X> or(final Iterable<X> other) {
        return () -> AssertorHelper.or(this.getStep(), other);
    }

    default PredicateAssertorDate or(final Date other) {
        return () -> AssertorHelper.or(this.getStep(), other);
    }

    default PredicateAssertorCalendar or(final Calendar other) {
        return () -> AssertorHelper.or(this.getStep(), other);
    }

    default PredicateAssertor<S, T> or() {
        return () -> AssertorHelper.or(this.getStep());
    }

    /**
     * Applies a predicate step in the current one with the operator XOR. The
     * aim of this is to provide the equivalence of parenthesis in condition
     * expressions.
     * 
     * <pre>
     * // '' empty xor 'text' not empty and contains 'r'
     * Assertor.that("").isEmpty().xor("text").isNotEmpty().and().contains("r").isOK();
     * // -&gt; false (because: true xor true and false &gt; true xor true = false &gt;
     * // false and false = false)
     * 
     * // '' empty xor ('text' not empty and contains 'r')
     * Assertor.that("").isEmpty().xor(Assertor.that("text").isNotEmpty().and().contains("r")).isOK();
     * // -&gt; true (because: true xor (true and false) &gt; (true and false) =
     * // false &gt; true xor false = true)
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
    default <X, R extends PredicateStep<R, X>> S xor(final PredicateStep<R, X> other) {
        return this.get(AssertorHelper.xor(this.getStep(), other.getStep()));
    }

    default <X, R extends PredicateStep<R, X>> PredicateAssertor<R, X> xor(final X other) {
        return () -> AssertorHelper.xor(this.getStep(), other);
    }

    default PredicateAssertorBoolean xor(final Boolean other) {
        return () -> AssertorHelper.xor(this.getStep(), other);
    }

    default <X extends CharSequence> PredicateAssertorCharSequence<X> xor(final X other) {
        return () -> AssertorHelper.xor(this.getStep(), other);
    }

    default <N extends Number & Comparable<N>> PredicateAssertorNumber<N> xor(final N other) {
        return () -> AssertorHelper.xor(this.getStep(), other);
    }

    default <X> PredicateAssertorArray<X> xor(final X[] other) {
        return () -> AssertorHelper.xor(this.getStep(), other);
    }

    default <X> PredicateAssertorClass<X> xor(final Class<X> other) {
        return () -> AssertorHelper.xor(this.getStep(), other);
    }

    default <K, V> PredicateAssertorMap<K, V> xor(final Map<K, V> other) {
        return () -> AssertorHelper.xor(this.getStep(), other);
    }

    default <X> PredicateAssertorIterable<X> xor(final Iterable<X> other) {
        return () -> AssertorHelper.xor(this.getStep(), other);
    }

    default PredicateAssertorDate xor(final Date other) {
        return () -> AssertorHelper.xor(this.getStep(), other);
    }

    default PredicateAssertorCalendar xor(final Calendar other) {
        return () -> AssertorHelper.xor(this.getStep(), other);
    }

    default PredicateAssertor<S, T> xor() {
        return () -> AssertorHelper.xor(this.getStep());
    }
}