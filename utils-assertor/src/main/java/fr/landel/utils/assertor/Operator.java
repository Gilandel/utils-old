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
import java.util.Map;

/**
 * Assertor operators (and, or &amp; xor). Manages the combining of assertions.
 *
 * @since 1 juil. 2016
 * @author Gilles
 *
 * @param <A>
 *            The assertor type
 * @param <T>
 *            The type of checked object
 */
public class Operator<A extends AbstractAssertObject<A, T>, T> extends EndPoints<A, T> {

    /**
     * Constructor
     *
     * @param assertor
     *            The linked assertor
     */
    public Operator(final A assertor) {
        super(assertor);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one.
     * 
     * @return the linked assertor
     */
    public A and() {
        this.getAssertor().setCondition(AND);

        return this.getAssertor();
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <N>
     *            The number to check
     * @return the new assertor
     */
    public <N extends Number & Comparable<N>> AssertNumber<N> and(final N object) {
        return this.condition(Assertor.that(object), AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <K>
     *            The type of key to check
     * @param <V>
     *            The type of value to check
     * @return the new assertor
     */
    public <K, V> AssertMap<K, V> and(final Map<K, V> object) {
        return this.condition(Assertor.that(object), AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <I>
     *            The type of the iterable to check
     * @param <Z>
     *            The generic type of the iterable to check
     * @return the new assertor
     */
    public <I extends Iterable<Z>, Z> AssertIterable<I, Z> and(final I object) {
        return this.condition(Assertor.that(object), AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z> AssertArray<Z> and(final Z[] object) {
        return this.condition(Assertor.that(object), AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z extends CharSequence> AssertCharSequence<Z> and(final Z object) {
        return this.condition(Assertor.that(object), AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertDate and(final Date object) {
        return this.condition(Assertor.that(object), AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertCalendar and(final Calendar object) {
        return this.condition(Assertor.that(object), AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertBoolean and(final Boolean object) {
        return this.condition(Assertor.that(object), AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z> AssertClass<Z> and(final Class<Z> object) {
        return this.condition(Assertor.that(object), AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <X>
     *            The assertor object
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    @SuppressWarnings("unchecked")
    public <X extends AssertObject<X, Z>, Z> X and(final Z object) {
        return this.condition((X) Assertor.that(object), AND);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one.
     * 
     * @return the linked assertor
     */
    public A or() {
        this.getAssertor().setCondition(OR);

        return this.getAssertor();
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <N>
     *            The number to check
     * @return the new assertor
     */
    public <N extends Number & Comparable<N>> AssertNumber<N> or(final N object) {
        return this.condition(Assertor.that(object), OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <K>
     *            The type of key to check
     * @param <V>
     *            The type of value to check
     * @return the new assertor
     */
    public <K, V> AssertMap<K, V> or(final Map<K, V> object) {
        return this.condition(Assertor.that(object), OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <I>
     *            The type of the iterable to check
     * @param <Z>
     *            The generic type of the iterable to check
     * @return the new assertor
     */
    public <I extends Iterable<Z>, Z> AssertIterable<I, Z> or(final I object) {
        return this.condition(Assertor.that(object), OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z> AssertArray<Z> or(final Z[] object) {
        return this.condition(Assertor.that(object), OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z extends CharSequence> AssertCharSequence<Z> or(final Z object) {
        return this.condition(Assertor.that(object), OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertDate or(final Date object) {
        return this.condition(Assertor.that(object), OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertCalendar or(final Calendar object) {
        return this.condition(Assertor.that(object), OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertBoolean or(final Boolean object) {
        return this.condition(Assertor.that(object), OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z> AssertClass<Z> or(final Class<Z> object) {
        return this.condition(Assertor.that(object), OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <X>
     *            The assertor object
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    @SuppressWarnings("unchecked")
    public <X extends AssertObject<X, Z>, Z> X or(final Z object) {
        return this.condition((X) Assertor.that(object), OR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one.
     * 
     * @return the linked assertor
     */
    public A xor() {
        this.getAssertor().setCondition(XOR);

        return this.getAssertor();
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <N>
     *            The number to check
     * @return the new assertor
     */
    public <N extends Number & Comparable<N>> AssertNumber<N> xor(final N object) {
        return this.condition(Assertor.that(object), XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <K>
     *            The type of key to check
     * @param <V>
     *            The type of value to check
     * @return the new assertor
     */
    public <K, V> AssertMap<K, V> xor(final Map<K, V> object) {
        return this.condition(Assertor.that(object), XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <I>
     *            The type of the iterable to check
     * @param <Z>
     *            The generic type of the iterable to check
     * @return the new assertor
     */
    public <I extends Iterable<Z>, Z> AssertIterable<I, Z> xor(final I object) {
        return this.condition(Assertor.that(object), XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z> AssertArray<Z> xor(final Z[] object) {
        return this.condition(Assertor.that(object), XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z extends CharSequence> AssertCharSequence<Z> xor(final Z object) {
        return this.condition(Assertor.that(object), XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertDate xor(final Date object) {
        return this.condition(Assertor.that(object), XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertCalendar xor(final Calendar object) {
        return this.condition(Assertor.that(object), XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertBoolean xor(final Boolean object) {
        return this.condition(Assertor.that(object), XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z> AssertClass<Z> xor(final Class<Z> object) {
        return this.condition(Assertor.that(object), XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <X>
     *            The assertor object
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    @SuppressWarnings("unchecked")
    public <X extends AssertObject<X, Z>, Z> X xor(final Z object) {
        return this.condition((X) Assertor.that(object), XOR);
    }

    @SuppressWarnings("unchecked")
    private <X extends AssertObject<X, Z>, Z> X condition(final X assertor, final int condition) {
        assertor.combine(condition, (X) this.getAssertor());

        return assertor;
    }
}