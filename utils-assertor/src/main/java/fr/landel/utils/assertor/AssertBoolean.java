/*
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

/**
 * Assertion utility class that assists in validating arguments for booleans.
 *
 * @since 14 mai 2016
 * @author Gilles
 */
public class AssertBoolean extends AssertObject<AssertBoolean, Boolean> {

    /**
     * 
     * Constructor
     *
     * @param condition
     *            The condition to check
     */
    protected AssertBoolean(final Boolean condition) {
        super(condition);
    }

    /**
     * Assert a boolean expression.
     * 
     * <pre>
     * Assertor.that(i &gt; 0).isTrue().toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertBoolean, Boolean> isFalse() {
        return this.combine(Boolean.FALSE.equals(this.get()), "this expression must be false");
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code false}.
     * 
     * <pre>
     * Assertor.that(i &gt; 0).isTrue().toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertBoolean, Boolean> isTrue() {
        return this.combine(Boolean.TRUE.equals(this.get()), "this expression must be true");
    }
}
