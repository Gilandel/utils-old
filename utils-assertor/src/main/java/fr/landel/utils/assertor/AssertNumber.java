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

import fr.landel.utils.commons.Comparators;

/**
 * Assertion utility class that assists in validating arguments for numbers.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 * @param <N>
 *            The type of each number <code>Byte</code>, <code>Short</code>,
 *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
 *            <code>Double</code>, <code>BigInteger</code> or
 *            <code>BigDecimal</code>. Supports new <code>Number</code>, types
 *            only if it implements <code>Comparable</code>.
 */
public class AssertNumber<N extends Number & Comparable<N>> extends AssertObject<AssertNumber<N>, N> {

    /**
     * 
     * Constructor
     *
     * @param number
     *            the number to check
     */
    protected AssertNumber(final N number) {
        super(number);
    }

    /**
     * Assert that the first number is equal to the second one.
     * 
     * <pre>
     * Assertor.that(10).isEqual(20).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param number
     *            The second number message the exception message, use the
     *            default assertion if null
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isEqual(final N number) {
        return this.combine(Comparators.compare(this.get(), number) == 0, new StringBuilder("number1 '").append(this.getParam())
                .append("' is not equal to number2 '").append(AssertObject.getParam(this.getParamIndex() + 1)).append("'"), number);
    }

    /**
     * Assert that the first number is not equal to the second one.
     * 
     * <pre>
     * Assertor.that(10).isNotEqual(10).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param number
     *            The second number message the exception message, use the
     *            default assertion if null
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isNotEqual(final N number) {
        return this.combine(Comparators.compare(this.get(), number) != 0, new StringBuilder("number1 '").append(this.getParam())
                .append("' is equal to number2 '").append(AssertObject.getParam(this.getParamIndex() + 1)).append("'"), number);
    }

    /**
     * Assert that the first number is greater than the second one.
     * 
     * <pre>
     * Assertor.that(10).isGT(10).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param number
     *            The second number
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isGT(final N number) {
        return this.combine(
                Comparators.compare(this.get(), number) > 0, new StringBuilder("number1 '").append(this.getParam())
                        .append("' is not greater than number2 '").append(AssertObject.getParam(this.getParamIndex() + 1)).append("'"),
                number);
    }

    /**
     * Assert that the first number is greater than or equal to the second one.
     * 
     * <pre>
     * Assertor.that(10).isGTE(10).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param number
     *            The second number
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isGTE(final N number) {
        return this.combine(Comparators.compare(this.get(), number) >= 0, new StringBuilder("number1 '").append(this.getParam())
                .append("' is not greater than or equal to number2 '").append(AssertObject.getParam(this.getParamIndex() + 1)).append("'"),
                number);
    }

    /**
     * Assert that the first number is lower than the second one.
     * 
     * <pre>
     * Assertor.that(10).isLT(10).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param number
     *            The second number
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isLT(final N number) {
        return this.combine(
                Comparators.compare(this.get(), number) < 0, new StringBuilder("number1 '").append(this.getParam())
                        .append("' is not lower than number2 '").append(AssertObject.getParam(this.getParamIndex() + 1)).append("'"),
                number);
    }

    /**
     * Assert that the first number is lower than or equal to the second one.
     * 
     * <pre>
     * Assertor.that(10).isLTE(10).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param number
     *            The second number
     * @return the operator
     */
    public Operator<AssertNumber<N>, N> isLTE(final N number) {
        return this.combine(Comparators.compare(this.get(), number) <= 0, new StringBuilder("number1 '").append(this.getParam())
                .append("' is not lower than or equal to number2 '").append(AssertObject.getParam(this.getParamIndex() + 1)).append("'"),
                number);
    }
}
