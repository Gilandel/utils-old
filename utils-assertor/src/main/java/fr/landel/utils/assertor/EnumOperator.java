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

/**
 * 
 * List of operators
 *
 * @since 7 août 2016
 * @author Gilles
 *
 */
public enum EnumOperator {

    /**
     * And operator
     */
    AND("operator.and"),

    /**
     * Or operator
     */
    OR("operator.or"),

    /**
     * Xor operator
     */
    XOR("operator.xor");

    private final String text;

    private EnumOperator(final String text) {
        this.text = text;
    }

    @Override
    public String toString() {
        return AssertorConstants.getProperty(this.text).toString();
    }
}
