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

/**
 * 
 * List of operators
 *
 * @since Aug 7, 2016
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

    private final String key;

    private EnumOperator(final String key) {
        this.key = key;
    }

    /**
     * @return the operator message key
     */
    public String getKey() {
        return this.key;
    }

    @Override
    public String toString() {
        return Constants.getProperty(this.key).toString();
    }
}
