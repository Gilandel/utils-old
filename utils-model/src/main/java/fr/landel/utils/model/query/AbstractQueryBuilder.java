/*
 * #%L
 * utils-model
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.model.query;

import java.util.ArrayList;

import fr.landel.utils.commons.StringUtils;

/**
 * The abstract query builder
 *
 * @since 30 nov. 2015
 * @author Gilles
 *
 */
public abstract class AbstractQueryBuilder extends ArrayList<String> {

    /**
     * The opened parenthesis character
     */
    public static final String PARENTHESIS_OPEN = "(";

    /**
     * The closed parenthesis character
     */
    public static final String PARENTHESIS_CLOSE = ")";

    /**
     * The 'all' character
     */
    public static final String ALL = "*";

    /**
     * The space character
     */
    protected static final String SPACE = " ";

    /**
     * The colon character
     */
    protected static final String COLON = ":";

    /**
     * The equal operator
     */
    protected static final String EQUAL = "=";

    /**
     * Serial
     */
    private static final long serialVersionUID = 629818342327662943L;

    @Override
    public String toString() {
        return StringUtils.join(this, SPACE);
    }
}
