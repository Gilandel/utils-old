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

import java.io.Serializable;
import java.util.ArrayList;

import org.apache.commons.lang3.StringUtils;

import fr.landel.utils.model.AbstractEntity;

/**
 * Query condition.
 *
 * @since 30 nov. 2015
 * @author Gilles
 *
 */
public class QueryCondition extends ArrayList<String> {

    /**
     * The equal operator
     */
    public static final String EQUAL = QueryBuilder.EQUAL;

    /**
     * The null value
     */
    public static final String NULL = "NULL";

    /**
     * The not equal operator
     */
    public static final String NOT_EQUAL = "!=";

    /**
     * The superior operator
     */
    public static final String SUPERIOR = ">";

    /**
     * The superior and equal operator
     */
    public static final String SUPERIOR_EQUAL = ">=";

    /**
     * The inferior operator
     */
    public static final String INFERIOR = "<";

    /**
     * The inferior and equal operator
     */
    public static final String INFERIOR_EQUAL = "<=";

    /**
     * The like operator
     */
    public static final String LIKE = "LIKE";

    /**
     * The in operator
     */
    public static final String IN = "IN";

    /**
     * The is operator
     */
    public static final String IS = "IS";

    /**
     * The is not operator
     */
    public static final String IS_NOT = "IS NOT";

    /**
     * The exists operator
     */
    public static final String EXISTS = "EXISTS";

    /**
     * The not exists operator
     */
    public static final String NOT_EXISTS = "NOT " + EXISTS;

    /**
     * Serial
     */
    private static final long serialVersionUID = 2202040080323964100L;

    /**
     * Constructor (adds automatically colon before parameter name if not
     * found).
     * 
     * @param column
     *            The column name
     * @param operator
     *            The operator
     * @param parameter
     *            The name of value parameter
     */
    public QueryCondition(final String column, final String operator, final String parameter) {
        add(column);
        add(operator);

        if (IN.equalsIgnoreCase(operator)) {
            add(QueryBuilder.PARENTHESIS_OPEN);
        }

        if (parameter.startsWith(QueryBuilder.COLON) || NULL.equalsIgnoreCase(parameter)) {
            add(parameter);
        } else {
            add(QueryBuilder.COLON + parameter);
        }

        if (IN.equalsIgnoreCase(operator)) {
            add(QueryBuilder.PARENTHESIS_CLOSE);
        }
    }

    /**
     * Constructor (adds automatically colon before parameter name if not
     * found).
     * 
     * @param query
     *            The sub-query
     * @param operator
     *            The operator
     * @param parameter
     *            The name of value parameter
     * @param <E>
     *            The entity class of the sub-query
     * @param <K>
     *            The primary key type
     */
    public <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> QueryCondition(final QueryBuilder<E, K> query,
            final String operator, final String parameter) {
        add(QueryBuilder.PARENTHESIS_OPEN);
        addAll(query);
        add(QueryBuilder.PARENTHESIS_CLOSE);
        add(operator);
        if (parameter.startsWith(QueryBuilder.COLON)) {
            add(parameter);
        } else {
            add(QueryBuilder.COLON + parameter);
        }
    }

    /**
     * Constructor, create exists or not exists condition.
     * 
     * @param query
     *            The sub-query
     * @param exists
     *            True = adds exists, otherwise = adds not exists
     * @param <E>
     *            The entity class of the sub-query
     * @param <K>
     *            The primary key type
     */
    public <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> QueryCondition(final Boolean exists,
            final QueryBuilder<E, K> query) {
        if (exists) {
            add(EXISTS);
        } else {
            add(NOT_EXISTS);
        }

        add(QueryBuilder.PARENTHESIS_OPEN);
        addAll(query);
        add(QueryBuilder.PARENTHESIS_CLOSE);
    }

    /**
     * Constructor (uses equal operator) (adds automatically colon before
     * parameter name if not found).
     * 
     * @param column
     *            The column name
     * @param parameter
     *            The name of value parameter
     */
    public QueryCondition(final String column, final String parameter) {
        this(column, EQUAL, parameter);
    }

    /**
     * Constructor (uses equal operator) (adds automatically colon before
     * parameter name if not found).
     * 
     * @param query
     *            The sub-query
     * @param parameter
     *            The name of value parameter
     * @param <E>
     *            The entity class of the sub-query
     * @param <K>
     *            The primary key type
     */
    public <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> QueryCondition(final QueryBuilder<E, K> query,
            final String parameter) {
        this(query, EQUAL, parameter);
    }

    /**
     * Constructor. (avoid this one: No check).
     * 
     * @param condition
     *            The condition
     */
    public QueryCondition(final String condition) {
        add(condition);
    }

    @Override
    public String toString() {
        return StringUtils.join(this, QueryBuilder.SPACE);
    }
}
