/*-
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
import java.util.Arrays;

import fr.landel.utils.commons.StringUtils;
import fr.landel.utils.model.AbstractEntity;

public class ClauseBuilder<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends AbstractEndBuilder<E, K> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 3127594802439882495L;

    /**
     * Constructor
     *
     * @param parentBuilder
     *            the parent builder
     * @param entityClass
     *            the entity class
     * @param alias
     *            the alias
     */
    public ClauseBuilder(final QueryBuilder<?, ?> parentBuilder, final Class<E> entityClass, final CharSequence alias) {
        super(parentBuilder, BuilderType.CLAUSE, entityClass, alias);
        parentBuilder.setClause(this);
    }

    /**
     * Build a 'equals' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * column = :parameter
     * </pre>
     * 
     * @param column
     *            the column
     * @param parameter
     *            the parameter name
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> isEqual(final CharSequence column, final CharSequence parameter) {
        return this.applyParameters(column, null, QueryOperator.EQUALS, parameter);
    }

    /**
     * Build a 'equals' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * (query) = :parameter
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param parameter
     *            the parameter name
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> isEqual(
            final QueryBuilder<N, Y> query, final CharSequence parameter) {
        return this.applyParameters(null, query, QueryOperator.EQUALS, parameter);
    }

    /**
     * Build a 'not equals' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * column &lt;&gt; :parameter
     * </pre>
     * 
     * @param column
     *            the column
     * @param parameter
     *            the parameter name
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> isNotEqual(final CharSequence column, final CharSequence parameter) {
        return this.applyParameters(column, null, QueryOperator.NOT_EQUALS, parameter);
    }

    /**
     * Build a 'not equals' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * (query) &lt;&gt; :parameter
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param parameter
     *            the parameter name
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> isNotEqual(
            final QueryBuilder<N, Y> query, final CharSequence parameter) {
        return this.applyParameters(null, query, QueryOperator.NOT_EQUALS, parameter);
    }

    /**
     * Build a 'is greater than' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * column &gt; :parameter
     * </pre>
     * 
     * @param column
     *            the column
     * @param parameter
     *            the parameter name
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> isGT(final CharSequence column, final CharSequence parameter) {
        return this.applyParameters(column, null, QueryOperator.GREATER, parameter);
    }

    /**
     * Build a 'is greater than' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * (query) &gt; :parameter
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param parameter
     *            the parameter name
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> isGT(final QueryBuilder<N, Y> query,
            final CharSequence parameter) {
        return this.applyParameters(null, query, QueryOperator.GREATER, parameter);
    }

    /**
     * Build a 'is greater than or equals' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * column &gt;= :parameter
     * </pre>
     * 
     * @param column
     *            the column
     * @param parameter
     *            the parameter name
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> isGTE(final CharSequence column, final CharSequence parameter) {
        return this.applyParameters(column, null, QueryOperator.GREATER_EQUALS, parameter);
    }

    /**
     * Build a 'is greater than or equals' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * (query) &gt;= :parameter
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param parameter
     *            the parameter name
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> isGTE(
            final QueryBuilder<N, Y> query, final CharSequence parameter) {
        return this.applyParameters(null, query, QueryOperator.GREATER_EQUALS, parameter);
    }

    /**
     * Build a 'is lower than' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * column &lt; :parameter
     * </pre>
     * 
     * @param column
     *            the column
     * @param parameter
     *            the parameter name
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> isLT(final CharSequence column, final CharSequence parameter) {
        return this.applyParameters(column, null, QueryOperator.LOWER, parameter);
    }

    /**
     * Build a 'is lower than' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * (query) &lt; :parameter
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param parameter
     *            the parameter name
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> isLT(final QueryBuilder<N, Y> query,
            final CharSequence parameter) {
        return this.applyParameters(null, query, QueryOperator.LOWER, parameter);
    }

    /**
     * Build a 'is greater than or equals' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * column &lt;= :parameter
     * </pre>
     * 
     * @param column
     *            the column
     * @param parameter
     *            the parameter name
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> isLTE(final CharSequence column, final CharSequence parameter) {
        return this.applyParameters(column, null, QueryOperator.LOWER_EQUALS, parameter);
    }

    /**
     * Build a 'is greater than or equals' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * (query) &lt;= :parameter
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param parameter
     *            the parameter name
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> isLTE(
            final QueryBuilder<N, Y> query, final CharSequence parameter) {
        return this.applyParameters(null, query, QueryOperator.LOWER_EQUALS, parameter);
    }

    /**
     * Build a 'is null' condition
     * 
     * <pre>
     * column IS NULL
     * </pre>
     * 
     * @param column
     *            the column
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> isNull(final CharSequence column) {
        return this.applyParameters(column, null, QueryOperator.IS_NULL);
    }

    /**
     * Build a 'is null' condition
     * 
     * <pre>
     * (query) IS NULL
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> isNull(
            final QueryBuilder<N, Y> query) {
        return this.applyParameters(null, query, QueryOperator.IS_NULL);
    }

    /**
     * Build a 'is not null' condition
     * 
     * <pre>
     * column IS NOT NULL
     * </pre>
     * 
     * @param column
     *            the column
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> isNotNull(final CharSequence column) {
        return this.applyParameters(column, null, QueryOperator.IS_NOT_NULL);
    }

    /**
     * Build a 'is not null' condition
     * 
     * <pre>
     * (query) IS NOT NULL
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> isNotNull(
            final QueryBuilder<N, Y> query) {
        return this.applyParameters(null, query, QueryOperator.IS_NOT_NULL);
    }

    /**
     * Build a 'is true' condition
     * 
     * <pre>
     * column IS TRUE
     * </pre>
     * 
     * @param column
     *            the column
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> isTrue(final CharSequence column) {
        return this.applyParameters(column, null, QueryOperator.IS_TRUE);
    }

    /**
     * Build a 'is true' condition
     * 
     * <pre>
     * (query) IS TRUE
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> isTrue(
            final QueryBuilder<N, Y> query) {
        return this.applyParameters(null, query, QueryOperator.IS_TRUE);
    }

    /**
     * Build a 'is not true' condition
     * 
     * <pre>
     * column IS NOT TRUE
     * </pre>
     * 
     * @param column
     *            the column
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> isNotTrue(final CharSequence column) {
        return this.applyParameters(column, null, QueryOperator.IS_NOT_TRUE);
    }

    /**
     * Build a 'is not true' condition
     * 
     * <pre>
     * (query) IS NOT TRUE
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> isNotTrue(
            final QueryBuilder<N, Y> query) {
        return this.applyParameters(null, query, QueryOperator.IS_NOT_TRUE);
    }

    /**
     * Build a 'is false' condition
     * 
     * <pre>
     * column IS FALSE
     * </pre>
     * 
     * @param column
     *            the column
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> isFalse(final CharSequence column) {
        return this.applyParameters(column, null, QueryOperator.IS_FALSE);
    }

    /**
     * Build a 'is false' condition
     * 
     * <pre>
     * (query) IS FALSE
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> isFalse(
            final QueryBuilder<N, Y> query) {
        return this.applyParameters(null, query, QueryOperator.IS_FALSE);
    }

    /**
     * Build a 'is not false' condition
     * 
     * <pre>
     * column IS NOT FALSE
     * </pre>
     * 
     * @param column
     *            the column
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> isNotFalse(final CharSequence column) {
        return this.applyParameters(column, null, QueryOperator.IS_NOT_FALSE);
    }

    /**
     * Build a 'is not false' condition
     * 
     * <pre>
     * (query) IS NOT FALSE
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> isNotFalse(
            final QueryBuilder<N, Y> query) {

        return this.applyParameters(null, query, QueryOperator.IS_NOT_FALSE);
    }

    /**
     * Build a 'is empty' condition
     * 
     * <pre>
     * column IS EMPTY
     * </pre>
     * 
     * @param column
     *            the column
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> isEmpty(final CharSequence column) {
        return this.applyParameters(column, null, QueryOperator.IS_EMPTY);
    }

    /**
     * Build a 'is empty' condition
     * 
     * <pre>
     * (query) IS EMPTY
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> isEmpty(
            final QueryBuilder<N, Y> query) {

        return this.applyParameters(null, query, QueryOperator.IS_EMPTY);
    }

    /**
     * Build a 'is not empty' condition
     * 
     * <pre>
     * column IS NOT EMPTY
     * </pre>
     * 
     * @param column
     *            the column
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> isNotEmpty(final CharSequence column) {

        return this.applyParameters(column, null, QueryOperator.IS_NOT_EMPTY);
    }

    /**
     * Build a 'is not empty' condition
     * 
     * <pre>
     * (query) IS NOT EMPTY
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> isNotEmpty(
            final QueryBuilder<N, Y> query) {

        return this.applyParameters(null, query, QueryOperator.IS_NOT_EMPTY);
    }

    /**
     * Build a 'exists' condition
     * 
     * <pre>
     * (query) EXISTS
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> exists(
            final QueryBuilder<N, Y> query) {

        return this.applyParameters(null, query, QueryOperator.EXISTS);
    }

    /**
     * Build a 'not exists' condition
     * 
     * <pre>
     * (query) NOT EXISTS
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> notExists(
            final QueryBuilder<N, Y> query) {

        return this.applyParameters(null, query, QueryOperator.NOT_EXISTS);
    }

    /**
     * Build a 'between x and y' condition
     * 
     * <pre>
     * column BETWEEN :parameter1 AND :parameter2
     * </pre>
     * 
     * @param column
     *            the column
     * @param parameter1
     *            the left bound
     * @param parameter2
     *            the right bound
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> between(final CharSequence column, final CharSequence parameter1, final CharSequence parameter2) {
        return this.applyParameters(column, null, QueryOperator.BETWEEN, parameter1, parameter2);
    }

    /**
     * Build a 'between x and y' condition
     * 
     * <pre>
     * (query) BETWEEN :parameter1 AND :parameter2
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param parameter1
     *            the left bound
     * @param parameter2
     *            the right bound
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> between(
            final QueryBuilder<N, Y> query, final CharSequence parameter1, final CharSequence parameter2) {

        return this.applyParameters(null, query, QueryOperator.BETWEEN, parameter1, parameter2);
    }

    /**
     * Build a 'in' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * column IN (:parameter)
     * </pre>
     * 
     * @param column
     *            the column
     * @param parameter
     *            the parameter name
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> in(final CharSequence column, final CharSequence parameter) {
        return this.applyParameters(column, null, QueryOperator.IN, parameter);
    }

    /**
     * Build a 'in' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * (query) IN (:parameter)
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param parameter
     *            the parameter name
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> in(final QueryBuilder<N, Y> query,
            final CharSequence parameter) {

        return this.applyParameters(null, query, QueryOperator.IN, parameter);
    }

    /**
     * Build a 'like' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * column LIKE :parameter
     * </pre>
     * 
     * @param column
     *            the column
     * @param parameter
     *            the parameter name
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> like(final CharSequence column, final CharSequence parameter) {
        return this.applyParameters(column, null, QueryOperator.LIKE, parameter);
    }

    /**
     * Build a 'like' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * (query) LIKE :parameter
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param parameter
     *            the parameter name
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> like(final QueryBuilder<N, Y> query,
            final CharSequence parameter) {

        return this.applyParameters(null, query, QueryOperator.LIKE, parameter);
    }

    /**
     * Build a 'like' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * column operator :parameter
     * </pre>
     * 
     * @param column
     *            the column
     * @param operator
     *            the operator
     * @param parameter
     *            the parameter name
     * @return the query condition instance
     */
    public ClauseBuilder<E, K> of(final CharSequence column, final QueryOperator operator, final CharSequence parameter) {
        return this.applyParameters(column, null, operator, parameter);
    }

    /**
     * Build a 'like' condition
     * 
     * <p>
     * The parameter is automatically prefixed by character colon ':'
     * </p>
     * 
     * <pre>
     * (query) operator :parameter
     * </pre>
     * 
     * @param query
     *            the sub-query
     * @param operator
     *            the operator
     * @param parameter
     *            the parameter name
     * @param <N>
     *            The entity class of the sub-query
     * @param <Y>
     *            The primary key type
     * @return the query condition instance
     */
    public <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> of(final QueryBuilder<N, Y> query,
            final QueryOperator operator, final CharSequence parameter) {
        return this.applyParameters(null, query, operator, parameter);
    }

    private <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> ClauseBuilder<E, K> applyParameters(
            final CharSequence column, final QueryBuilder<N, Y> query, final QueryOperator operator, final CharSequence... parameters) {

        if (parameters != null) {
            operator.isParamsNumberCorrect(parameters.length);

            CharSequence[] params = validateParameters(parameters);

            if (column != null && operator.getColumnApplier() != null) {
                operator.getColumnApplier().accept(column, params, this.getBuilder());
            } else if (query != null && operator.getQueryApplier() != null) {
                operator.getQueryApplier().accept(query, params, this.getBuilder());
            }
        } else {
            add(operator.toString());
        }

        return this;
    }

    /**
     * Searches in parameters invalid characters.
     * 
     * @param parameters
     *            the parameters list
     * @return {@code true} if valid, {@code false} otherwise
     */
    protected static CharSequence[] validateParameters(final CharSequence[] parameters) {
        CharSequence[] params = new CharSequence[parameters.length];
        for (int i = 0; i < parameters.length; i++) {
            if (StringUtils.isBlank(parameters[i])) {
                throw new IllegalArgumentException("At least one parameter is blank");
            }
            if (parameters[i].charAt(0) == COLON) {
                params[i] = parameters[i];
                if (params[i].length() < 2) {
                    throw new IllegalArgumentException("At least one parameter is incorrect");
                }
            } else {
                params[i] = new StringBuilder().append(COLON).append(parameters[i]);
            }
            for (int j = 1; j < params[i].length(); j++) {
                if (Arrays.binarySearch(AUTHORIZED_CHARACTERS, params[i].charAt(j)) < 0) {
                    throw new IllegalArgumentException(new StringBuilder("The parameter '").append(params[i])
                            .append("' contains unauthorized characters, supported: (:?[\\w\\d_]+)").toString());
                }
            }
        }
        return params;
    }

    /**
     * @return or logical operator
     */
    public ClauseBuilder<E, K> or() {
        this.add(OR);

        return this;
    }

    /**
     * @return and logical operator
     */
    public ClauseBuilder<E, K> and() {
        this.add(AND);

        return this;
    }
}
