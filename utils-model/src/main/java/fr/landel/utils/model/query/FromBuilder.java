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

import fr.landel.utils.model.AbstractEntity;

public class FromBuilder<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends AbstractEndBuilder<E, K> {

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
    public FromBuilder(final QueryBuilder<?, ?> parentBuilder, final Class<E> entityClass, final CharSequence alias) {
        super(parentBuilder, BuilderType.FROM, entityClass, alias);
        parentBuilder.setFrom(this);
    }

    /**
     * Join builder.
     * 
     * @param joinObject
     *            The object to join
     * @param joinAlias
     *            The join alias (Optional)
     * @return the current query builder
     */
    public FromBuilder<E, K> join(final String joinObject, final String joinAlias) {
        this.add(JOIN);
        this.add(joinObject);
        if (joinAlias != null) {
            this.add(AS);
            this.add(joinAlias);
        }

        return this;
    }

    /**
     * Left join builder.
     * 
     * @param joinObject
     *            The object to join
     * @param joinAlias
     *            The join alias (Optional)
     * @return the current query builder
     */
    public FromBuilder<E, K> leftJoin(final String joinObject, final String joinAlias) {
        this.add(LEFT);
        this.join(joinObject, joinAlias);

        return this;
    }

    /**
     * Inner join builder.
     * 
     * @param joinObject
     *            The object to join
     * @param joinAlias
     *            The join alias (Optional)
     * @return the current query builder
     */
    public FromBuilder<E, K> innerJoin(final String joinObject, final String joinAlias) {
        this.add(INNER);
        return this.join(joinObject, joinAlias);
    }

    /**
     * Right join builder.
     * 
     * @param joinObject
     *            The object to join
     * @param joinAlias
     *            The join alias (Optional)
     * @return the current query builder
     */
    public FromBuilder<E, K> rightJoin(final String joinObject, final String joinAlias) {
        this.add(RIGHT);
        return this.join(joinObject, joinAlias);
    }

    /**
     * Left outer join builder.
     * 
     * @param joinObject
     *            The object to join
     * @param joinAlias
     *            The join alias (Optional)
     * @return the current query builder
     */
    public FromBuilder<E, K> leftOuterJoin(final String joinObject, final String joinAlias) {
        this.add(LEFT);
        this.add(OUTER);
        return this.join(joinObject, joinAlias);
    }

    /**
     * Right outer join builder.
     * 
     * @param joinObject
     *            The object to join
     * @param joinAlias
     *            The join alias (Optional)
     * @return the current query builder
     */
    public FromBuilder<E, K> rightOuterJoin(final String joinObject, final String joinAlias) {
        this.add(RIGHT);
        this.add(OUTER);
        return this.join(joinObject, joinAlias);
    }

    /**
     * Where builder.
     * 
     * @return the query condition builder
     */
    public ClauseBuilder<E, K> where() {
        this.add(WHERE);
        return new ClauseBuilder<>(this.getParentBuilder(), this.getEntityClass(), this.getAlias());
    }

    /**
     * Where builder.
     * 
     * @param clauseBuilder
     *            the clause builder
     * @return the query condition builder
     */
    public <E1 extends AbstractEntity<E1, K1>, K1 extends Serializable & Comparable<K1>> EndBuilder<E, K> where(
            final ClauseBuilder<E, K> clauseBuilder) {
        this.add(WHERE);

        this.getParentBuilder().setClause(clauseBuilder);

        return new EndBuilder<>(this.getParentBuilder(), this.getEntityClass(), this.getAlias());
    }
}
