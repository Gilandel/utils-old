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

import fr.landel.utils.commons.StringUtils;
import fr.landel.utils.model.AbstractEntity;

/**
 * Select builder
 *
 * @since 28 nov. 2016
 * @author Gilles
 *
 * 
 * @param <E>
 *            Entity type
 * @param <K>
 *            Primary key type
 */
public class SelectBuilder<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends AbstractBuilder<E, K> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -1827338500155521637L;

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
    public SelectBuilder(final QueryBuilder<?, ?> parentBuilder, final Class<E> entityClass, final CharSequence alias) {
        super(parentBuilder, BuilderType.SELECT, entityClass, alias);

        this.add(SELECT);
        this.addEntity(this.getEntityClass(), this.getAlias());
    }

    /**
     * Constructor
     *
     * @param parentBuilder
     *            the parent builder
     * @param entityClass
     *            the entity class
     * @param alias
     *            the alias
     * @param selection
     *            the selection
     */
    public SelectBuilder(final QueryBuilder<?, ?> parentBuilder, final Class<E> entityClass, final CharSequence alias,
            final CharSequence selection) {
        super(parentBuilder, BuilderType.SELECT, entityClass, alias);

        this.add(SELECT);
        if (StringUtils.isNotEmpty(selection)) {
            this.add(selection);
        }
    }

    /**
     * Constructor
     *
     * @param parentBuilder
     *            the parent builder
     * @param entityClass
     *            the entity class
     * @param alias
     *            the alias
     * @param queryDTO
     *            the Data Transfer Object to create for each result
     */
    public SelectBuilder(final QueryBuilder<?, ?> parentBuilder, final Class<E> entityClass, final CharSequence alias,
            final QueryDTO queryDTO) {
        super(parentBuilder, BuilderType.SELECT, entityClass, alias);

        this.add(SELECT);
        if (queryDTO != null) {
            this.add(queryDTO.toString());
        }
    }

    /**
     * Constructor
     *
     * @param parentBuilder
     *            the parent builder
     * @param entityClass
     *            the entity class
     * @param alias
     *            the alias
     * @param subQuery
     *            the sub-query to inject
     */
    public SelectBuilder(final QueryBuilder<?, ?> parentBuilder, final Class<E> entityClass, final CharSequence alias,
            final QueryBuilder<?, ?> subQuery) {
        super(parentBuilder, BuilderType.SELECT, entityClass, alias);

        this.add(SELECT);
        if (subQuery != null) {
            this.add(subQuery);
        }
    }

    /**
     * From builder.
     * 
     * @return the current query builder
     */
    public FromBuilder<E, K> from() {
        return this.from(null);
    }

    /**
     * From builder.
     * 
     * @param alias
     *            The alias of the entity
     * @return the current query builder
     */
    public FromBuilder<E, K> from(final String alias) {
        return this.from(this.getEntityClass(), alias);
    }

    /**
     * From builder.
     * 
     * @param entityClassLinked
     *            The linked entity class
     * @param alias
     *            The alias of the entity
     * @return the current query builder
     */
    public FromBuilder<E, K> from(final Class<E> entityClassLinked, final String alias) {
        this.add(FROM);
        final CharSequence finalAlias = StringUtils.defaultIfEmpty(alias, this.getAlias());
        this.addEntity(entityClassLinked, finalAlias);

        return new FromBuilder<>(this.getParentBuilder(), entityClassLinked, finalAlias);
    }

    /**
     * Add an entity entity.
     * 
     * @param entityClassSub
     *            entity class
     * @param alias
     *            alias
     * @param <N>
     *            The entity type
     * @param <Y>
     *            The primary key type
     */
    private <N extends AbstractEntity<N, Y>, Y extends Serializable & Comparable<Y>> void addEntity(final Class<N> entityClassSub,
            final CharSequence alias) {

        this.add(entityClassSub.getCanonicalName());
        this.add(AS);
        this.add(StringUtils.defaultIfEmpty(alias, this.getAlias()));
    }
}
