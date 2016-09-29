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
import java.util.ArrayList;
import java.util.List;

import fr.landel.utils.commons.StringUtils;
import fr.landel.utils.commons.exception.IllegalOperationException;
import fr.landel.utils.model.AbstractEntity;

/**
 * A HQL query builder
 *
 * @since Aug 15, 2016
 * @author Gilles
 * 
 * @param <E>
 *            The entity
 * @param <K>
 *            The primary key type
 */
public class QueryBuilder<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends AbstractBuilder<E, K> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 3752851217819190672L;

    private static final int OPERATION = 1;

    private int step = 0;

    // select, update, insert, delete
    private AbstractBuilder<?, ?> operation;
    // from, join
    private FromBuilder<?, ?> from;
    // where
    private ClauseBuilder<?, ?> clause;
    // group by, having, order by, limit, offset
    private EndBuilder<?, ?> end;

    /**
     * Constructor
     *
     * @param entityClass
     *            the class of the entity
     * @param alias
     *            the entity alias
     */
    public QueryBuilder(final Class<E> entityClass, final CharSequence alias) {
        super(entityClass, alias);
    }

    /**
     * Constructor
     *
     * @param entityClass
     *            the class of the entity
     */
    public QueryBuilder(final Class<E> entityClass) {
        this(entityClass, null);
    }

    public SelectBuilder<E, K> select() {
        return this.select(null, null, null);
    }

    public SelectBuilder<E, K> select(final CharSequence selection) {
        return this.select(selection, null, null);
    }

    public SelectBuilder<E, K> select(final QueryDTO queryDTO) {
        return this.select(null, queryDTO, null);
    }

    public SelectBuilder<E, K> select(final QueryBuilder<?, ?> subQuery) {
        return this.select(null, null, subQuery);
    }

    private SelectBuilder<E, K> select(final CharSequence selection, final QueryDTO queryDTO, final QueryBuilder<?, ?> subQuery) {
        if (this.step == 0) {
            step |= OPERATION;
            final SelectBuilder<E, K> select;
            if (selection != null) {
                select = new SelectBuilder<>(this, this.getEntityClass(), this.getAlias(), selection);
            } else if (queryDTO != null) {
                select = new SelectBuilder<>(this, this.getEntityClass(), this.getAlias(), queryDTO);
            } else if (subQuery != null) {
                select = new SelectBuilder<>(this, this.getEntityClass(), this.getAlias(), subQuery);
            } else {
                select = new SelectBuilder<>(this, this.getEntityClass(), this.getAlias());
            }
            this.operation = select;
            return select;
        }
        throw new IllegalOperationException("Another operation has already been declared");
    }

    public InsertBuilder<E, K> insert() {
        if (this.step == 0) {
            step |= OPERATION;
            final InsertBuilder<E, K> insert = new InsertBuilder<>(this, this.getEntityClass(), this.getAlias());
            this.operation = insert;
            return insert;
        }
        throw new IllegalOperationException("Another operation has already been declared");
    }

    public UpdateBuilder<E, K> update() {
        if (this.step == 0) {
            step |= OPERATION;
            final UpdateBuilder<E, K> update = new UpdateBuilder<>(this, this.getEntityClass(), this.getAlias());
            this.operation = update;
            return update;
        }
        throw new IllegalOperationException("Another operation has already been declared");
    }

    public DeleteBuilder<E, K> delete() {
        if (this.step == 0) {
            step |= OPERATION;
            final DeleteBuilder<E, K> delete = new DeleteBuilder<>(this, this.getEntityClass(), this.getAlias());
            this.operation = delete;
            return delete;
        }
        throw new IllegalOperationException("Another operation has already been declared");
    }

    /**
     * @return the from
     */
    protected FromBuilder<?, ?> getFrom() {
        return this.from;
    }

    /**
     * @param from
     *            the from to set
     */
    protected void setFrom(FromBuilder<?, ?> from) {
        this.from = from;
    }

    /**
     * @return the clause
     */
    protected ClauseBuilder<?, ?> getClause() {
        return this.clause;
    }

    /**
     * @param clause
     *            the clause to set
     */
    protected void setClause(final ClauseBuilder<?, ?> clause) {
        this.clause = clause;
    }

    /**
     * @return the end
     */
    protected EndBuilder<?, ?> getEnd() {
        return this.end;
    }

    /**
     * @param end
     *            the end to set
     */
    protected void setEnd(EndBuilder<?, ?> end) {
        this.end = end;
    }

    @Override
    public String build() {
        final List<CharSequence> builder = new ArrayList<>();
        builder.add(this.operation.build());
        if (this.from != null) {
            builder.add(this.from.build());
        }
        if (this.clause != null) {
            builder.add(this.clause.build());
        }
        if (this.end != null) {
            builder.add(this.end.build());
        }
        return StringUtils.join(builder, SPACE);
    }
}