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
import java.util.Calendar;

import fr.landel.utils.commons.StringUtils;

import fr.landel.utils.model.AbstractEntity;

/**
 * A simple query builder based on a list.
 * 
 * todo: hibernate 3 methods: size, elements, indices, minindex, maxindex,
 * minelement, maxelement(Description)
 *
 * @since 30 nov. 2015
 * @author Gilles
 *
 * @param <E>
 *            The entity
 * @param <K>
 *            The primary key type
 */
public class QueryBuilder<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends ArrayList<String> {

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
    private static final long serialVersionUID = 629818342427662943L;

    private static final String COMMA = ",";

    private static final String SELECT = "SELECT";
    private static final String UPDATE = "UPDATE";
    private static final String INSERT = "INSERT INTO";
    private static final String DELETE = "DELETE FROM";

    private static final String FROM = "FROM";
    private static final String SET = "SET";
    private static final String AS = "AS";

    private static final String WHERE = "WHERE";
    private static final String OR = "OR";
    private static final String AND = "AND";

    private static final String LEFT = "LEFT";
    private static final String RIGHT = "RIGHT";
    private static final String OUTER = "OUTER";
    private static final String INNER = "INNER";
    private static final String JOIN = "JOIN";

    private static final String GROUP_BY = "GROUP BY";
    private static final String ORDER_BY = "ORDER BY";

    private static final String LIMIT = "LIMIT";
    private static final String OFFSET = "OFFSET";

    private static final String COUNT = "COUNT";

    private Class<E> entityClass;
    private boolean set;
    private String alias;

    /**
     * Constructor.
     * 
     * @param entityClass
     *            The class of the entity
     */
    public QueryBuilder(final Class<E> entityClass) {
        this(entityClass, null);
    }

    /**
     * Constructor.
     * 
     * @param entityClass
     *            The class of the entity
     * @param alias
     *            The entity alias
     */
    public QueryBuilder(final Class<E> entityClass, final String alias) {
        this.entityClass = entityClass;

        if (StringUtils.isNotEmpty(alias)) {
            this.alias = alias;
        } else {
            this.alias = entityClass.getSimpleName().toLowerCase() + Calendar.getInstance().getTimeInMillis();
        }
    }

    @Override
    public String toString() {
        return StringUtils.join(this, SPACE);
    }

    /**
     * Add an entity entity.
     * 
     * @param entityClassSub
     *            entity class
     * @param alias
     *            alias
     */
    private void addEntity(final Class<? extends AbstractEntity<?, ?>> entityClassSub, final String alias) {
        this.add(entityClassSub.getCanonicalName());
        this.add(AS);
        if (StringUtils.isNotEmpty(alias)) {
            this.add(alias);
        } else {
            this.add(this.getAlias());
        }
    }

    /**
     * Add an entity.
     * 
     * @param alias
     *            alias
     */
    private void addEntity(final String alias) {
        this.addEntity(this.entityClass, alias);
    }

    /**
     * Get the current alias.
     * 
     * @return the current alias
     */
    public String getAlias() {
        return this.alias;
    }

    /**
     * Select builder.
     * 
     * @param selection
     *            The selection (columns list...)
     * @return the current query builder
     */
    public QueryBuilder<E, K> select(final String selection) {
        this.add(SELECT);
        if (StringUtils.isNotEmpty(selection)) {
            this.add(selection);
        }

        return this;
    }

    /**
     * Select entity builder.
     * 
     * @param query
     *            The sub-query
     * @param <T>
     *            the entity class of the sub-query
     * @param <Y>
     *            the primary key type
     * @return the current query builder
     */
    public <T extends AbstractEntity<T, Y>, Y extends Serializable & Comparable<Y>> QueryBuilder<E, K> select(final QueryBuilder<T, Y> query) {
        this.select("");
        return this.append(query);
    }

    /**
     * Select entity builder.
     * 
     * @return the current query builder
     */
    public QueryBuilder<E, K> selectEntity() {
        return this.selectEntity(null);
    }

    /**
     * Select entity builder.
     * 
     * @param alias
     *            alias of the entity
     * @return the current query builder
     */
    public QueryBuilder<E, K> selectEntity(final String alias) {
        if (StringUtils.isNotEmpty(alias)) {
            this.select(alias);
        } else {
            this.select(this.getAlias());
        }

        return this;
    }

    /**
     * From builder.
     * 
     * @return the current query builder
     */
    public QueryBuilder<E, K> from() {
        return this.from(null);
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
    public QueryBuilder<E, K> from(final Class<? extends AbstractEntity<?, ?>> entityClassLinked, final String alias) {
        String localAlias = alias;
        this.add(FROM);
        if (StringUtils.isEmpty(localAlias)) {
            localAlias = entityClassLinked.getSimpleName().toLowerCase() + Calendar.getInstance().getTimeInMillis();
        }
        this.addEntity(entityClassLinked, localAlias);

        return this;
    }

    /**
     * From builder.
     * 
     * @param alias
     *            The alias of the entity
     * @return the current query builder
     */
    public QueryBuilder<E, K> from(final String alias) {
        if (!this.contains(SELECT)) {
            this.selectEntity(alias);
        }
        this.add(FROM);
        this.addEntity(alias);

        return this;
    }

    /**
     * Where builder.
     * 
     * @return the current query builder
     */
    private void where() {
        this.add(WHERE);
    }

    /**
     * Where builder.
     * 
     * @param condition
     *            The condition
     * @return the current query builder
     */
    public QueryBuilder<E, K> where(final QueryCondition condition) {
        this.where();
        condition(condition);

        return this;
    }

    /**
     * Where builder.
     * 
     * @param query
     *            The sub-query
     * @param <T>
     *            the entity class of the sub-query
     * @param <Y>
     *            the primary key type
     * @return the current query builder
     */
    public <T extends AbstractEntity<T, Y>, Y extends Serializable & Comparable<Y>> QueryBuilder<E, K> where(final QueryBuilder<T, Y> query) {
        this.where();
        return this.append(query);
    }

    /**
     * Where is null builder.
     * 
     * @param column
     *            The column to check
     * @return the current query builder
     */
    public QueryBuilder<E, K> whereIsNull(final String column) {
        this.where(new QueryCondition(column, QueryCondition.IS, QueryCondition.NULL));

        return this;
    }

    /**
     * Where is not null builder.
     * 
     * @param column
     *            The column to check
     * @return the current query builder
     */
    public QueryBuilder<E, K> whereIsNotNull(final String column) {
        this.where(new QueryCondition(column, QueryCondition.IS_NOT, QueryCondition.NULL));

        return this;
    }

    /**
     * Add a condition.
     * 
     * @param condition
     *            condition to add
     */
    private void condition(final QueryCondition condition) {
        if (condition != null && StringUtils.isNotEmpty(condition.toString())) {
            this.add(PARENTHESIS_OPEN);
            addAll(condition);
            this.add(PARENTHESIS_CLOSE);
        }
    }

    /**
     * Add builder.
     * 
     * @param condition
     *            The condition
     * @return the current query builder
     */
    public QueryBuilder<E, K> add(final QueryCondition condition) {
        if (condition != null && StringUtils.isNotEmpty(condition.toString())) {
            condition(condition);
        }

        return this;
    }

    /**
     * Or builder.
     * 
     * @param condition
     *            The condition
     * @return the current query builder
     */
    public QueryBuilder<E, K> or(final QueryCondition condition) {
        this.add(OR);
        condition(condition);

        return this;
    }

    /**
     * Or builder.
     * 
     * @param query
     *            The sub-query
     * @param <T>
     *            the entity class of the sub-query
     * @param <Y>
     *            the primary key type
     * @return the current query builder
     */
    public <T extends AbstractEntity<T, Y>, Y extends Serializable & Comparable<Y>> QueryBuilder<E, K> or(final QueryBuilder<T, Y> query) {
        this.or((QueryCondition) null);
        return this.append(query);
    }

    /**
     * Or is null builder.
     * 
     * @param column
     *            The column to check
     * @return the current query builder
     */
    public QueryBuilder<E, K> orIsNull(final String column) {
        this.or(new QueryCondition(column, QueryCondition.IS, QueryCondition.NULL));

        return this;
    }

    /**
     * Or is not null builder.
     * 
     * @param column
     *            The column to check
     * @return the current query builder
     */
    public QueryBuilder<E, K> orIsNotNull(final String column) {
        this.or(new QueryCondition(column, QueryCondition.IS_NOT, QueryCondition.NULL));

        return this;
    }

    /**
     * And builder.
     * 
     * @param condition
     *            The condition
     * @return the current query builder
     */
    public QueryBuilder<E, K> and(final QueryCondition condition) {
        this.add(AND);
        condition(condition);

        return this;
    }

    /**
     * And builder.
     * 
     * @param query
     *            The sub-query
     * @param <T>
     *            the entity class of the sub-query
     * @param <Y>
     *            the primary key type
     * @return the current query builder
     */
    public <T extends AbstractEntity<T, Y>, Y extends Serializable & Comparable<Y>> QueryBuilder<E, K> and(final QueryBuilder<T, Y> query) {
        this.and((QueryCondition) null);
        return this.append(query);
    }

    /**
     * And is null builder.
     * 
     * @param column
     *            The column to check
     * @return the current query builder
     */
    public QueryBuilder<E, K> andIsNull(final String column) {
        this.and(new QueryCondition(column, QueryCondition.IS, QueryCondition.NULL));

        return this;
    }

    /**
     * And is not null builder.
     * 
     * @param column
     *            The column to check
     * @return the current query builder
     */
    public QueryBuilder<E, K> andIsNotNull(final String column) {
        this.and(new QueryCondition(column, QueryCondition.IS_NOT, QueryCondition.NULL));

        return this;
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
    public QueryBuilder<E, K> join(final String joinObject, final String joinAlias) {
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
    public QueryBuilder<E, K> leftJoin(final String joinObject, final String joinAlias) {
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
    public QueryBuilder<E, K> innerJoin(final String joinObject, final String joinAlias) {
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
    public QueryBuilder<E, K> rightJoin(final String joinObject, final String joinAlias) {
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
    public QueryBuilder<E, K> leftOuterJoin(final String joinObject, final String joinAlias) {
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
    public QueryBuilder<E, K> rightOuterJoin(final String joinObject, final String joinAlias) {
        this.add(RIGHT);
        this.add(OUTER);
        return this.join(joinObject, joinAlias);
    }

    /**
     * Update builder.
     * 
     * @return the current query builder
     */
    public QueryBuilder<E, K> update() {
        return this.update(null);
    }

    /**
     * Update builder.
     * 
     * @param alias
     *            The alias of the entity to update
     * @return the current query builder
     */
    public QueryBuilder<E, K> update(final String alias) {
        this.add(UPDATE);
        this.addEntity(alias);

        return this;
    }

    /**
     * Set builder.
     * 
     * (adds automatically colon before parameter name if not found)
     * 
     * @param column
     *            The column name
     * @param parameter
     *            The name of value parameter
     * @return the current query builder
     */
    public QueryBuilder<E, K> set(final String column, final String parameter) {
        if (StringUtils.isNotEmpty(column) && StringUtils.isNotEmpty(parameter)) {
            if (this.set) {
                this.add(COMMA);
            } else {
                this.add(SET);
            }
            this.add(column);
            this.add(EQUAL);
            if (parameter.startsWith(COLON)) {
                this.add(parameter);
            } else {
                this.add(COLON + parameter);
            }

            this.set = true;
        }
        return this;
    }

    /**
     * Insert builder.
     * 
     * @return the current query builder
     */
    public QueryBuilder<E, K> insert() {
        return this.insert(null);
    }

    /**
     * Insert builder.
     * 
     * @param alias
     *            The alias of the entity to insert
     * @return the current query builder
     */
    public QueryBuilder<E, K> insert(final String alias) {
        this.add(INSERT);
        this.addEntity(alias);

        return this;
    }

    /**
     * Insert columns builder.
     * 
     * @param columns
     *            The columns of the entity to insert
     * @return the current query builder
     */
    public QueryBuilder<E, K> insertColumn(final String... columns) {
        if (columns != null && columns.length > 0) {
            this.add(PARENTHESIS_OPEN);
            for (int i = 0; i < columns.length; i++) {
                this.add(columns[i]);
                if (i < columns.length - 1) {
                    this.add(COMMA);
                }
            }
            this.add(PARENTHESIS_CLOSE);
        }

        return this;
    }

    /**
     * Delete builder.
     * 
     * @return the current query builder
     */
    public QueryBuilder<E, K> delete() {
        return this.delete(null);
    }

    /**
     * Delete builder.
     * 
     * @param alias
     *            The alias of the entity to delete
     * @return the current query builder
     */
    public QueryBuilder<E, K> delete(final String alias) {
        this.add(DELETE);
        this.addEntity(alias);

        return this;
    }

    /**
     * Group by builder.
     * 
     * @return the current query builder
     */
    public QueryBuilder<E, K> groupBy() {
        return this.groupBy(this.alias);
    }

    /**
     * Group by builder.
     * 
     * @param alias
     *            The alias of the entity
     * @return the current query builder
     */
    public QueryBuilder<E, K> groupBy(final String alias) {
        this.add(GROUP_BY);
        this.add(alias);

        return this;
    }

    /**
     * Order by builder.
     * 
     * @param queryOrder
     *            The order queries
     * @return the current query builder
     */
    public QueryBuilder<E, K> orderBy(final QueryOrder... queryOrder) {
        this.add(ORDER_BY);
        this.add(StringUtils.join(queryOrder, COMMA + " "));

        return this;
    }

    /**
     * Limit builder.
     * 
     * @param limit
     *            The limit (limit has to be upper than 0)
     * @return the current query builder
     */
    public QueryBuilder<E, K> limit(final Integer limit) {
        if (limit != null && limit > 0) {
            this.add(LIMIT);
            this.add(String.valueOf(limit));
        }

        return this;
    }

    /**
     * Offet builder.
     * 
     * @param offset
     *            The offset (offset has to be upper than 0)
     * @return the current query builder
     */
    public QueryBuilder<E, K> offset(final Integer offset) {
        if (offset != null && offset > 0) {
            this.add(OFFSET);
            this.add(String.valueOf(offset));
        }

        return this;
    }

    /**
     * Count builder.
     * 
     * @param column
     *            The column to count
     * @return the current query builder
     */
    public QueryBuilder<E, K> selectCount(final String column) {
        this.select(COUNT);
        this.add(PARENTHESIS_OPEN);
        if (StringUtils.isNotEmpty(column)) {
            this.add(column);
        } else {
            this.add(ALL);
        }
        this.add(PARENTHESIS_CLOSE);

        return this;
    }

    /**
     * Append builder.
     * 
     * @param text
     *            The text to append
     * @return the current query builder
     */
    public QueryBuilder<E, K> append(final String text) {
        if (StringUtils.isNotEmpty(text)) {
            this.add(text);
        }

        return this;
    }

    /**
     * Append builder.
     * 
     * @param query
     *            The sub-query
     * @param <T>
     *            the entity class of the sub-query
     * @param <Y>
     *            the primary key type
     * @return the current query builder
     */
    public <T extends AbstractEntity<T, Y>, Y extends Serializable & Comparable<Y>> QueryBuilder<E, K> append(final QueryBuilder<T, Y> query) {
        if (query != null) {
            this.add(PARENTHESIS_OPEN);
            addAll(query);
            this.add(PARENTHESIS_CLOSE);
        }

        return this;
    }
}
