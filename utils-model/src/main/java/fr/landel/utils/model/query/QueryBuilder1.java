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
import java.util.UUID;

import fr.landel.utils.commons.EnumChar;
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
public class QueryBuilder1<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends AbstractQueryBuilder1 {

    /**
     * OR operator
     */
    public static final String OR = "OR";

    /**
     * AND operator
     */
    public static final String AND = "AND";

    /**
     * Where clause
     */
    public static final String WHERE = "WHERE";

    private static final String COMMA = EnumChar.COMMA.getUnicode();

    private static final String SELECT = "SELECT";
    private static final String UPDATE = "UPDATE";
    private static final String INSERT = "INSERT INTO";
    private static final String DELETE = "DELETE FROM";

    private static final String FROM = "FROM";
    private static final String SET = "SET";
    private static final String AS = "AS";

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

    /**
     * Serial
     */
    private static final long serialVersionUID = 629818342427662943L;

    private final Class<E> entityClass;
    private boolean set;
    private String alias;
    private QueryCondition1<E, K> queryCondition;

    /**
     * Constructor.
     * 
     * @param entityClass
     *            The class of the entity
     */
    public QueryBuilder1(final Class<E> entityClass) {
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
    public QueryBuilder1(final Class<E> entityClass, final String alias) {
        this.entityClass = entityClass;

        if (StringUtils.isNotEmpty(alias)) {
            this.alias = alias;
        } else {
            this.alias = generateAlias(this.entityClass);
        }
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
     * @param queryDTO
     *            The DTO query
     * @return the current query builder
     */
    public QueryBuilder1<E, K> select(final QueryDTO queryDTO) {
        this.add(SELECT);
        if (queryDTO != null) {
            this.add(queryDTO.toString());
        }

        return this;
    }

    /**
     * Select builder.
     * 
     * @param selection
     *            The selection (columns list...)
     * @return the current query builder
     */
    public QueryBuilder1<E, K> select(final String selection) {
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
    public <T extends AbstractEntity<T, Y>, Y extends Serializable & Comparable<Y>> QueryBuilder1<E, K> select(
            final QueryBuilder1<T, Y> query) {
        this.select("");
        return this.append(query);
    }

    /**
     * Select entity builder.
     * 
     * @return the current query builder
     */
    public QueryBuilder1<E, K> selectEntity() {
        return this.selectEntity(null);
    }

    /**
     * Select entity builder.
     * 
     * @param alias
     *            alias of the entity
     * @return the current query builder
     */
    public QueryBuilder1<E, K> selectEntity(final String alias) {
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
    public QueryBuilder1<E, K> from() {
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
    public QueryBuilder1<E, K> from(final Class<? extends AbstractEntity<?, ?>> entityClassLinked, final String alias) {
        String localAlias = alias;
        this.add(FROM);
        if (StringUtils.isEmpty(localAlias)) {
            localAlias = generateAlias(entityClassLinked);
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
    public QueryBuilder1<E, K> from(final String alias) {
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
     * @return the query condition builder
     */
    public QueryCondition1<E, K> where() {
        if (this.queryCondition == null) {
            this.add(WHERE);

            this.queryCondition = new QueryCondition1<>(this);
        }

        return this.queryCondition;
    }

    /**
     * Where builder.
     * 
     * @param condition
     *            The condition
     * @return the current query builder
     */
    public QueryBuilder1<E, K> where(final QueryCondition1<E, K> condition) {
        this.add(WHERE);
        condition(condition);

        return this;
    }

    /**
     * Where builder. (avoid this no check)
     * 
     * @param condition
     *            The condition
     * @return the current query builder
     */
    public QueryBuilder1<E, K> where(final CharSequence condition) {
        this.add(WHERE);
        this.add(condition);

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
    public <T extends AbstractEntity<T, Y>, Y extends Serializable & Comparable<Y>> QueryBuilder1<E, K> where(
            final QueryBuilder1<T, Y> query) {
        this.add(WHERE);
        return this.append(query);
    }

    /**
     * Where is null builder.
     * 
     * @param column
     *            The column to check
     * @return the current query builder
     */
    public QueryBuilder1<E, K> whereIsNull(final String column) {
        this.where().isNull(column);

        return this;
    }

    /**
     * Where is not null builder.
     * 
     * @param column
     *            The column to check
     * @return the current query builder
     */
    public QueryBuilder1<E, K> whereIsNotNull(final String column) {
        this.where().isNotNull(column);

        return this;
    }

    /**
     * Add a condition.
     * 
     * @param condition
     *            condition to add
     */
    private void condition(final QueryCondition1<E, K> condition) {
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
    public QueryBuilder1<E, K> add(final QueryCondition1<E, K> condition) {
        if (condition != null && StringUtils.isNotEmpty(condition.toString())) {
            condition(condition);
        }

        return this;
    }

    /**
     * Or builder.
     * 
     * @return the current query builder
     */
    public QueryBuilder1<E, K> or() {
        this.where().or();

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
    public <T extends AbstractEntity<T, Y>, Y extends Serializable & Comparable<Y>> QueryBuilder1<E, K> or(final QueryBuilder1<T, Y> query) {
        this.or();
        return this.append(query);
    }

    /**
     * Or is null builder.
     * 
     * @param column
     *            The column to check
     * @return the current query builder
     */
    public QueryBuilder1<E, K> orIsNull(final String column) {
        this.where().or().isNull(column);

        return this;
    }

    /**
     * Or is not null builder.
     * 
     * @param column
     *            The column to check
     * @return the current query builder
     */
    public QueryBuilder1<E, K> orIsNotNull(final String column) {
        this.where().or().isNotNull(column);

        return this;
    }

    /**
     * And builder.
     * 
     * @return the current query builder
     */
    public QueryBuilder1<E, K> and() {
        this.where().and();

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
    public <T extends AbstractEntity<T, Y>, Y extends Serializable & Comparable<Y>> QueryBuilder1<E, K> and(final QueryBuilder1<T, Y> query) {
        this.and();
        return this.append(query);
    }

    /**
     * And is null builder.
     * 
     * @param column
     *            The column to check
     * @return the current query builder
     */
    public QueryBuilder1<E, K> andIsNull(final String column) {
        this.where().and().isNull(column);

        return this;
    }

    /**
     * And is not null builder.
     * 
     * @param column
     *            The column to check
     * @return the current query builder
     */
    public QueryBuilder1<E, K> andIsNotNull(final String column) {
        this.where().and().isNotNull(column);

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
    public QueryBuilder1<E, K> join(final String joinObject, final String joinAlias) {
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
    public QueryBuilder1<E, K> leftJoin(final String joinObject, final String joinAlias) {
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
    public QueryBuilder1<E, K> innerJoin(final String joinObject, final String joinAlias) {
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
    public QueryBuilder1<E, K> rightJoin(final String joinObject, final String joinAlias) {
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
    public QueryBuilder1<E, K> leftOuterJoin(final String joinObject, final String joinAlias) {
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
    public QueryBuilder1<E, K> rightOuterJoin(final String joinObject, final String joinAlias) {
        this.add(RIGHT);
        this.add(OUTER);
        return this.join(joinObject, joinAlias);
    }

    /**
     * Update builder.
     * 
     * @return the current query builder
     */
    public QueryBuilder1<E, K> update() {
        return this.update(null);
    }

    /**
     * Update builder.
     * 
     * @param alias
     *            The alias of the entity to update
     * @return the current query builder
     */
    public QueryBuilder1<E, K> update(final String alias) {
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
    public QueryBuilder1<E, K> set(final String column, final String parameter) {
        if (StringUtils.isNotEmpty(column) && StringUtils.isNotEmpty(parameter)) {
            if (this.set) {
                this.add(COMMA);
            } else {
                this.add(SET);
            }
            this.add(column);
            this.add(QueryOperator.EQUALS.toString());
            if (parameter.charAt(0) == COLON) {
                this.add(parameter);
            } else {
                this.add(new StringBuilder().append(COLON).append(parameter));
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
    public QueryBuilder1<E, K> insert() {
        return this.insert(null);
    }

    /**
     * Insert builder.
     * 
     * @param alias
     *            The alias of the entity to insert
     * @return the current query builder
     */
    public QueryBuilder1<E, K> insert(final String alias) {
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
    public QueryBuilder1<E, K> insertColumn(final String... columns) {
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
    public QueryBuilder1<E, K> delete() {
        return this.delete(null);
    }

    /**
     * Delete builder.
     * 
     * @param alias
     *            The alias of the entity to delete
     * @return the current query builder
     */
    public QueryBuilder1<E, K> delete(final String alias) {
        this.add(DELETE);
        this.addEntity(alias);

        return this;
    }

    /**
     * Group by builder.
     * 
     * @return the current query builder
     */
    public QueryBuilder1<E, K> groupBy() {
        return this.groupBy(this.alias);
    }

    /**
     * Group by builder.
     * 
     * @param alias
     *            The alias of the entity
     * @return the current query builder
     */
    public QueryBuilder1<E, K> groupBy(final String alias) {
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
    public QueryBuilder1<E, K> orderBy(final QueryOrder... queryOrder) {
        this.add(ORDER_BY);
        this.add(StringUtils.join(queryOrder, StringUtils.JOIN_SEPARATOR));

        return this;
    }

    /**
     * Limit builder.
     * 
     * @param limit
     *            The limit (limit has to be upper than 0)
     * @return the current query builder
     */
    public QueryBuilder1<E, K> limit(final Integer limit) {
        if (limit != null && limit > 0) {
            this.add(LIMIT);
            this.add(String.valueOf(limit));
        }

        return this;
    }

    /**
     * Offset builder.
     * 
     * @param offset
     *            The offset (offset has to be upper than 0)
     * @return the current query builder
     */
    public QueryBuilder1<E, K> offset(final Integer offset) {
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
    public QueryBuilder1<E, K> selectCount(final String column) {
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
    public QueryBuilder1<E, K> append(final String text) {
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
    public <T extends AbstractEntity<T, Y>, Y extends Serializable & Comparable<Y>> QueryBuilder1<E, K> append(
            final QueryBuilder1<T, Y> query) {
        if (query != null) {
            this.add(PARENTHESIS_OPEN);
            addAll(query);
            this.add(PARENTHESIS_CLOSE);
        }

        return this;
    }

    private static final String generateAlias(final Class<?> clazz) {
        return new StringBuilder(clazz.getSimpleName().toLowerCase()).append(StringUtils.remove(UUID.randomUUID().toString(), '-'))
                .toString();
    }
}
