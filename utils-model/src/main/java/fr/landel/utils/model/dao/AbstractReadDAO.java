/*-
 * #%L
 * utils-model
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.model.dao;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import javax.persistence.PersistenceException;
import javax.persistence.TypedQuery;

import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Sort;

import fr.landel.utils.model.AbstractEntity;
import fr.landel.utils.model.exception.ModelException;
import fr.landel.utils.model.filter.FilterInfo;
import fr.landel.utils.model.filter.FilterPageable;
import fr.landel.utils.model.query.QueryBuilder1;
import fr.landel.utils.model.query.QueryOperator;
import fr.landel.utils.model.query.QueryOrder;

/**
 * Abstract read DAO.
 * 
 * @see <a href="http://martinfowler.com/bliki/CQRS.html">Command Query
 *      Responsibility Segregation</a>
 *
 * @since Jul 13, 2015
 * @author Erwan Ropartz
 * @author Gilles
 * @param <E>
 *            Class of the entity
 */
public abstract class AbstractReadDAO<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends AbstractDAO<E, K>
        implements ReadDAO<E, K> {

    /**
     * Name of entity identifier
     */
    public static final String ID_NAME = "id";

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -6101856339345930778L;

    private static final Pattern PATTERN_SELECTABLE = Pattern.compile("[\\w\\d\\.]+");

    /**
     * Constructor
     *
     * @param entityClass
     *            The class of the entity
     */
    public AbstractReadDAO(final Class<E> entityClass) {
        super(entityClass);
    }

    @Override
    public final boolean exists(final K id) {
        try {
            return this.find(id) != null;
        } catch (final ModelException e) {
            return false;
        }
    }

    @Override
    public List<E> find(final List<K> pks) throws ModelException {
        try {
            final String paramIDs = "PARAM_IDS";

            final QueryBuilder1<E, K> query = new QueryBuilder1<>(this.getEntityClass());

            query.selectEntity().from();

            query.where().in(query.getAlias() + DOT + ID_NAME, paramIDs);

            final TypedQuery<E> typedQuery = this.getQuery(query.toString(), false);

            typedQuery.setParameter(paramIDs, pks);

            return typedQuery.getResultList();
        } catch (IllegalStateException | PersistenceException | ModelException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "finding all entities"), e);
        }
    }

    @Override
    public List<E> findAll() throws ModelException {
        return this.findAll(false);
    }

    @Override
    public List<E> findAll(final boolean cacheable) throws ModelException {
        try {
            final QueryBuilder1<E, K> query = new QueryBuilder1<>(this.getEntityClass());

            query.selectEntity().from();

            final TypedQuery<E> typedQuery = this.getQuery(query.toString(), cacheable);

            return typedQuery.getResultList();
        } catch (IllegalStateException | PersistenceException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "finding all entities"), e);
        }
    }

    @Override
    public List<E> findAll(final Sort sort) throws ModelException {
        try {
            final QueryBuilder1<E, K> qb = new QueryBuilder1<>(this.getEntityClass());
            qb.from();

            // Manages orders
            qb.orderBy(this.buildOrders(sort));

            final TypedQuery<E> contentQuery = this.getEntityManager().createQuery(qb.toString(), this.getEntityClass());

            return contentQuery.getResultList();
        } catch (IllegalStateException | PersistenceException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "finding all sorted"), e);
        }
    }

    @Override
    public final List<E> findAll(final QueryBuilder1<E, K> qb, final FilterPageable<E, K> pageRequest) throws ModelException {
        try {
            final StringBuilder queryBuilder = new StringBuilder();

            if (!pageRequest.getJoins().isEmpty()) {
                for (final Entry<String, String> join : pageRequest.getJoins().entrySet()) {
                    qb.leftJoin(qb.getAlias() + DOT + join.getKey(), join.getValue());
                }
            }

            queryBuilder.append(qb.toString());

            // Manages filters
            this.manageFiltersFindAll(pageRequest, queryBuilder);

            // Manages orders
            this.manageOrdersFindAll(pageRequest, queryBuilder);

            final TypedQuery<E> contentQuery = this.getQuery(queryBuilder);

            // Manages filters
            if (!pageRequest.getFilters().isEmpty()) {
                this.filterHelper.buildFilterParameters(contentQuery, pageRequest.getFilters());
            }

            if (pageRequest.getOffset() >= 0) {
                contentQuery.setFirstResult(pageRequest.getOffset());
            }
            if (pageRequest.getPageSize() > 0) {
                contentQuery.setMaxResults(pageRequest.getPageSize());
            }

            return contentQuery.getResultList();
        } catch (IllegalStateException | PersistenceException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "finding all sorted"), e);
        }
    }

    @Override
    public final List<E> findAll(final String hqlSelection, final FilterPageable<E, K> pageRequest) throws ModelException {
        return this.findAll(this.getQueryBuilder().append(hqlSelection), pageRequest);
    }

    @Override
    public final List<E> findAll(final FilterPageable<E, K> pageRequest) throws ModelException {
        final QueryBuilder1<E, K> queryBuilder = new QueryBuilder1<>(this.getEntityClass(), "entity");
        queryBuilder.selectEntity().from();
        return this.findAll(queryBuilder, pageRequest);
    }

    private void manageOrdersFindAll(final FilterPageable<E, K> pageRequest, final StringBuilder queryBuilder) {
        final QueryOrder[] qos = this.buildOrders(pageRequest.getSort());
        if (qos != null && qos.length > 0) {
            queryBuilder.append(" ORDER BY ");
            for (final QueryOrder qo : qos) {
                queryBuilder.append(qo).append(" ");
            }
        }
    }

    private void manageFiltersFindAll(final FilterPageable<E, K> pageRequest, final StringBuilder queryBuilder) {
        if (!pageRequest.getFilters().isEmpty()) {
            if (!queryBuilder.toString().contains("WHERE")) {
                queryBuilder.append(" WHERE ");
            } else {
                queryBuilder.append(" AND ");
            }
            queryBuilder.append(this.filterHelper.buildFilters(pageRequest.getFilters()));
        }

        if (!pageRequest.getGroupBy().isEmpty()) {
            queryBuilder.append(" GROUP BY ");
            for (Integer i = 0; i < pageRequest.getGroupBy().size(); i++) {
                final String currenGroup = pageRequest.getGroupBy().get(i);
                queryBuilder.append(currenGroup);
                if (i < pageRequest.getGroupBy().size() - 1) {
                    queryBuilder.append(",");
                }

            }
        }
    }

    @Override
    public <X> List<X> findByColumn(final Class<X> clazz, final List<String> selectables, final String columnName, final Object value)
            throws ModelException {
        try {
            this.minSecurityChecker(selectables);

            final QueryBuilder1<E, K> query = new QueryBuilder1<>(this.getEntityClass(), TABLE_ALIAS);

            query.select(StringUtils.join(selectables, ", ")).from();

            query.where().isEqual(TABLE_ALIAS + DOT + columnName, PARAM_COLUMN);

            final TypedQuery<X> typedQuery = this.getQuery(clazz, query);

            typedQuery.setParameter(PARAM_COLUMN, value);

            return typedQuery.getResultList();
        } catch (IllegalStateException | PersistenceException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "finding by column"), e);
        }
    }

    @Override
    public <X> List<X> findByColumns(final Class<X> clazz, final List<String> selectables, final Map<String, Object> columns)
            throws ModelException {
        try {
            this.minSecurityChecker(selectables);

            final QueryBuilder1<E, K> query = new QueryBuilder1<>(this.getEntityClass(), TABLE_ALIAS);

            query.select(StringUtils.join(selectables, ", ")).from();

            int i = 0;
            for (final Entry<String, Object> entry : columns.entrySet()) {
                final QueryOperator operator;
                if (entry.getValue() != null && Iterable.class.isAssignableFrom(entry.getValue().getClass())) {
                    operator = QueryOperator.IN;
                } else {
                    operator = QueryOperator.EQUALS;
                }

                if (i == 0) {
                    query.where().of(TABLE_ALIAS + DOT + entry.getKey(), operator, PARAM_COLUMN + i);
                } else {
                    query.where().and().of(TABLE_ALIAS + DOT + entry.getKey(), operator, PARAM_COLUMN + i);
                }
                i++;
            }

            final TypedQuery<X> typedQuery = this.getQuery(clazz, query);

            i = 0;
            for (final Object value : columns.values()) {
                typedQuery.setParameter(PARAM_COLUMN + i, value);
                i++;
            }

            return typedQuery.getResultList();
        } catch (IllegalStateException | PersistenceException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "finding by column"), e);
        }
    }

    @Override
    public List<E> findByColumn(final String columnName, final Object value) throws ModelException {
        try {
            final QueryBuilder1<E, K> query = new QueryBuilder1<>(this.getEntityClass(), TABLE_ALIAS);

            query.selectEntity().from().where().isEqual(TABLE_ALIAS + DOT + columnName, PARAM_COLUMN);

            final TypedQuery<E> typedQuery = this.getQuery(query);

            typedQuery.setParameter(PARAM_COLUMN, value);

            return typedQuery.getResultList();
        } catch (IllegalStateException | PersistenceException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "finding by column"), e);
        }
    }

    @Override
    public List<E> findByColumns(final Map<String, Object> columns) throws ModelException {
        try {
            final QueryBuilder1<E, K> query = new QueryBuilder1<>(this.getEntityClass(), TABLE_ALIAS);

            query.selectEntity().from();

            int i = 0;
            for (final Entry<String, Object> entry : columns.entrySet()) {
                final QueryOperator operator;
                if (entry.getValue() != null && Iterable.class.isAssignableFrom(entry.getValue().getClass())) {
                    operator = QueryOperator.IN;
                } else {
                    operator = QueryOperator.EQUALS;
                }

                if (i == 0) {
                    query.where().of(TABLE_ALIAS + DOT + entry.getKey(), operator, PARAM_COLUMN + i);
                } else {
                    query.where().and().of(TABLE_ALIAS + DOT + entry.getKey(), operator, PARAM_COLUMN + i);
                }
                i++;
            }

            final TypedQuery<E> typedQuery = this.getQuery(query);

            i = 0;
            for (final Object value : columns.values()) {
                typedQuery.setParameter(PARAM_COLUMN + i, value);
                i++;
            }

            return typedQuery.getResultList();
        } catch (IllegalStateException | PersistenceException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "finding by column"), e);
        }
    }

    @Override
    public Long count(final String column) throws ModelException {
        try {
            final QueryBuilder1<E, K> query = new QueryBuilder1<>(this.getEntityClass());

            String col = column;
            if (column == null) {
                col = QueryBuilder1.ALL;
            }

            query.selectCount(col).from();

            final TypedQuery<Long> typedQuery = this.getQuery(Long.class, query);

            return typedQuery.getSingleResult();
        } catch (IllegalStateException | PersistenceException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "counting"), e);
        }
    }

    @Override
    public final long countAll(final FilterPageable<E, K> pageable) throws ModelException {
        try {
            final QueryBuilder1<E, K> qb = new QueryBuilder1<>(this.getEntityClass(), "entity");

            qb.selectCount(qb.getAlias()).from();

            if (!pageable.getJoins().isEmpty()) {
                for (final Entry<String, String> join : pageable.getJoins().entrySet()) {
                    qb.leftJoin(qb.getAlias() + DOT + join.getKey(), join.getValue());
                }
            }

            final List<FilterInfo<E, K>> filters = pageable.getFilters();

            // Manages orders
            if (filters != null && !filters.isEmpty()) {
                qb.where(this.filterHelper.buildFilters(filters));
            }

            final TypedQuery<Long> countQuery = this.getEntityManager().createQuery(qb.toString(), Long.class);

            if (filters != null) {
                this.filterHelper.buildFilterParameters(countQuery, filters);
            }

            return countQuery.getSingleResult();
        } catch (IllegalStateException | PersistenceException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "counting"), e);
        }
    }

    /**
     * @param query
     *            The typed query to execute
     * @return The single result or null (if none or multiple)
     * @throws ModelException
     *             The persistence exception
     */
    @Override
    protected E getSingleResult(final TypedQuery<E> query) throws ModelException {
        try {
            final List<E> results = query.getResultList();

            if (results != null && results.size() == 1) {
                return results.get(0);
            }

            return null;
        } catch (IllegalStateException | PersistenceException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "getting single result"), e);
        }
    }

    private void minSecurityChecker(final List<String> selectables) throws ModelException {
        for (String selectable : selectables) {
            if (!PATTERN_SELECTABLE.matcher(selectable).matches()) {
                throw new ModelException("Unexpected content for selectable, can only contains characters, digits and dots");
            }
        }
    }
}
