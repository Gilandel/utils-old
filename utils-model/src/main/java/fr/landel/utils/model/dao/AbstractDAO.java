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
package fr.landel.utils.model.dao;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.persistence.CacheRetrieveMode;
import javax.persistence.CacheStoreMode;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.PersistenceContextType;
import javax.persistence.PersistenceException;
import javax.persistence.Query;
import javax.persistence.TransactionRequiredException;
import javax.persistence.TypedQuery;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;

import fr.landel.utils.model.AbstractEntity;
import fr.landel.utils.model.exception.ModelException;
import fr.landel.utils.model.filter.FilterHelper;
import fr.landel.utils.model.query.QueryBuilder1;
import fr.landel.utils.model.query.QueryOrder;

/**
 * Abstract DAO.
 *
 * @since Jul 13, 2015
 * @author Erwan Ropartz
 * @author Gilles Landel
 * @param <E>
 *            Class of the entity
 * @param <K>
 *            Class of the primary key
 */
public abstract class AbstractDAO<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> implements DAO<E, K> {

    /**
     * The template for error message
     */
    protected static final String ERROR_MESSAGE = "Error occurred on %s.";

    /**
     * The dot symbol
     */
    protected static final String DOT = ".";
    /**
     * key to hibernate cache
     */
    protected static final String HIBERNATE_CACHEABLE = "org.hibernate.cacheable";

    /**
     * key to persistence cache retrieve mode
     */
    protected static final String PERSISTENCE_CACHE_RETRIEVE_MODE = "javax.persistence.cache.retrieveMode";

    /**
     * key to persistence cache store mode
     */
    protected static final String PERSISTENCE_CACHE_STORE_MODE = "javax.persistence.cache.storeMode";

    /**
     * table alias
     */
    protected static final String TABLE_ALIAS = "t";

    /**
     * primary key
     */
    protected static final String PRIMARY_KEY = "id";

    /**
     * param column
     */
    protected static final String PARAM_COLUMN = "paramColumn";

    /**
     * Helper to create filters
     */
    @Autowired
    protected FilterHelper filterHelper;

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 1204120287147150408L;

    /**
     * Entity manager for persistence.
     */
    @PersistenceContext(type = PersistenceContextType.TRANSACTION)
    private EntityManager entityManager;

    /**
     * The class of the entity
     */
    private Class<E> entityClass;

    /**
     * Default constructor.
     *
     * @param entityClass
     *            The entity class
     */
    public AbstractDAO(final Class<E> entityClass) {
        this.entityClass = entityClass;
    }

    /**
     * Get the entity manager.
     *
     * @return the entity manager
     */
    public final EntityManager getEntityManager() {
        return this.entityManager;
    }

    /**
     * @return return a new instance of query builder
     */
    protected QueryBuilder1<E, K> getQueryBuilder() {
        return new QueryBuilder1<E, K>(this.getEntityClass());
    }

    /**
     * @param alias
     *            The alias used to identify the entity
     * @return return a new instance of query builder
     */
    protected QueryBuilder1<E, K> getQueryBuilder(final String alias) {
        return new QueryBuilder1<E, K>(this.getEntityClass(), alias);
    }

    /**
     * Find an entity through its primary key
     * 
     * @param pk
     *            the primary to find
     * @return the entity
     * @throws ModelException
     *             on find error
     */
    @Override
    public E find(final K pk) throws ModelException {
        try {
            return this.getEntityManager().find(this.getEntityClass(), pk);
        } catch (final IllegalArgumentException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "finding the entity"), e);
        }
    }

    @Override
    public void flushAndClear() throws ModelException {
        try {
            this.getEntityManager().flush();
            this.getEntityManager().clear();
        } catch (final PersistenceException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "flushing and clearing the session"), e);
        }
    }

    @Override
    public void flush() throws ModelException {
        try {
            this.getEntityManager().flush();
        } catch (final PersistenceException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "flushing the entity"), e);
        }
    }

    @Override
    public void refresh(final E entity) throws ModelException {
        try {
            this.getEntityManager().refresh(entity);
        } catch (TransactionRequiredException | IllegalArgumentException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "refreshing the entity"), e);
        }
    }

    /**
     * Build HQL Order.
     *
     * @param sort
     *            List of sort
     * @return The HQL order
     */
    protected final QueryOrder[] buildOrders(final Sort sort) {
        final List<QueryOrder> orders = new ArrayList<>();

        if (sort != null) {
            Sort.Order order = null;
            for (final Iterator<Sort.Order> itOrder = sort.iterator(); itOrder.hasNext();) {
                order = itOrder.next();

                if (Direction.ASC.equals(order.getDirection())) {
                    orders.add(QueryOrder.asc(order.getProperty()));
                } else {
                    orders.add(QueryOrder.desc(order.getProperty()));
                }
            }
        }

        return orders.toArray(new QueryOrder[0]);
    }

    /**
     * Create an executable query (update or delete)
     * 
     * @param strQuery
     *            The HQL query
     * @return The generated typed query
     * @throws ModelException
     *             The persistence exception
     */
    private Query getExecutableQuery(final String strQuery) throws ModelException {
        try {
            return this.getEntityManager().createQuery(strQuery);
        } catch (final IllegalArgumentException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "creating the executable query"), e);
        }
    }

    /**
     * Create an executable query (update or delete)
     * 
     * @param query
     *            The HQL query
     * @return The generated typed query
     * @throws ModelException
     *             The persistence exception
     */
    protected Query getExecutableQuery(final StringBuilder query) throws ModelException {
        return this.getExecutableQuery(query.toString());
    }

    /**
     * Create an executable query (update or delete)
     * 
     * @param query
     *            The HQL query
     * @return The generated typed query
     * @throws ModelException
     *             The persistence exception
     */
    protected Query getExecutableQuery(final QueryBuilder1<E, K> query) throws ModelException {
        return this.getExecutableQuery(query.toString());
    }

    /**
     * Get persistence query
     * 
     * @param strQuery
     *            a Java Persistence query string
     * @param cacheable
     *            is it cacheable
     * @return the new query instance
     * @throws ModelException
     *             The core exception
     */
    protected TypedQuery<E> getQuery(final String strQuery, final boolean cacheable) throws ModelException {
        try {
            final TypedQuery<E> query = this.getEntityManager().createQuery(strQuery, this.getEntityClass());
            if (cacheable) {
                query.setHint(HIBERNATE_CACHEABLE, Boolean.TRUE);
                query.setHint(PERSISTENCE_CACHE_RETRIEVE_MODE, CacheRetrieveMode.USE);
                query.setHint(PERSISTENCE_CACHE_STORE_MODE, CacheStoreMode.USE);
            }
            return query;
        } catch (final IllegalArgumentException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "creating the query"), e);
        }
    }

    /**
     * Create a typed query
     * 
     * @param query
     *            The HQL query
     * @return The generated typed query
     * @throws ModelException
     *             The persistence exception
     */
    protected TypedQuery<E> getQuery(final StringBuilder query) throws ModelException {
        return this.getQuery(query.toString(), false);
    }

    /**
     * Create a typed query
     * 
     * @param query
     *            The HQL query
     * @return The generated typed query
     * @throws ModelException
     *             The persistence exception
     */
    protected TypedQuery<E> getQuery(final QueryBuilder1<E, K> query) throws ModelException {
        return this.getQuery(query.toString(), false);
    }

    private <T> TypedQuery<T> getQuery(final Class<T> queryReturnType, final String strQuery, final boolean cacheable)
            throws ModelException {
        try {
            final TypedQuery<T> query = this.getEntityManager().createQuery(strQuery, queryReturnType);
            if (cacheable) {
                query.setHint(HIBERNATE_CACHEABLE, Boolean.TRUE);
                query.setHint(PERSISTENCE_CACHE_RETRIEVE_MODE, CacheRetrieveMode.USE);
                query.setHint(PERSISTENCE_CACHE_STORE_MODE, CacheStoreMode.USE);
            }
            return query;
        } catch (final IllegalArgumentException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "creating the query"), e);
        }
    }

    /**
     * Create a typed query
     * 
     * @param queryReturnType
     *            The class of the return
     * @param query
     *            The HQL query
     * @param <T>
     *            The type of the return
     * @return The generated typed query
     * @throws ModelException
     *             The persistence exception
     */
    protected <T> TypedQuery<T> getQuery(final Class<T> queryReturnType, final StringBuilder query) throws ModelException {
        return this.getQuery(queryReturnType, query.toString(), false);
    }

    /**
     * Create a typed query
     * 
     * @param queryReturnType
     *            The class of the return
     * @param query
     *            The HQL query
     * @param <T>
     *            The type of the return
     * @return The generated typed query
     * @throws ModelException
     *             The persistence exception
     */
    protected <T> TypedQuery<T> getQuery(final Class<T> queryReturnType, final QueryBuilder1<E, K> query) throws ModelException {
        return this.getQuery(queryReturnType, query.toString(), false);
    }

    /**
     * Create a cacheable typed query
     * 
     * @param strQuery
     *            The HQL query
     * @return The generated typed query
     * @throws ModelException
     *             The persistence exception
     */
    protected TypedQuery<E> getCacheableQuery(final StringBuilder strQuery) throws ModelException {
        return this.getQuery(strQuery.toString(), true);
    }

    /**
     * Create a cacheable typed query
     * 
     * @param query
     *            The HQL query
     * @return The generated typed query
     * @throws ModelException
     *             The persistence exception
     */
    protected TypedQuery<E> getCacheableQuery(final QueryBuilder1<E, K> query) throws ModelException {
        return this.getQuery(query.toString(), true);
    }

    /**
     * Create a cacheable typed query
     * 
     * @param queryReturnType
     *            The class of the return
     * @param strQuery
     *            The HQL query
     * @param <T>
     *            The type of the return
     * @return The generated typed query
     * @throws ModelException
     *             The persistence exception
     */
    protected <T> TypedQuery<T> getCacheableQuery(final Class<T> queryReturnType, final StringBuilder strQuery) throws ModelException {
        return this.getQuery(queryReturnType, strQuery.toString(), true);
    }

    /**
     * Create a cacheable typed query
     * 
     * @param queryReturnType
     *            The class of the return
     * @param query
     *            The HQL query
     * @param <T>
     *            The type of the return
     * @return The generated typed query
     * @throws ModelException
     *             The persistence exception
     */
    protected <T> TypedQuery<T> getCacheableQuery(final Class<T> queryReturnType, final QueryBuilder1<E, K> query) throws ModelException {
        return this.getQuery(queryReturnType, query.toString(), true);
    }

    /**
     * @param query
     *            The typed query to execute
     * @return The single result or null (if none or multiple)
     * @throws ModelException
     *             The persistence exception
     */
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

    /**
     * Get the class of the entity
     * 
     * @return the class of the entity
     */
    protected Class<E> getEntityClass() {
        return this.entityClass;
    }
}
