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

import org.springframework.data.domain.Sort;

import fr.landel.utils.model.AbstractEntity;
import fr.landel.utils.model.exception.ModelException;
import fr.landel.utils.model.filter.FilterPageable;
import fr.landel.utils.model.query.QueryBuilder1;

/**
 * Definition of methods for all read DAO.
 * 
 * @see <a href="http://martinfowler.com/bliki/CQRS.html">Command Query
 *      Responsibility Segregation</a>
 *
 * @since Jul 13, 2015
 * @author Erwan Ropartz
 * @author Gilles Landel
 * @param <E>
 *            Class of the entity
 * @param <K>
 *            Class of the primary key
 */
public interface ReadDAO<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends DAO<E, K> {

    /**
     * Find entities by primary keys.
     *
     * @param ids
     *            Identifiers of the entities
     * @return Detail of the entities
     * @throws ModelException
     *             On persistence exception
     */
    List<E> find(List<K> ids) throws ModelException;

    /**
     * Check if an entity exist.
     *
     * @param id
     *            Identifier of the entity
     * @return <code>true</code> if the entity exists else otherwise
     */
    boolean exists(K id);

    /**
     * @return A list with all entities
     * @throws ModelException
     *             The persistence exception
     */
    List<E> findAll() throws ModelException;

    /**
     * Find all entities corresponding to the page request (Pagination and
     * filter).
     *
     * @param hqlSelection
     *            HQL clause to select entities
     * @param pageRequest
     *            the page request (Pagination and filter).
     * @return List of entities
     * @throws ModelException
     *             The persistence exception
     */
    List<E> findAll(String hqlSelection, FilterPageable<E, K> pageRequest) throws ModelException;

    /**
     * Find all entities corresponding to the page request (Pagination and
     * filter).
     *
     * @param qb
     *            Query builder HQL clause to select entities
     * @param pageRequest
     *            the page request (Pagination and filter).
     * @return List of entities
     * @throws ModelException
     *             The persistence exception
     */
    List<E> findAll(QueryBuilder1<E, K> qb, FilterPageable<E, K> pageRequest) throws ModelException;

    /**
     * Find all entities corresponding to the page request (Pagination and
     * filter).
     *
     * @param pageRequest
     *            the page request (Pagination and filter).
     * @return List of entities
     * @throws ModelException
     *             The persistence exception
     */
    List<E> findAll(FilterPageable<E, K> pageRequest) throws ModelException;

    /**
     * Find all entities and sort.
     *
     * @param sort
     *            The sort order
     * @return List of entities
     * @throws ModelException
     *             The persistence exception
     */
    List<E> findAll(Sort sort) throws ModelException;

    /**
     * Find all entities and set cacheable.
     * 
     * @param cacheable
     *            set or not cacheable
     * @return List of entities
     * @throws ModelException
     *             The persistence exception
     */
    List<E> findAll(boolean cacheable) throws ModelException;

    /**
     * @param clazz
     *            the common class type between selectables
     * @param selectables
     *            the columns to return
     * @param columnName
     *            the column name to filter
     * @param value
     *            The value
     * @return A list of objects matching with the condition
     * @throws ModelException
     *             The persistence exception.
     * @param <X>
     *            The class type
     */
    <X> List<X> findByColumn(Class<X> clazz, List<String> selectables, String columnName, Object value) throws ModelException;

    /**
     * @param clazz
     *            the common class type between selectables
     * @param selectables
     *            the columns to return
     * @param columns
     *            The columns
     * @return A list of objects matching with the condition
     * @throws ModelException
     *             The persistence exception
     * @param <X>
     *            The class type
     */
    <X> List<X> findByColumns(Class<X> clazz, List<String> selectables, Map<String, Object> columns) throws ModelException;

    /**
     * @param columnName
     *            the column name to filter
     * @param value
     *            The value
     * @return A list with entities matching with the condition
     * @throws ModelException
     *             The persistence exception
     */
    List<E> findByColumn(String columnName, Object value) throws ModelException;

    /**
     * @param columns
     *            The columns
     * @return A list with entities matching with the condition
     * @throws ModelException
     *             The persistence exception
     */
    List<E> findByColumns(Map<String, Object> columns) throws ModelException;

    /**
     * Counts the number of element following the column
     * 
     * @param column
     *            The column used for the count (if null replaces column by '*')
     * @return The number of elements
     * @throws ModelException
     *             The persistence exception
     */
    Long count(String column) throws ModelException;

    /**
     * Count all the entities corresponding to the filters.
     *
     * @param filters
     *            The filters
     * @return The number of entities corresponding to the filter
     * @throws ModelException
     *             The persistence exception
     */
    long countAll(FilterPageable<E, K> filters) throws ModelException;

}
