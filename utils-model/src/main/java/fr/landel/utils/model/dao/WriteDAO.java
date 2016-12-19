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

import fr.landel.utils.model.AbstractEntity;
import fr.landel.utils.model.exception.ModelException;

/**
 * Definition of methods for all write DAO.
 *
 * @see <a href="http://martinfowler.com/bliki/CQRS.html">Command Query
 *      Responsibility Segregation</a>
 *
 * @since 13 juil. 2015
 * @author Erwan Ropartz
 * @author Gilles Landel
 * @param <E>
 *            Class of the entity
 * @param <K>
 *            Class of the primary key
 */
public interface WriteDAO<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends DAO<E, K> {

    /**
     * Create an entity.
     *
     * @param entity
     *            The entity to create
     * @return The entity created
     * @throws ModelException
     *             On persistence exception
     */
    E create(E entity) throws ModelException;

    /**
     * Update an entity.
     *
     * @param entity
     *            The entity to update
     * @return The entity updated
     * @throws ModelException
     *             On persistence exception
     */
    E update(E entity) throws ModelException;

    /**
     * Update an entity without change the fields who's null.
     *
     * @param entity
     *            The entity to update
     * @return The entity updated
     * @throws ModelException
     *             On persistence exception
     */
    E updateModification(E entity) throws ModelException;

    /**
     * Delete an entity.
     *
     * @param entity
     *            The entity
     * @throws ModelException
     *             On persistence exception
     */
    void delete(E entity) throws ModelException;

    /**
     * Delete an entity.
     *
     * @param id
     *            Identifier of the entity
     * @throws ModelException
     *             On persistence exception
     */
    void delete(K id) throws ModelException;
}
