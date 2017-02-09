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

import fr.landel.utils.model.AbstractEntity;
import fr.landel.utils.model.exception.ModelException;

/**
 * Definition of methods for all DAO.
 *
 * @since Jul 13, 2015
 * @author Erwan Ropartz
 * @author Gilles Landel
 * @param <E>
 *            Class of the entity
 * @param <K>
 *            Class of the primary key
 *
 */
public interface DAO<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends Serializable {

    /**
     * Find an entity by primary key.
     *
     * @param id
     *            Identifier of the entity
     * @return Detail of the entity
     * @throws ModelException
     *             On persistence exception
     */
    E find(K id) throws ModelException;

    /**
     * Flush and clear the current transaction session
     * 
     * @throws ModelException
     *             The persistence exception
     */
    void flushAndClear() throws ModelException;

    /**
     * Flush the current transaction session
     * 
     * @throws ModelException
     *             The persistence exception
     */
    void flush() throws ModelException;

    /**
     * Refresh the entity
     * 
     * @param entity
     *            The entity to refresh
     * @throws ModelException
     *             The persistence exception
     */
    void refresh(final E entity) throws ModelException;
}
