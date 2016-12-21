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
package fr.landel.utils.model.mapper;

import java.beans.Transient;

import fr.landel.utils.mapper.MapperException;

/**
 * DTO identifiers for tests.
 *
 * @since Nov 27, 2015
 * @author Gilles
 *
 */
public interface MyDTOIdentifier {

    /**
     * Child list (deep: 1)
     */
    String CHILD_LIST = "CHILD_LIST";

    /**
     * Child details (deep: 3)
     */
    String CHILD_DETAILS = "CHILD_DETAILS";

    /**
     * Parent list (deep: 1)
     */
    String PARENT_LIST = "PARENT_LIST";

    /**
     * Parent details (deep: 1)
     */
    String PARENT_DETAILS = "PARENT_DETAILS";

    /**
     * Parent max (deep: MAX)
     */
    String PARENT_MAX = "PARENT_MAX";

    /**
     * Parent list in save mode (deep: 1)
     */
    String PARENT_SAVE_LIST = "PARENT_SAVE_LIST";

    /**
     * Parent details in save mode (deep: 2)
     */
    String PARENT_SAVE_DETAILS = "PARENT_SAVE_DETAILS";

    /**
     * All lists (deep: 1)
     */
    String LIST = "LIST";

    /**
     * All details (deep: 2)
     */
    String DETAILS = "DETAILS";

    /**
     * To save all (deep: 1)
     */
    String SAVE = "SAVE";

    /**
     * All the identifiers (deep: MAX)
     */
    String ALL = "ALL";

    /**
     * Load the identifiers or do nothing
     * 
     * @throws MapperException
     *             If not implemented
     */
    @Transient
    default void load() throws MapperException {
        throw new MapperException("Not implemented");
    }
}
