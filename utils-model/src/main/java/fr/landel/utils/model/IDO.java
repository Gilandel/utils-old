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
package fr.landel.utils.model;

import java.io.Serializable;

/**
 * Identifiable Data Object
 *
 * @since 15 sept. 2015
 * @author Gilles
 *
 * @param <D>
 *            The type of the data object
 * @param <K>
 *            The type of the identifier
 */
public interface IDO<D extends IDO<D, K>, K extends Serializable> extends Serializable {

    /**
     * @return The primary key
     */
    K getPrimaryKey();
}
