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
package fr.landel.utils.model.filter;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import org.springframework.data.domain.Pageable;

import fr.landel.utils.model.AbstractEntity;

/**
 * Interface for a pageable request.
 *
 * @since Aug 5, 2015
 * @author Erwan Ropartz
 * @author Gilles Landel
 *
 */
public interface FilterPageable<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends Pageable {

    /**
     * Get filters for a pageable request.
     *
     * @return The filters
     */
    List<FilterInfo<E, K>> getFilters();

    /**
     * List of joins to add in the HQL request.
     *
     * @return List of joins
     */
    Map<String, String> getJoins();

    /**
     * List of group by to add in the HQL request.
     *
     * @return List of group by
     */
    List<String> getGroupBy();
}
