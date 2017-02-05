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
package fr.landel.utils.model.filter;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;

import fr.landel.utils.model.AbstractEntity;

/**
 * Filter page request.
 *
 * @since Jul 14, 2015
 * @author Erwan Ropartz
 * @author Gilles Landel
 *
 */
public class FilterPageRequest<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends PageRequest
        implements FilterPageable<E, K> {

    /**
     * Serial version.
     */
    private static final long serialVersionUID = -239376367006463741L;

    /**
     * Filters for this request.
     */
    private List<FilterInfo<E, K>> filters = new ArrayList<>();

    /**
     * List of groupBy to add in the request.
     */
    private List<String> groupBy = new ArrayList<>();

    /**
     * List of joins to add in the request.
     * 
     */
    private Map<String, String> joins = new HashMap<>();

    /**
     * Default constructor.
     *
     * @param page
     *            Number of the requested page (subtract 1, so minimum value as
     *            to be 1)
     * @param size
     *            Size of this page
     */
    public FilterPageRequest(final Integer page, final Integer size) {
        super(page - 1, size);
    }

    /**
     * Constructor with direction.
     *
     * @param page
     *            Number of the requested page (subtract 1, so minimum value as
     *            to be 1)
     * @param size
     *            Size of this page
     * @param sort
     *            Direction for the request
     * @param sidx
     *            Column to sort
     */
    public FilterPageRequest(final Integer page, final Integer size, final Direction sort, final String sidx) {
        super(page - 1, size, sort, sidx);
    }

    /**
     * Constructor with direction.
     *
     * @param page
     *            Number of the requested page (subtract 1, so minimum value as
     *            to be 1)
     * @param size
     *            Size of this page
     * @param sort
     *            The sort
     */
    public FilterPageRequest(final Integer page, final Integer size, final Sort sort) {
        super(page - 1, size, sort);
    }

    /**
     * Default constructor.
     *
     * @param page
     *            The requested page
     */
    public FilterPageRequest(final Pageable page) {
        super(page.getPageNumber(), page.getPageSize(), page.getSort());
    }

    @Override
    public List<FilterInfo<E, K>> getFilters() {
        return this.filters;
    }

    @Override
    public List<String> getGroupBy() {
        return this.groupBy;
    }

    @Override
    public Map<String, String> getJoins() {
        return this.joins;
    }

    public void setFilters(final List<FilterInfo<E, K>> filters) {
        this.filters = filters;
    }

    public void setGroupBy(final List<String> groupBy) {
        this.groupBy = groupBy;
    }

    public void setJoins(final Map<String, String> joins) {
        this.joins = joins;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        if (this.filters != null) {
            result = prime * result + this.filters.hashCode();
        }
        if (this.groupBy != null) {
            result = prime * result + this.groupBy.hashCode();
        }
        if (this.joins != null) {
            result = prime * result + this.joins.hashCode();
        }
        return result;
    }

    @Override
    public boolean equals(final Object obj) {
        return super.equals(obj);
    }

}
