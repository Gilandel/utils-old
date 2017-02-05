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
import java.util.Iterator;
import java.util.List;

import javax.persistence.Query;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import fr.landel.utils.model.AbstractEntity;
import fr.landel.utils.model.query.QueryBuilder1;

/**
 * Helper class to build hql request filter.
 *
 * @since Nov 24, 2015
 * @author Erwan Ropartz
 * @author Gilles Landel
 *
 */
@Component
public class FilterHelper {

    /**
     * Build a hql condition with the filters.
     *
     * @param filters
     *            the filters
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return The HQL condition
     */
    public final <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> String buildFilters(
            final List<FilterInfo<E, K>> filters) {
        final List<CharSequence> builder = new ArrayList<>();

        if (CollectionUtils.isNotEmpty(filters)) {

            FilterInfo<E, K> filter = null;
            for (final Iterator<FilterInfo<E, K>> it = filters.iterator(); it.hasNext();) {
                filter = it.next();
                if (filter.isBeginConcat()) {
                    builder.add(QueryBuilder1.PARENTHESIS_OPEN);
                }
                builder.add(this.buildFilter(filter));
                if (filter.isEndConcat()) {
                    builder.add(QueryBuilder1.PARENTHESIS_CLOSE);
                }
                if (it.hasNext()) {
                    if (filter.isAnd()) {
                        builder.add(QueryBuilder1.AND);
                    } else {
                        builder.add(QueryBuilder1.OR);
                    }
                }
            }
        }

        return StringUtils.join(builder, QueryBuilder1.SPACE);
    }

    /**
     * Build the query parameter for a hql request.
     *
     * @param query
     *            the query
     * @param filters
     *            The filters
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     */
    public final <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> void buildFilterParameters(final Query query,
            final List<FilterInfo<E, K>> filters) {
        for (FilterInfo<E, K> currentFilter : filters) {
            if (currentFilter.getColumn() != null) {
                if (currentFilter.getValue() != null) {
                    query.setParameter(currentFilter.getParameter(), currentFilter.getValue());
                } else if (currentFilter.getValues() != null) {
                    List<Object> sousFiltre = new ArrayList<>();
                    for (FilterInfo<E, K> valeur : currentFilter.getValues()) {
                        sousFiltre.add(valeur.getValue());
                    }
                    query.setParameter(currentFilter.getParameter(), sousFiltre);
                }
            } else {
                this.buildFilterParameters(query, currentFilter.getValues());
            }

        }
    }

    /**
     * Build a hql condition for a filter.
     *
     * @param filter
     *            The filter
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return The hql condition
     */
    private <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> String buildFilter(final FilterInfo<E, K> filter) {
        final List<CharSequence> builder = new ArrayList<>();

        if (FilterInfo.OP_EXISTS.equals(filter.getOperator())) {
            // QueryBuilder<E, K> query = new QueryBuilder<>(filter.getTable());

            // query.select("1").from().where(query)

            builder.add("EXISTS ( SELECT 1 FROM");
            builder.add(filter.getTable().getSimpleName());
            builder.add(QueryBuilder1.WHERE);
            builder.add(this.buildFilters(filter.getValues()));
            builder.add(QueryBuilder1.PARENTHESIS_CLOSE);
        } else {
            if (filter.getCaseSensitive()) {
                builder.add(filter.getColumn());
            } else {
                if (FilterInfo.TYPE_LONG.equals(filter.getType()) || FilterInfo.TYPE_INTEGER.equals(filter.getType())
                        || FilterInfo.TYPE_BOOLEAN.equals(filter.getType())) {
                    builder.add(filter.getColumn());
                } else {
                    builder.add("UPPER(");
                    builder.add(filter.getColumn());
                    builder.add(")");
                }
                filter.setValue(filter.getValue().toString().toUpperCase());
            }

            if (FilterInfo.OP_LIKE.equals(filter.getOperator()) || FilterInfo.OP_EQ.equals(filter.getOperator())) {
                builder.add(this.buildSimpleFilterValue(filter));
            } else if (FilterInfo.OP_IN_LIST.equals(filter.getOperator())) {
                builder.add(this.buildListFilterValue(filter));
            }
        }

        return StringUtils.join(builder, QueryBuilder1.SPACE);
    }

    /**
     * Build a hql condition for a simple filter (OP_LIKE or OP_EQ).
     *
     * @param filter
     *            The filter
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return The hql condition
     */
    private <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> String buildSimpleFilterValue(
            final FilterInfo<E, K> filter) {
        StringBuilder queryBuilder = new StringBuilder();

        if (FilterInfo.OP_LIKE.equals(filter.getOperator())) {
            queryBuilder.append(" LIKE :");
        } else if (FilterInfo.OP_EQ.equals(filter.getOperator())) {
            queryBuilder.append(" = :");
        }

        if (filter.getType() == null || FilterInfo.TYPE_STRING.equals(filter.getType())) {
            filter.setValue(filter.getValue().toString());
        } else if (FilterInfo.TYPE_BOOLEAN.equals(filter.getType())) {
            filter.setValue(Boolean.parseBoolean(filter.getValue().toString()));
        } else if (FilterInfo.TYPE_INTEGER.equals(filter.getType())) {
            filter.setValue(Integer.parseInt(filter.getValue().toString()));
        } else if (FilterInfo.TYPE_LONG.equals(filter.getType())) {
            filter.setValue(Long.parseLong(filter.getValue().toString()));
        }

        queryBuilder.append(filter.getParameter());

        return queryBuilder.toString();
    }

    /**
     * Build a hql condition for a list (OP_IN_LIST).
     *
     * @param filter
     *            The filter
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return The hql condition
     */
    private <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> String buildListFilterValue(
            final FilterInfo<E, K> filter) {
        StringBuilder queryBuilder = new StringBuilder();

        queryBuilder.append(" IN ( :");
        queryBuilder.append(filter.getParameter());
        queryBuilder.append(") ");
        return queryBuilder.toString();
    }
}
