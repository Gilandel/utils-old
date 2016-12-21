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
package fr.landel.utils.model.query;

/**
 * Query order.
 *
 * @since Nov 30, 2015
 * @author Gilles
 *
 */
public final class QueryOrder {

    /**
     * Ascendant order
     */
    protected static final String ORDER_ASC = "ASC";

    /**
     * Descendant order
     */
    protected static final String ORDER_DESC = "DESC";

    /**
     * Order
     */
    private String order;

    /**
     * Column
     */
    private String column;

    /**
     * Constructor.
     * 
     * @param column
     *            The column
     * @param order
     *            The order
     */
    private QueryOrder(final String column, final String order) {
        this.order = order;
        this.column = column;
    }

    @Override
    public String toString() {
        return this.column + QueryBuilder.SPACE + this.order;
    }

    /**
     * Build an ASC order query.
     * 
     * @param column
     *            the column
     * @return The order object
     */
    public static QueryOrder asc(final String column) {
        return new QueryOrder(column, ORDER_ASC);
    }

    /**
     * Build an DESC order query.
     * 
     * @param column
     *            the column
     * @return The order object
     */
    public static QueryOrder desc(final String column) {
        return new QueryOrder(column, ORDER_DESC);
    }
}
