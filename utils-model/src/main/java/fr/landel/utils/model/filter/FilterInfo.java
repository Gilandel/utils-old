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

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

import fr.landel.utils.model.AbstractEntity;

/**
 * Filter.
 *
 * @since 14 juil. 2015
 * @author Erwan Ropartz
 * @author Gilles Landel
 *
 */
@JsonInclude(Include.NON_NULL)
public class FilterInfo<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> implements Serializable {

    /**
     * Percent symbol (like)
     */
    public static final String PERCENT = "%";

    /**
     * Equals operator.
     */
    public static final String OP_EQ = "EQ";

    /**
     * like operator.
     */
    public static final String OP_LIKE = "LIKE";

    /**
     * Like operator.
     */
    public static final String OP_EXISTS = "EXITS";

    /**
     * In operator.
     */
    public static final String OP_IN_LIST = "IN_LIST";

    /**
     * String type.
     */
    public static final String TYPE_STRING = String.class.getSimpleName();

    /**
     * Integer type.
     */
    public static final String TYPE_INTEGER = Integer.class.getSimpleName();

    /**
     * Boolean type.
     */
    public static final String TYPE_BOOLEAN = Boolean.class.getSimpleName();

    /**
     * Boolean type.
     */
    public static final String TYPE_LONG = Long.class.getSimpleName();

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 600923812582347689L;

    /**
     * Operator.
     */
    private String operator;

    /**
     * Column.
     */
    private String column;

    /**
     * Table.
     */
    private Class<E> table;

    /**
     * Case Sensitive.
     */
    private boolean caseSensitive;

    /**
     * Value.
     */
    private Object value;

    /**
     * Values.
     */
    private List<FilterInfo<E, K>> values;

    /**
     * Type of the parameter.
     */
    private String type;

    /**
     * Name of the parameter in the hql request.
     */
    private String parameter;

    /**
     * If the filters have to be combined by AND (otherwise OR)
     */
    private boolean and = true;

    /**
     * 
     */
    private boolean beginConcat;

    /**
     * 
     */
    private boolean endConcat;

    /**
     * Constructeur.
     */
    public FilterInfo() {
    }

    /**
     * Constructor for filter a value.
     *
     * @param table
     *            Table to filter
     * @param column
     *            Column to filter
     * @param value
     *            Value
     * @param caseSensitive
     *            Filter case sensitive
     * @param operator
     *            Operator to filter
     * @param type
     *            filter Type
     * @param parameter
     *            parameter
     */
    public FilterInfo(final Class<E> table, final String column, final Object value, final boolean caseSensitive, final String operator,
            final String type, final String parameter) {
        this(table, column, caseSensitive, operator, parameter);

        this.value = value;
        this.type = type;
    }

    /**
     * Constructor for filter a list of values.
     *
     * @param column
     *            Column to filter
     * @param table
     *            Table
     * @param operator
     *            Operator to filter
     * @param values
     *            filter Values
     * @param parameter
     *            parameter
     */
    public FilterInfo(final String column, final Class<E> table, final String operator, final List<FilterInfo<E, K>> values,
            final String parameter) {
        this(table, column, Boolean.TRUE, operator, parameter);

        this.values = values;
    }

    private FilterInfo(final Class<E> table, final String column, final boolean caseSensitive, final String operator,
            final String parameter) {

        this.table = table;
        this.column = column;
        this.caseSensitive = caseSensitive;
        this.operator = operator;
        this.parameter = parameter;
    }

    /**
     * @return the operator
     */
    public String getOperator() {
        return this.operator;
    }

    /**
     * @param operator
     *            the operator to set
     */
    public void setOperator(final String operator) {
        this.operator = operator;
    }

    /**
     * @return the column
     */
    public String getColumn() {
        return this.column;
    }

    /**
     * @param column
     *            the column to set
     */
    public void setColumn(final String column) {
        this.column = column;
    }

    /**
     * @return the table
     */
    public Class<E> getTable() {
        return this.table;
    }

    /**
     * @param table
     *            the table to set
     */
    public void setTable(final Class<E> table) {
        this.table = table;
    }

    /**
     * @return the caseSensitive
     */
    public boolean getCaseSensitive() {
        return this.caseSensitive;
    }

    /**
     * @param caseSensitive
     *            the caseSensitive to set
     */
    public void setCaseSensitive(final boolean caseSensitive) {
        this.caseSensitive = caseSensitive;
    }

    /**
     * @return the value
     */
    public Object getValue() {
        return this.value;
    }

    /**
     * @param value
     *            the value to set
     */
    public void setValue(final Object value) {
        this.value = value;
    }

    /**
     * @return the values
     */
    public List<FilterInfo<E, K>> getValues() {
        return this.values;
    }

    /**
     * @param values
     *            the values to set
     */
    public void setValues(final List<FilterInfo<E, K>> values) {
        this.values = values;
    }

    /**
     * @return the type
     */
    public String getType() {
        return this.type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(final String type) {
        this.type = type;
    }

    /**
     * @return the parameter
     */
    public String getParameter() {
        return this.parameter;
    }

    /**
     * @param parameter
     *            the parameter to set
     */
    public void setParameter(final String parameter) {
        this.parameter = parameter;
    }

    /**
     * @return the and
     */
    public boolean isAnd() {
        return this.and;
    }

    /**
     * @param and
     *            the and to set
     */
    public void setAnd(final boolean and) {
        this.and = and;
    }

    /**
     * @return the beginConcat
     */
    public boolean isBeginConcat() {
        return this.beginConcat;
    }

    /**
     * @param beginConcat
     *            the beginConcat to set
     */
    public void setBeginConcat(final boolean beginConcat) {
        this.beginConcat = beginConcat;
    }

    /**
     * @return the endConcat
     */
    public boolean isEndConcat() {
        return this.endConcat;
    }

    /**
     * @param endConcat
     *            the endConcat to set
     */
    public void setEndConcat(final boolean endConcat) {
        this.endConcat = endConcat;
    }
}
