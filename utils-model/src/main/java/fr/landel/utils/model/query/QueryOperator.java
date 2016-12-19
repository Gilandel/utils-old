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
package fr.landel.utils.model.query;

import java.util.List;

import fr.landel.utils.assertor.Assertor;
import fr.landel.utils.commons.function.TriConsumer;

/**
 * List of query operators
 * 
 * @since Aug 13, 2016
 * @author Gilles
 *
 */
public enum QueryOperator {

    /**
     * The '=' operator
     */
    EQUALS("=", 1, createColumnApplier("="), createQueryApplier("=")),

    /**
     * The '&lt;&gt;' operator
     */
    NOT_EQUALS("<>", 1, createColumnApplier("<>"), createQueryApplier("<>")),

    /**
     * The '&gt;' operator
     */
    GREATER(">", 1, createColumnApplier(">"), createQueryApplier(">")),

    /**
     * The '&gt;=' operator
     */
    GREATER_EQUALS(">=", 1, createColumnApplier(">="), createQueryApplier(">=")),

    /**
     * The '&lt;' operator
     */
    LOWER("<", 1, createColumnApplier("<"), createQueryApplier("<")),

    /**
     * The '&lt;=' operator
     */
    LOWER_EQUALS("<=", 1, createColumnApplier("<="), createQueryApplier("<=")),

    /**
     * The 'like' operator
     */
    LIKE("LIKE", 1, createColumnApplier("LIKE"), createQueryApplier("LIKE")),

    /**
     * The 'similar to' operator
     */
    SIMILAR_TO("SIMILAR TO", 1, createColumnApplier("SIMILAR TO"), createQueryApplier("SIMILAR TO")),

    /**
     * The 'not similar to' operator
     */
    NOT_SIMILAR_TO("NOT SIMILAR TO", 1, createColumnApplier("NOT SIMILAR TO"), createQueryApplier("NOT SIMILAR TO")),

    /**
     * The 'in' operator
     */
    IN("IN", 1, (column, parameters, output) -> {
        output.add(column);
        output.add("IN");
        output.add("(");
        output.add(parameters[0]);
        output.add(")");
    }, (query, parameters, output) -> {
        output.add("(");
        output.addAll(query.getBuilder());
        output.add(")");
        output.add("IN");
        output.add("(");
        output.add(parameters[0]);
        output.add(")");
    }),

    /**
     * The 'not in' operator
     */
    NOT_IN("NOT IN", 1, (column, parameters, output) -> {
        output.add(column);
        output.add("NOT IN");
        output.add("(");
        output.add(parameters[0]);
        output.add(")");
    }, (query, parameters, output) -> {
        output.add("(");
        output.addAll(query.getBuilder());
        output.add(")");
        output.add("NOT IN");
        output.add("(");
        output.add(parameters[0]);
        output.add(")");
    }),

    /**
     * The 'is null' operator
     */
    IS_NULL("IS NULL", 0, createColumnApplierNoParam("IS NULL"), createQueryApplierNoParam("IS NULL")),

    /**
     * The 'is not null' operator
     */
    IS_NOT_NULL("IS NOT NULL", 0, createColumnApplierNoParam("IS NOT NULL"), createQueryApplierNoParam("IS NOT NULL")),

    /**
     * The 'is true' operator
     */
    IS_TRUE("IS TRUE", 0, createColumnApplierNoParam("IS TRUE"), createQueryApplierNoParam("IS TRUE")),

    /**
     * The 'is true' operator
     */
    IS_NOT_TRUE("IS NOT TRUE", 0, createColumnApplierNoParam("IS NOT TRUE"), createQueryApplierNoParam("IS NOT TRUE")),

    /**
     * The 'is false' operator
     */
    IS_FALSE("IS FALSE", 0, createColumnApplierNoParam("IS FALSE"), createQueryApplierNoParam("IS FALSE")),

    /**
     * The 'is not false' operator
     */
    IS_NOT_FALSE("IS NOT FALSE", 0, createColumnApplierNoParam("IS NOT FALSE"), createQueryApplierNoParam("IS NOT FALSE")),

    /**
     * The 'is empty' operator
     */
    IS_EMPTY("IS EMPTY", 0, createColumnApplierNoParam("IS EMPTY"), createQueryApplierNoParam("IS EMPTY")),

    /**
     * The 'is not empty' operator
     */
    IS_NOT_EMPTY("IS NOT EMPTY", 0, createColumnApplierNoParam("IS NOT EMPTY"), createQueryApplierNoParam("IS NOT EMPTY")),

    /**
     * The 'member of' operator
     */
    MEMBER_OF("MEMBER OF", 1, createColumnApplier("MEMBER OF"), createQueryApplier("MEMBER OF")),

    /**
     * The 'is not member of' operator
     */
    NOT_MEMBER_OF("NOT MEMBER OF", 1, createColumnApplier("NOT MEMBER OF"), createQueryApplier("NOT MEMBER OF")),

    /**
     * The 'exists' operator
     */
    EXISTS("EXISTS", 0, null, (query, parameters, output) -> {
        output.add("EXISTS");
        output.add("(");
        output.addAll(query.getBuilder());
        output.add(")");
    }),

    /**
     * The 'not exists' operator
     */
    NOT_EXISTS("NOT EXISTS", 0, null, (query, parameters, output) -> {
        output.add("NOT EXISTS");
        output.add("(");
        output.addAll(query.getBuilder());
        output.add(")");
    }),

    /**
     * The 'between x and y' operator
     */
    BETWEEN("BETWEEN", 2, (column, parameters, output) -> {
        output.add(column);
        output.add("BETWEEN");
        output.add(parameters[0]);
        output.add("AND");
        output.add(parameters[1]);
    }, (query, parameters, output) -> {
        output.add("(");
        output.addAll(query.getBuilder());
        output.add(")");
        output.add("BETWEEN");
        output.add(parameters[0]);
        output.add("AND");
        output.add(parameters[1]);
    });

    private final String text;
    private final int nbParams;
    private final TriConsumer<CharSequence, CharSequence[], List<CharSequence>> columnApplier;
    private final TriConsumer<AbstractBuilder<?, ?>, CharSequence[], List<CharSequence>> queryApplier;

    private QueryOperator(final String text, final int nbParams,
            final TriConsumer<CharSequence, CharSequence[], List<CharSequence>> columnApplier,
            final TriConsumer<AbstractBuilder<?, ?>, CharSequence[], List<CharSequence>> queryApplier) {

        this.text = text;
        this.nbParams = nbParams;
        this.columnApplier = columnApplier;
        this.queryApplier = queryApplier;
    }

    /**
     * @return the columnApplier
     */
    public TriConsumer<CharSequence, CharSequence[], List<CharSequence>> getColumnApplier() {
        return this.columnApplier;
    }

    /**
     * @return the queryApplier
     */
    public TriConsumer<AbstractBuilder<?, ?>, CharSequence[], List<CharSequence>> getQueryApplier() {
        return this.queryApplier;
    }

    public void isParamsNumberCorrect(final int number) {
        Assertor.that(number).isEqual(this.nbParams).toThrow();
    }

    @Override
    public String toString() {
        return this.text;
    }

    private static TriConsumer<CharSequence, CharSequence[], List<CharSequence>> createColumnApplier(final CharSequence operator) {
        return (column, parameters, output) -> {
            output.add(column);
            output.add(operator);
            output.add(parameters[0]);
        };
    }

    private static TriConsumer<AbstractBuilder<?, ?>, CharSequence[], List<CharSequence>> createQueryApplier(final CharSequence operator) {
        return (query, parameters, output) -> {
            output.add("(");
            output.addAll(query.getBuilder());
            output.add(")");
            output.add(operator);
            output.add(parameters[0]);
        };
    }

    private static TriConsumer<CharSequence, CharSequence[], List<CharSequence>> createColumnApplierNoParam(final CharSequence operator) {
        return (column, parameters, output) -> {
            output.add(column);
            output.add(operator);
        };
    }

    private static TriConsumer<AbstractBuilder<?, ?>, CharSequence[], List<CharSequence>> createQueryApplierNoParam(
            final CharSequence operator) {
        return (query, parameters, output) -> {
            output.add("(");
            output.addAll(query.getBuilder());
            output.add(")");
            output.add(operator);
        };
    }
}
