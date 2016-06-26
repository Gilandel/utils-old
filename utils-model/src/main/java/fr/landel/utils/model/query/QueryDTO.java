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

import java.util.Arrays;
import java.util.List;

/**
 * Query DTO.
 *
 * @since 30 nov. 2015
 * @author Gilles
 *
 */
public class QueryDTO extends AbstractQueryBuilder {

    /**
     * Serial
     */
    private static final long serialVersionUID = 2202010080323964100L;

    private static final String NEW = "NEW";

    /**
     * Constructor.
     * 
     * @param dtoClass
     *            The DTO class
     * @param fields
     *            The fields or columns
     */
    public QueryDTO(final Class<?> dtoClass, final List<String> fields) {
        add(NEW);
        add(dtoClass.getCanonicalName());
        add(PARENTHESIS_OPEN);
        addAll(fields);
        add(PARENTHESIS_CLOSE);
    }

    /**
     * Constructor.
     * 
     * @param dtoClass
     *            The DTO class
     * @param fields
     *            The fields or columns
     */
    public QueryDTO(final Class<?> dtoClass, final String... fields) {
        this(dtoClass, Arrays.asList(fields));
    }
}