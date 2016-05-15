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

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import fr.landel.utils.model.query.QueryBuilder;
import fr.landel.utils.model.query.QueryCondition;

/**
 * Check query condition.
 *
 * @since 30 nov. 2015
 * @author Gilles
 *
 */
public class QueryConditionTest {

    /**
     * Constructor
     */
    public QueryConditionTest() {
    }

    /**
     * Test method for {@link QueryCondition#toString()}.
     */
    @Test
    public void testToString() {
        final String space = " ";
        final String param = "param";
        final String value = "value";
        final String valueParam = ":value";

        QueryCondition queryCondition = new QueryCondition(param, value);
        assertEquals(queryCondition.toString(), param + space + QueryCondition.EQUAL + space + valueParam);

        queryCondition = new QueryCondition(param, valueParam);
        assertEquals(queryCondition.toString(), param + space + QueryCondition.EQUAL + space + valueParam);

        queryCondition = new QueryCondition(param, QueryCondition.EQUAL, value);
        assertEquals(queryCondition.toString(), param + space + QueryCondition.EQUAL + space + valueParam);

        queryCondition = new QueryCondition(param, QueryCondition.NOT_EQUAL, value);
        assertEquals(queryCondition.toString(), param + space + QueryCondition.NOT_EQUAL + space + valueParam);

        queryCondition = new QueryCondition(param, QueryCondition.INFERIOR, value);
        assertEquals(queryCondition.toString(), param + space + QueryCondition.INFERIOR + space + valueParam);

        queryCondition = new QueryCondition(param, QueryCondition.INFERIOR_EQUAL, value);
        assertEquals(queryCondition.toString(), param + space + QueryCondition.INFERIOR_EQUAL + space + valueParam);

        queryCondition = new QueryCondition(param, QueryCondition.SUPERIOR, value);
        assertEquals(queryCondition.toString(), param + space + QueryCondition.SUPERIOR + space + valueParam);

        queryCondition = new QueryCondition(param, QueryCondition.SUPERIOR_EQUAL, value);
        assertEquals(queryCondition.toString(), param + space + QueryCondition.SUPERIOR_EQUAL + space + valueParam);

        queryCondition = new QueryCondition(param, QueryCondition.LIKE, value);
        assertEquals(queryCondition.toString(), param + space + QueryCondition.LIKE + space + valueParam);

        queryCondition = new QueryCondition(param, QueryCondition.LIKE, value);
        assertEquals(queryCondition.toString(), param + space + QueryCondition.LIKE + space + valueParam);

        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.selectEntity("entity");
        queryCondition = new QueryCondition(query, QueryCondition.LIKE, value);
        assertEquals(queryCondition.toString(), "( SELECT entity ) " + QueryCondition.LIKE + space + valueParam);
    }
}
