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

import org.junit.Ignore;

import fr.landel.utils.model.mappable.EntityParent;

/**
 * Check query condition.
 *
 * @since 30 nov. 2015
 * @author Gilles
 *
 */
public class QueryCondition1Test {

    /**
     * Test method for {@link QueryCondition1#toString()}.
     */
    @Ignore
    public void testToString() {
        final String space = " ";
        final String param = "param";
        final String value = "value";
        final String valueParam = ":value";

        QueryBuilder1<EntityParent, String> query = new QueryBuilder1<>(EntityParent.class, "p");

        QueryCondition1<EntityParent, String> queryCondition = query.where().isEqual(param, value);
        assertEquals(param + space + QueryOperator.EQUALS + space + valueParam, queryCondition.toString());

        query = new QueryBuilder1<>(EntityParent.class, "p");
        queryCondition = query.where().isEqual(param, valueParam);
        assertEquals(param + space + QueryOperator.EQUALS + space + valueParam, queryCondition.toString());

        query = new QueryBuilder1<>(EntityParent.class, "p");
        queryCondition = query.where().of(param, QueryOperator.EQUALS, value);
        assertEquals(param + space + QueryOperator.EQUALS + space + valueParam, queryCondition.toString());

        query = new QueryBuilder1<>(EntityParent.class, "p");
        queryCondition = query.where().isNotEqual(param, value);
        assertEquals(param + space + QueryOperator.NOT_EQUALS + space + valueParam, queryCondition.toString());

        query = new QueryBuilder1<>(EntityParent.class, "p");
        queryCondition = query.where().isLT(param, value);
        assertEquals(param + space + QueryOperator.LOWER + space + valueParam, queryCondition.toString());

        query = new QueryBuilder1<>(EntityParent.class, "p");
        queryCondition = query.where().isLTE(param, value);
        assertEquals(param + space + QueryOperator.LOWER_EQUALS + space + valueParam, queryCondition.toString());

        query = new QueryBuilder1<>(EntityParent.class, "p");
        queryCondition = query.where().isGT(param, value);
        assertEquals(param + space + QueryOperator.GREATER + space + valueParam, queryCondition.toString());

        query = new QueryBuilder1<>(EntityParent.class, "p");
        queryCondition = query.where().isGTE(param, value);
        assertEquals(param + space + QueryOperator.GREATER_EQUALS + space + valueParam, queryCondition.toString());

        query = new QueryBuilder1<>(EntityParent.class, "p");
        queryCondition = query.where().like(param, value);
        assertEquals(param + space + QueryOperator.LIKE + space + valueParam, queryCondition.toString());

        query = new QueryBuilder1<>(EntityParent.class, "p");
        queryCondition = query.where().like(param, value);
        assertEquals(param + space + QueryOperator.LIKE + space + valueParam, queryCondition.toString());

        QueryBuilder1<Zone, Integer> query2 = new QueryBuilder1<>(Zone.class, "zone");
        query.selectEntity("entity");
        QueryCondition1<Zone, Integer> queryCondition2 = query2.where().like(query, value);
        assertEquals("( SELECT entity ) " + QueryOperator.LIKE + space + valueParam, queryCondition2.toString());
    }
}
