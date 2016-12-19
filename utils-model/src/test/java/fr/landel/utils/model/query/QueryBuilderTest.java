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

import java.awt.Point;

import org.junit.Test;

import fr.landel.utils.model.mappable.EntityParent;

public class QueryBuilderTest {

    /*
     * 
     * select * from (select * from test) as t; select * from t where b in
     * (select * from test)
     * 
     */

    @Test
    public void testSelect() {
        QueryBuilder<EntityParent, String> query = QueryBuilder.of(EntityParent.class, "p");

        query.select("1").from("p").join("p.children", "c").where().in("c.id", "p1");
        System.out.println(query);

        query = QueryBuilder.of(EntityParent.class, "p");

        query.select(Point.class, "p.valInteger", "c.valInteger").from().innerJoin("p.children", "c").where().isNotEmpty("c.name");
        System.out.println(query);

        query = QueryBuilder.of(EntityParent.class, "p");

        query.select("1").from("p").innerJoin("p.children", "c").groupBy("c.title").orderBy(QueryOrder.asc("c.id")).having()
                .isGT("c.size", "pSize").orderBy(QueryOrder.desc("c.name"));
        System.out.println(query);

        query = QueryBuilder.of(EntityParent.class, "p");
        QueryBuilder<EntityParent, String> subquery = QueryBuilder.of(EntityParent.class, "x");

        QueryDTO queryDTO = QueryDTOBuilder.of(Point.class).append(QueryFunction.max("p.valInteger")).append(subquery.select().from())
                .build();
        query.select(queryDTO).from().groupBy("p.children");
        System.out.println(query);
    }

    @Test
    public void testInsert() {
        QueryBuilder<EntityParent, String> query = QueryBuilder.of(EntityParent.class, "p");
        System.out.println(query.insert());
    }

    @Test
    public void testUpdate() {
        QueryBuilder<EntityParent, String> query = QueryBuilder.of(EntityParent.class, "p");
        System.out.println(query.update());
    }

    @Test
    public void testDelete() {
        QueryBuilder<EntityParent, String> query = QueryBuilder.of(EntityParent.class, "p");
        System.out.println(query.delete());
    }
}
