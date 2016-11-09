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

    @Test
    public void testSelect() {
        QueryBuilder<EntityParent, String> query = new QueryBuilder<>(EntityParent.class, "p");

        System.out.println(query.select("1").from("p").join("p.children", "c").where().in("c.id", "p1"));

        query = new QueryBuilder<>(EntityParent.class, "p");

        System.out.println(
                query.select(Point.class, "p.valInteger", "c.valInteger").from().innerJoin("p.children", "c").where().isNotEmpty("c.name"));

        query = new QueryBuilder<>(EntityParent.class, "p");

        System.out.println(query.select("1").from("p").innerJoin("p.children", "c").groupBy("c.title").orderBy(QueryOrder.asc("c.id"))
                .having().isGT("c.size", "pSize").orderBy(QueryOrder.desc("c.name")));
    }

    @Test
    public void testInsert() {
        QueryBuilder<EntityParent, String> query = new QueryBuilder<>(EntityParent.class, "p");
        System.out.println(query.insert());
    }

    @Test
    public void testUpdate() {
        QueryBuilder<EntityParent, String> query = new QueryBuilder<>(EntityParent.class, "p");
        System.out.println(query.update());
    }

    @Test
    public void testDelete() {
        QueryBuilder<EntityParent, String> query = new QueryBuilder<>(EntityParent.class, "p");
        System.out.println(query.delete());
    }
}
