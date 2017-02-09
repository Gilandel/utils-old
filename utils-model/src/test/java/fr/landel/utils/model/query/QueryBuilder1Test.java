/*
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
package fr.landel.utils.model.query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.apache.commons.lang3.StringUtils;
import org.junit.Ignore;
import org.junit.Test;

/**
 * Check query builder.
 *
 * @since Nov 30, 2015
 * @author Gilles
 *
 */
public class QueryBuilder1Test {

    /**
     * Test method for {@link QueryBuilder1#getAlias()}.
     */
    @Test
    public void testGetAlias() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        assertEquals(query.getAlias(), "zone");

        query = new QueryBuilder1<>(Zone.class);
        assertTrue(query.getAlias().matches("zone[\\da-f]+"));

        query = new QueryBuilder1<>(Zone.class, null);
        assertTrue(query.getAlias().matches("zone[\\da-f]+"));
    }

    /**
     * Test method for {@link QueryBuilder1#select(java.lang.String)} .
     */
    @Test
    public void testSelectString() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.select("entity");
        assertEquals(query.toString(), "SELECT entity");

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.select((String) null);
        assertEquals(query.toString(), "SELECT");

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.select("");
        assertEquals(query.toString(), "SELECT");
    }

    /**
     * Test method for {@link QueryBuilder1#select(QueryBuilder1)} .
     */
    @Test
    public void testSelectQueryBuilderOfC() {
        QueryBuilder1<Zone, Integer> queryZone = new QueryBuilder1<>(Zone.class);
        QueryBuilder1<Project, Integer> queryProject = new QueryBuilder1<>(Project.class);

        queryProject.select("entity");
        queryZone.select(queryProject);

        assertEquals(queryZone.toString(), "SELECT ( SELECT entity )");

    }

    /**
     * Test method for {@link QueryBuilder1#selectEntity()}.
     */
    @Test
    public void testSelectEntity() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class);
        query.selectEntity();
        assertTrue(query.toString().matches("SELECT zone[\\da-f]+"));

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.selectEntity();
        assertEquals(query.toString(), "SELECT zone");
    }

    /**
     * Test method for {@link QueryBuilder1#selectEntity(java.lang.String)} .
     */
    @Test
    public void testSelectEntityString() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.selectEntity("entity");
        assertEquals(query.toString(), "SELECT entity");

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.selectEntity(null);
        assertEquals(query.toString(), "SELECT zone");

        query = new QueryBuilder1<>(Zone.class, null);
        query.selectEntity(null);
        assertTrue(query.toString().matches("SELECT zone[\\da-f]+"));
    }

    /**
     * Test method for {@link QueryBuilder1#from()}.
     */
    @Test
    public void testFrom() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.from();
        assertEquals(query.toString(), "SELECT zone FROM " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder1<>(Zone.class);
        query.from();
        assertTrue(query.toString().matches("SELECT zone[\\da-f]+ FROM " + Zone.class.getCanonicalName() + " AS zone[\\da-f]+"));
    }

    /**
     * Test method for {@link QueryBuilder1#from(java.lang.String)} .
     */
    @Test
    public void testFromString() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.from("zone");
        assertEquals(query.toString(), "SELECT zone FROM " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder1<>(Zone.class);
        query.from("zone");
        assertEquals(query.toString(), "SELECT zone FROM " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder1<>(Zone.class);
        query.from(null);
        assertTrue(query.toString().matches("SELECT zone[\\da-f]+ FROM " + Zone.class.getCanonicalName() + " AS zone[\\da-f]+"));

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.from("z");
        assertEquals(query.toString(), "SELECT z FROM " + Zone.class.getCanonicalName() + " AS z");
    }

    /**
     * Test method for
     * {@link QueryBuilder1#from(java.lang.Class, java.lang.String)} .
     */
    @Test
    public void testFromEntityString() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.from(Project.class, "project");
        assertEquals(query.toString(), "FROM " + Project.class.getCanonicalName() + " AS project");

        query = new QueryBuilder1<>(Zone.class);
        query.from(Project.class, "");
        assertTrue(query.toString().matches("FROM " + Project.class.getCanonicalName() + " AS project[\\da-f]+"));

        query = new QueryBuilder1<>(Zone.class);
        query.from(Project.class, null);
        assertTrue(query.toString().matches("FROM " + Project.class.getCanonicalName() + " AS project[\\da-f]+"));
    }

    /**
     * Test method for {@link QueryBuilder1#add(QueryCondition)} .
     */
    @Ignore
    public void testAddCondition() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone").where("zone");
        assertEquals(query.toString(), "( zone )");
    }

    /**
     * Test method for {@link QueryBuilder1#where(QueryCondition)} .
     */
    @Ignore
    public void testWhereQueryCondition() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone").where("zone");
        assertEquals(query.toString(), "WHERE ( zone )");
    }

    /**
     * Test method for {@link QueryBuilder1#where(QueryBuilder1)} .
     */
    @Test
    public void testWhereQueryBuilderOfC() {
        QueryBuilder1<Zone, Integer> queryZone = new QueryBuilder1<>(Zone.class);
        QueryBuilder1<Project, Integer> queryProject = new QueryBuilder1<>(Project.class);

        queryProject.select("entity");

        queryZone.where(queryProject);
        assertEquals(queryZone.toString(), "WHERE ( SELECT entity )");
    }

    /**
     * Test method for {@link QueryBuilder1#whereIsNull(java.lang.String)} .
     */
    @Ignore
    public void testWhereIsNullCondition() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.whereIsNull("test.col");
        assertEquals(query.toString(), "WHERE ( test.col IS NULL )");
    }

    /**
     * Test method for {@link QueryBuilder1#whereIsNotNull(java.lang.String)} .
     */
    @Ignore
    public void testWhereIsNotNullCondition() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.whereIsNotNull("test.col");
        assertEquals(query.toString(), "WHERE ( test.col IS NOT NULL )");
    }

    /**
     * Test method for {@link QueryBuilder1#or(QueryBuilder1)} .
     */
    @Ignore
    public void testOrQueryBuilderOfC() {
        QueryBuilder1<Zone, Integer> queryZone = new QueryBuilder1<>(Zone.class);
        QueryBuilder1<Project, Integer> queryProject = new QueryBuilder1<>(Project.class);

        queryProject.select("entity");

        queryZone.or(queryProject);
        assertEquals(queryZone.toString(), "OR ( SELECT entity )");
    }

    /**
     * Test method for {@link QueryBuilder1#orIsNull(java.lang.String)} .
     */
    @Ignore
    public void testOrIsNullCondition() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.orIsNull("test.col");
        assertEquals(query.toString(), "OR ( test.col IS NULL )");
    }

    /**
     * Test method for {@link QueryBuilder1#orIsNotNull(java.lang.String)} .
     */
    @Ignore
    public void testOrIsNotNullCondition() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.orIsNotNull("test.col");
        assertEquals(query.toString(), "OR ( test.col IS NOT NULL )");
    }

    /**
     * Test method for {@link QueryBuilder1#and(QueryBuilder1)} .
     */
    @Ignore
    public void testAndQueryBuilderOfC() {
        QueryBuilder1<Zone, Integer> queryZone = new QueryBuilder1<>(Zone.class);
        QueryBuilder1<Project, Integer> queryProject = new QueryBuilder1<>(Project.class);

        queryProject.select("entity");

        queryZone.and(queryProject);
        assertEquals(queryZone.toString(), "AND ( SELECT entity )");
    }

    /**
     * Test method for {@link QueryBuilder1#andIsNull(java.lang.String)} .
     */
    @Ignore
    public void testAndIsNullCondition() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.andIsNull("test.col");
        assertEquals(query.toString(), "AND ( test.col IS NULL )");
    }

    /**
     * Test method for {@link QueryBuilder1#andIsNotNull(java.lang.String)} .
     */
    @Ignore
    public void testAndIsNotNullCondition() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.andIsNotNull("test.col");
        assertEquals(query.toString(), "AND ( test.col IS NOT NULL )");
    }

    /**
     * Test method for
     * {@link QueryBuilder1#join(java.lang.String, java.lang.String)} .
     */
    @Test
    public void testJoin() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.join("zone.projects", "projects");
        assertEquals(query.toString(), "JOIN zone.projects AS projects");
    }

    /**
     * Test method for
     * {@link QueryBuilder1#leftJoin(java.lang.String, java.lang.String)} .
     */
    @Test
    public void testLeftJoin() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.leftJoin("zone.projects", "projects");
        assertEquals(query.toString(), "LEFT JOIN zone.projects AS projects");
    }

    /**
     * Test method for
     * {@link QueryBuilder1#innerJoin(java.lang.String, java.lang.String)} .
     */
    @Test
    public void testInnerJoin() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.innerJoin("zone.projects", "projects");
        assertEquals(query.toString(), "INNER JOIN zone.projects AS projects");
    }

    /**
     * Test method for
     * {@link QueryBuilder1#rightJoin(java.lang.String, java.lang.String)} .
     */
    @Test
    public void testRightJoin() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.rightJoin("zone.projects", "projects");
        assertEquals(query.toString(), "RIGHT JOIN zone.projects AS projects");
    }

    /**
     * Test method for
     * {@link QueryBuilder1#leftOuterJoin(java.lang.String, java.lang.String)} .
     */
    @Test
    public void testLeftOuterJoin() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.leftOuterJoin("zone.projects", "projects");
        assertEquals(query.toString(), "LEFT OUTER JOIN zone.projects AS projects");
    }

    /**
     * Test method for
     * {@link QueryBuilder1#rightOuterJoin(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public void testRightOuterJoin() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.rightOuterJoin("zone.projects", "projects");
        assertEquals(query.toString(), "RIGHT OUTER JOIN zone.projects AS projects");
    }

    /**
     * Test method for {@link QueryBuilder1#update()}.
     */
    @Test
    public void testUpdate() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.update();
        assertEquals(query.toString(), "UPDATE " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder1<>(Zone.class);
        query.update();
        assertTrue(query.toString().matches("UPDATE " + Zone.class.getCanonicalName() + " AS zone[\\da-f]+"));
    }

    /**
     * Test method for {@link QueryBuilder1#update(java.lang.String)} .
     */
    @Test
    public void testUpdateString() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.update("zone");
        assertEquals(query.toString(), "UPDATE " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder1<>(Zone.class);
        query.update("zone");
        assertEquals(query.toString(), "UPDATE " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder1<>(Zone.class);
        query.update(null);
        assertTrue(query.toString().matches("UPDATE " + Zone.class.getCanonicalName() + " AS zone[\\da-f]+"));

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.update("z");
        assertEquals(query.toString(), "UPDATE " + Zone.class.getCanonicalName() + " AS z");
    }

    /**
     * Test method for {@link QueryBuilder1#groupBy(java.lang.String)} .
     */
    @Test
    public void testGroupBy() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.groupBy("zone");
        assertEquals(query.toString(), "GROUP BY zone");

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.groupBy();
        assertEquals(query.toString(), "GROUP BY zone");

        query = new QueryBuilder1<>(Zone.class);
        query.groupBy();
        assertTrue(query.toString().matches("GROUP BY zone[\\da-f]+"));

        query = new QueryBuilder1<>(Zone.class);
        query.groupBy("zone");
        assertEquals(query.toString(), "GROUP BY zone");
    }

    /**
     * Test method for {@link QueryBuilder1#groupBy(java.lang.String)} .
     */
    @Test
    public void testOrderBy() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.orderBy(QueryOrder.asc("zone.name"));
        assertEquals(query.toString(), "ORDER BY zone.name ASC");

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.orderBy(QueryOrder.desc("zone.name"));
        assertEquals(query.toString(), "ORDER BY zone.name DESC");

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.orderBy(QueryOrder.desc("zone.codeInsee"), QueryOrder.asc("zone.name"));
        assertEquals(query.toString(), "ORDER BY zone.codeInsee DESC, zone.name ASC");
    }

    /**
     * Test method for {@link QueryBuilder1#limit(java.lang.Integer)} .
     */
    @Test
    public void testLimit() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.limit(2);
        assertEquals(query.toString(), "LIMIT 2");

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.limit(0);
        assertEquals(query.toString(), "");

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.limit(null);
        assertEquals(query.toString(), "");
    }

    /**
     * Test method for {@link QueryBuilder1#offset(java.lang.Integer)} .
     */
    @Test
    public void testOffset() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.offset(2);
        assertEquals(query.toString(), "OFFSET 2");

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.offset(0);
        assertEquals(query.toString(), "");

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.offset(null);
        assertEquals(query.toString(), "");
    }

    /**
     * Test method for
     * {@link QueryBuilder1#set(java.lang.String, java.lang.String)} .
     */
    @Test
    public void testSetStringString() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.set("zone.codeInsee", "param");
        assertEquals(query.toString(), "SET zone.codeInsee = :param");

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.set("zone.codeInsee", "codeInsee");
        query.set("zone.label", "label");
        assertEquals(query.toString(), "SET zone.codeInsee = :codeInsee , zone.label = :label");
    }

    /**
     * Test method for {@link QueryBuilder1#delete()}.
     */
    @Test
    public void testDelete() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.delete();
        assertEquals(query.toString(), "DELETE FROM " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder1<>(Zone.class);
        query.delete();
        assertTrue(query.toString().matches("DELETE FROM " + Zone.class.getCanonicalName() + " AS zone[\\da-f]+"));
    }

    /**
     * Test method for {@link QueryBuilder1#delete(java.lang.String)} .
     */
    @Test
    public void testDeleteString() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.delete("zone");
        assertEquals(query.toString(), "DELETE FROM " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder1<>(Zone.class);
        query.delete("zone");
        assertEquals(query.toString(), "DELETE FROM " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder1<>(Zone.class);
        query.delete(null);
        assertTrue(query.toString().matches("DELETE FROM " + Zone.class.getCanonicalName() + " AS zone[\\da-f]+"));

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.delete("z");
        assertEquals(query.toString(), "DELETE FROM " + Zone.class.getCanonicalName() + " AS z");
    }

    /**
     * Test method for {@link QueryBuilder1#selectCount(java.lang.String)} .
     */
    @Test
    public void testSelectCount() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.selectCount("zone.codeInsee");
        assertEquals(query.toString(), "SELECT COUNT ( zone.codeInsee )");

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.selectCount("");
        assertEquals(query.toString(), "SELECT COUNT ( * )");

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.selectCount(null);
        assertEquals(query.toString(), "SELECT COUNT ( * )");
    }

    /**
     * Test method for {@link QueryBuilder1#append(java.lang.String)} .
     */
    @Test
    public void testAppendString() {
        QueryBuilder1<Zone, Integer> query = new QueryBuilder1<>(Zone.class, "zone");
        query.append("test");
        assertEquals(query.toString(), "test");

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.append("");
        assertTrue(StringUtils.isEmpty(query.toString()));

        query = new QueryBuilder1<>(Zone.class, "zone");
        query.append((String) null);
        assertTrue(StringUtils.isEmpty(query.toString()));
    }

    /**
     * Test method for {@link QueryBuilder1#append(QueryBuilder1)} .
     */
    @Test
    public void testAppendQueryBuilderOfC() {
        QueryBuilder1<Project, Integer> queryProject = new QueryBuilder1<>(Project.class);
        queryProject.select("entity");

        QueryBuilder1<Zone, Integer> queryZone = new QueryBuilder1<>(Zone.class);
        queryZone.select("zone");
        queryZone.append(queryProject);
        assertEquals(queryZone.toString(), "SELECT zone ( SELECT entity )");

        queryZone = new QueryBuilder1<>(Zone.class, "zone");
        queryZone.select("zone");
        queryZone.append((QueryBuilder1<Project, Integer>) null);
        assertEquals(queryZone.toString(), "SELECT zone");
    }
}
