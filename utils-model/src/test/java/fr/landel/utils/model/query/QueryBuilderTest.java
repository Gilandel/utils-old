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
import static org.junit.Assert.assertTrue;

import org.apache.commons.lang3.StringUtils;
import org.junit.Test;

/**
 * Check query builder.
 *
 * @since 30 nov. 2015
 * @author Gilles
 *
 */
public class QueryBuilderTest {

    /**
     * Constructor
     */
    public QueryBuilderTest() {
    }

    /**
     * Test method for {@link QueryBuilder#getAlias()}.
     */
    @Test
    public void testGetAlias() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        assertEquals(query.getAlias(), "zone");

        query = new QueryBuilder<>(Zone.class);
        assertTrue(query.getAlias().matches("zone\\d+"));

        query = new QueryBuilder<>(Zone.class, null);
        assertTrue(query.getAlias().matches("zone\\d+"));
    }

    /**
     * Test method for {@link QueryBuilder#select(java.lang.String)} .
     */
    @Test
    public void testSelectString() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.select("entity");
        assertEquals(query.toString(), "SELECT entity");

        query = new QueryBuilder<>(Zone.class, "zone");
        query.select((String) null);
        assertEquals(query.toString(), "SELECT");

        query = new QueryBuilder<>(Zone.class, "zone");
        query.select("");
        assertEquals(query.toString(), "SELECT");
    }

    /**
     * Test method for {@link QueryBuilder#select(QueryBuilder)} .
     */
    @Test
    public void testSelectQueryBuilderOfC() {
        QueryBuilder<Zone, Integer> queryZone = new QueryBuilder<>(Zone.class);
        QueryBuilder<Project, Integer> queryProject = new QueryBuilder<>(Project.class);

        queryProject.select("entity");
        queryZone.select(queryProject);

        assertEquals(queryZone.toString(), "SELECT ( SELECT entity )");

    }

    /**
     * Test method for {@link QueryBuilder#selectEntity()}.
     */
    @Test
    public void testSelectEntity() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class);
        query.selectEntity();
        assertTrue(query.toString().matches("SELECT zone\\d+"));

        query = new QueryBuilder<>(Zone.class, "zone");
        query.selectEntity();
        assertEquals(query.toString(), "SELECT zone");
    }

    /**
     * Test method for {@link QueryBuilder#selectEntity(java.lang.String)} .
     */
    @Test
    public void testSelectEntityString() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.selectEntity("entity");
        assertEquals(query.toString(), "SELECT entity");

        query = new QueryBuilder<>(Zone.class, "zone");
        query.selectEntity(null);
        assertEquals(query.toString(), "SELECT zone");

        query = new QueryBuilder<>(Zone.class, null);
        query.selectEntity(null);
        assertTrue(query.toString().matches("SELECT zone\\d+"));
    }

    /**
     * Test method for {@link QueryBuilder#from()}.
     */
    @Test
    public void testFrom() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.from();
        assertEquals(query.toString(), "SELECT zone FROM " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder<>(Zone.class);
        query.from();
        assertTrue(query.toString().matches("SELECT zone\\d+ FROM " + Zone.class.getCanonicalName() + " AS zone\\d+"));
    }

    /**
     * Test method for {@link QueryBuilder#from(java.lang.String)} .
     */
    @Test
    public void testFromString() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.from("zone");
        assertEquals(query.toString(), "SELECT zone FROM " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder<>(Zone.class);
        query.from("zone");
        assertEquals(query.toString(), "SELECT zone FROM " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder<>(Zone.class);
        query.from(null);
        assertTrue(query.toString().matches("SELECT zone\\d+ FROM " + Zone.class.getCanonicalName() + " AS zone\\d+"));

        query = new QueryBuilder<>(Zone.class, "zone");
        query.from("z");
        assertEquals(query.toString(), "SELECT z FROM " + Zone.class.getCanonicalName() + " AS z");
    }

    /**
     * Test method for
     * {@link QueryBuilder#from(java.lang.Class, java.lang.String)} .
     */
    @Test
    public void testFromEntityString() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.from(Project.class, "project");
        assertEquals(query.toString(), "FROM " + Project.class.getCanonicalName() + " AS project");

        query = new QueryBuilder<>(Zone.class);
        query.from(Project.class, "");
        assertTrue(query.toString().matches("FROM " + Project.class.getCanonicalName() + " AS project\\d+"));

        query = new QueryBuilder<>(Zone.class);
        query.from(Project.class, null);
        assertTrue(query.toString().matches("FROM " + Project.class.getCanonicalName() + " AS project\\d+"));
    }

    /**
     * Test method for {@link QueryBuilder#add(QueryCondition)} .
     */
    @Test
    public void testAddCondition() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.add(new QueryCondition("zone"));
        assertEquals(query.toString(), "( zone )");
    }

    /**
     * Test method for {@link QueryBuilder#where(QueryCondition)} .
     */
    @Test
    public void testWhereQueryCondition() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.where(new QueryCondition("zone"));
        assertEquals(query.toString(), "WHERE ( zone )");
    }

    /**
     * Test method for {@link QueryBuilder#where(QueryBuilder)} .
     */
    @Test
    public void testWhereQueryBuilderOfC() {
        QueryBuilder<Zone, Integer> queryZone = new QueryBuilder<>(Zone.class);
        QueryBuilder<Project, Integer> queryProject = new QueryBuilder<>(Project.class);

        queryProject.select("entity");

        queryZone.where(queryProject);
        assertEquals(queryZone.toString(), "WHERE ( SELECT entity )");
    }

    /**
     * Test method for {@link QueryBuilder#whereIsNull(java.lang.String)} .
     */
    @Test
    public void testWhereIsNullCondition() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.whereIsNull("test.col");
        assertEquals(query.toString(), "WHERE ( test.col IS NULL )");
    }

    /**
     * Test method for {@link QueryBuilder#whereIsNotNull(java.lang.String)} .
     */
    @Test
    public void testWhereIsNotNullCondition() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.whereIsNotNull("test.col");
        assertEquals(query.toString(), "WHERE ( test.col IS NOT NULL )");
    }

    /**
     * Test method for {@link QueryBuilder#or(QueryCondition)} .
     */
    @Test
    public void testOrQueryCondition() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.or(new QueryCondition("zone"));
        assertEquals(query.toString(), "OR ( zone )");
    }

    /**
     * Test method for {@link QueryBuilder#or(QueryBuilder)} .
     */
    @Test
    public void testOrQueryBuilderOfC() {
        QueryBuilder<Zone, Integer> queryZone = new QueryBuilder<>(Zone.class);
        QueryBuilder<Project, Integer> queryProject = new QueryBuilder<>(Project.class);

        queryProject.select("entity");

        queryZone.or(queryProject);
        assertEquals(queryZone.toString(), "OR ( SELECT entity )");
    }

    /**
     * Test method for {@link QueryBuilder#orIsNull(java.lang.String)} .
     */
    @Test
    public void testOrIsNullCondition() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.orIsNull("test.col");
        assertEquals(query.toString(), "OR ( test.col IS NULL )");
    }

    /**
     * Test method for {@link QueryBuilder#orIsNotNull(java.lang.String)} .
     */
    @Test
    public void testOrIsNotNullCondition() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.orIsNotNull("test.col");
        assertEquals(query.toString(), "OR ( test.col IS NOT NULL )");
    }

    /**
     * Test method for {@link QueryBuilder#and(QueryCondition)} .
     */
    @Test
    public void testAndQueryCondition() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.and(new QueryCondition("zone"));
        assertEquals(query.toString(), "AND ( zone )");
    }

    /**
     * Test method for {@link QueryBuilder#and(QueryBuilder)} .
     */
    @Test
    public void testAndQueryBuilderOfC() {
        QueryBuilder<Zone, Integer> queryZone = new QueryBuilder<>(Zone.class);
        QueryBuilder<Project, Integer> queryProject = new QueryBuilder<>(Project.class);

        queryProject.select("entity");

        queryZone.and(queryProject);
        assertEquals(queryZone.toString(), "AND ( SELECT entity )");
    }

    /**
     * Test method for {@link QueryBuilder#andIsNull(java.lang.String)} .
     */
    @Test
    public void testAndIsNullCondition() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.andIsNull("test.col");
        assertEquals(query.toString(), "AND ( test.col IS NULL )");
    }

    /**
     * Test method for {@link QueryBuilder#andIsNotNull(java.lang.String)} .
     */
    @Test
    public void testAndIsNotNullCondition() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.andIsNotNull("test.col");
        assertEquals(query.toString(), "AND ( test.col IS NOT NULL )");
    }

    /**
     * Test method for
     * {@link QueryBuilder#join(java.lang.String, java.lang.String)} .
     */
    @Test
    public void testJoin() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.join("zone.projects", "projects");
        assertEquals(query.toString(), "JOIN zone.projects AS projects");
    }

    /**
     * Test method for
     * {@link QueryBuilder#leftJoin(java.lang.String, java.lang.String)} .
     */
    @Test
    public void testLeftJoin() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.leftJoin("zone.projects", "projects");
        assertEquals(query.toString(), "LEFT JOIN zone.projects AS projects");
    }

    /**
     * Test method for
     * {@link QueryBuilder#innerJoin(java.lang.String, java.lang.String)} .
     */
    @Test
    public void testInnerJoin() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.innerJoin("zone.projects", "projects");
        assertEquals(query.toString(), "INNER JOIN zone.projects AS projects");
    }

    /**
     * Test method for
     * {@link QueryBuilder#rightJoin(java.lang.String, java.lang.String)} .
     */
    @Test
    public void testRightJoin() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.rightJoin("zone.projects", "projects");
        assertEquals(query.toString(), "RIGHT JOIN zone.projects AS projects");
    }

    /**
     * Test method for
     * {@link QueryBuilder#leftOuterJoin(java.lang.String, java.lang.String)} .
     */
    @Test
    public void testLeftOuterJoin() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.leftOuterJoin("zone.projects", "projects");
        assertEquals(query.toString(), "LEFT OUTER JOIN zone.projects AS projects");
    }

    /**
     * Test method for
     * {@link QueryBuilder#rightOuterJoin(java.lang.String, java.lang.String)} .
     */
    @Test
    public void testRightOuterJoin() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.rightOuterJoin("zone.projects", "projects");
        assertEquals(query.toString(), "RIGHT OUTER JOIN zone.projects AS projects");
    }

    /**
     * Test method for {@link QueryBuilder#update()}.
     */
    @Test
    public void testUpdate() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.update();
        assertEquals(query.toString(), "UPDATE " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder<>(Zone.class);
        query.update();
        assertTrue(query.toString().matches("UPDATE " + Zone.class.getCanonicalName() + " AS zone\\d+"));
    }

    /**
     * Test method for {@link QueryBuilder#update(java.lang.String)} .
     */
    @Test
    public void testUpdateString() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.update("zone");
        assertEquals(query.toString(), "UPDATE " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder<>(Zone.class);
        query.update("zone");
        assertEquals(query.toString(), "UPDATE " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder<>(Zone.class);
        query.update(null);
        assertTrue(query.toString().matches("UPDATE " + Zone.class.getCanonicalName() + " AS zone\\d+"));

        query = new QueryBuilder<>(Zone.class, "zone");
        query.update("z");
        assertEquals(query.toString(), "UPDATE " + Zone.class.getCanonicalName() + " AS z");
    }

    /**
     * Test method for {@link QueryBuilder#groupBy(java.lang.String)} .
     */
    @Test
    public void testGroupBy() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.groupBy("zone");
        assertEquals(query.toString(), "GROUP BY zone");

        query = new QueryBuilder<>(Zone.class, "zone");
        query.groupBy();
        assertEquals(query.toString(), "GROUP BY zone");

        query = new QueryBuilder<>(Zone.class);
        query.groupBy();
        assertTrue(query.toString().matches("GROUP BY zone\\d+"));

        query = new QueryBuilder<>(Zone.class);
        query.groupBy("zone");
        assertEquals(query.toString(), "GROUP BY zone");
    }

    /**
     * Test method for {@link QueryBuilder#groupBy(java.lang.String)} .
     */
    @Test
    public void testOrderBy() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.orderBy(QueryOrder.asc("zone.name"));
        assertEquals(query.toString(), "ORDER BY zone.name ASC");

        query = new QueryBuilder<>(Zone.class, "zone");
        query.orderBy(QueryOrder.desc("zone.name"));
        assertEquals(query.toString(), "ORDER BY zone.name DESC");

        query = new QueryBuilder<>(Zone.class, "zone");
        query.orderBy(QueryOrder.desc("zone.codeInsee"), QueryOrder.asc("zone.name"));
        assertEquals(query.toString(), "ORDER BY zone.codeInsee DESC, zone.name ASC");
    }

    /**
     * Test method for {@link QueryBuilder#limit(java.lang.Integer)} .
     */
    @Test
    public void testLimit() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.limit(2);
        assertEquals(query.toString(), "LIMIT 2");

        query = new QueryBuilder<>(Zone.class, "zone");
        query.limit(0);
        assertEquals(query.toString(), "");

        query = new QueryBuilder<>(Zone.class, "zone");
        query.limit(null);
        assertEquals(query.toString(), "");
    }

    /**
     * Test method for {@link QueryBuilder#offset(java.lang.Integer)} .
     */
    @Test
    public void testOffset() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.offset(2);
        assertEquals(query.toString(), "OFFSET 2");

        query = new QueryBuilder<>(Zone.class, "zone");
        query.offset(0);
        assertEquals(query.toString(), "");

        query = new QueryBuilder<>(Zone.class, "zone");
        query.offset(null);
        assertEquals(query.toString(), "");
    }

    /**
     * Test method for
     * {@link QueryBuilder#set(java.lang.String, java.lang.String)} .
     */
    @Test
    public void testSetStringString() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.set("zone.codeInsee", "param");
        assertEquals(query.toString(), "SET zone.codeInsee = :param");

        query = new QueryBuilder<>(Zone.class, "zone");
        query.set("zone.codeInsee", "codeInsee");
        query.set("zone.label", "label");
        assertEquals(query.toString(), "SET zone.codeInsee = :codeInsee , zone.label = :label");
    }

    /**
     * Test method for {@link QueryBuilder#delete()}.
     */
    @Test
    public void testDelete() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.delete();
        assertEquals(query.toString(), "DELETE FROM " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder<>(Zone.class);
        query.delete();
        assertTrue(query.toString().matches("DELETE FROM " + Zone.class.getCanonicalName() + " AS zone\\d+"));
    }

    /**
     * Test method for {@link QueryBuilder#delete(java.lang.String)} .
     */
    @Test
    public void testDeleteString() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.delete("zone");
        assertEquals(query.toString(), "DELETE FROM " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder<>(Zone.class);
        query.delete("zone");
        assertEquals(query.toString(), "DELETE FROM " + Zone.class.getCanonicalName() + " AS zone");

        query = new QueryBuilder<>(Zone.class);
        query.delete(null);
        assertTrue(query.toString().matches("DELETE FROM " + Zone.class.getCanonicalName() + " AS zone\\d+"));

        query = new QueryBuilder<>(Zone.class, "zone");
        query.delete("z");
        assertEquals(query.toString(), "DELETE FROM " + Zone.class.getCanonicalName() + " AS z");
    }

    /**
     * Test method for {@link QueryBuilder#selectCount(java.lang.String)} .
     */
    @Test
    public void testSelectCount() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.selectCount("zone.codeInsee");
        assertEquals(query.toString(), "SELECT COUNT ( zone.codeInsee )");

        query = new QueryBuilder<>(Zone.class, "zone");
        query.selectCount("");
        assertEquals(query.toString(), "SELECT COUNT ( * )");

        query = new QueryBuilder<>(Zone.class, "zone");
        query.selectCount(null);
        assertEquals(query.toString(), "SELECT COUNT ( * )");
    }

    /**
     * Test method for {@link QueryBuilder#append(java.lang.String)} .
     */
    @Test
    public void testAppendString() {
        QueryBuilder<Zone, Integer> query = new QueryBuilder<>(Zone.class, "zone");
        query.append("test");
        assertEquals(query.toString(), "test");

        query = new QueryBuilder<>(Zone.class, "zone");
        query.append("");
        assertTrue(StringUtils.isEmpty(query.toString()));

        query = new QueryBuilder<>(Zone.class, "zone");
        query.append((String) null);
        assertTrue(StringUtils.isEmpty(query.toString()));
    }

    /**
     * Test method for {@link QueryBuilder#append(QueryBuilder)} .
     */
    @Test
    public void testAppendQueryBuilderOfC() {
        QueryBuilder<Project, Integer> queryProject = new QueryBuilder<>(Project.class);
        queryProject.select("entity");

        QueryBuilder<Zone, Integer> queryZone = new QueryBuilder<>(Zone.class);
        queryZone.select("zone");
        queryZone.append(queryProject);
        assertEquals(queryZone.toString(), "SELECT zone ( SELECT entity )");

        queryZone = new QueryBuilder<>(Zone.class, "zone");
        queryZone.select("zone");
        queryZone.append((QueryBuilder<Project, Integer>) null);
        assertEquals(queryZone.toString(), "SELECT zone");
    }
}
