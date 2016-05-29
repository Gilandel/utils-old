/*
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.awt.Point;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import org.apache.commons.collections4.Transformer;
import org.hamcrest.Matchers;
import org.junit.Assert;
import org.junit.Test;

/**
 * Check collections utils
 *
 * @since 2 févr. 2016
 * @author Gilles Landel
 *
 */
public class CollectionUtils2Test {

    private static final Transformer<Point, String> TRANSFORMER = new Transformer<Point, String>() {
        @Override
        public String transform(Point input) {
            if (input != null) {
                return ((int) input.getX()) + ", " + ((int) input.getY());
            }
            return null;
        }
    };

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoList(java.lang.Iterable, org.apache.commons.collections4.Transformer)}
     * .
     */
    @Test
    public void testTransformIntoListIterableOfITransformerOfIO() {
        try {
            List<Point> points = new ArrayList<>();
            points.add(new Point(1, 2));
            points.add(new Point(2, 0));
            points.add(null);

            List<String> strPoints = CollectionUtils2.transformIntoList(points, TRANSFORMER);

            Assert.assertThat(strPoints, Matchers.contains("1, 2", "2, 0", null));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoSet(java.lang.Iterable, org.apache.commons.collections4.Transformer)}
     * .
     */
    @Test
    public void testTransformIntoSetIterableOfITransformerOfIO() {
        try {
            List<Point> points = new ArrayList<>();
            points.add(new Point(1, 2));
            points.add(new Point(2, 0));
            points.add(null);

            Set<String> strPoints = CollectionUtils2.transformIntoSet(points, TRANSFORMER);

            Assert.assertThat(strPoints, Matchers.containsInAnyOrder("1, 2", "2, 0", null));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoList(java.lang.Iterable)}.
     */
    @Test
    public void testTransformIntoListIterableOfI() {
        try {
            List<Point> points = new ArrayList<>();
            points.add(new Point(1, 2));
            points.add(new Point(2, 0));
            points.add(null);

            List<String> strPoints = CollectionUtils2.transformIntoList(points);

            Assert.assertThat(strPoints, Matchers.contains("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", null));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoList(java.lang.Iterable, boolean)}.
     */
    @Test
    public void testTransformIntoListIterableOfIB() {
        try {
            List<Point> points = new ArrayList<>();
            points.add(new Point(1, 2));
            points.add(new Point(2, 0));
            points.add(null);

            List<String> strPoints = CollectionUtils2.transformIntoList(points, true);

            Assert.assertThat(strPoints, Matchers.contains("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", "null"));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoSet(java.lang.Iterable)}.
     */
    @Test
    public void testTransformIntoSetIterableOfI() {
        try {
            List<Point> points = new ArrayList<>();
            points.add(new Point(1, 2));
            points.add(new Point(2, 0));
            points.add(null);

            Set<String> strPoints = CollectionUtils2.transformIntoSet(points);

            Assert.assertThat(strPoints, Matchers.containsInAnyOrder("java.awt.Point[x=2,y=0]", "java.awt.Point[x=1,y=2]", null));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoSet(java.lang.Iterable, boolean)}.
     */
    @Test
    public void testTransformIntoSetIterableOfIB() {
        try {
            List<Point> points = new ArrayList<>();
            points.add(new Point(1, 2));
            points.add(new Point(2, 0));
            points.add(null);

            Set<String> strPoints = CollectionUtils2.transformIntoSet(points, true);

            Assert.assertThat(strPoints, Matchers.containsInAnyOrder("java.awt.Point[x=2,y=0]", "java.awt.Point[x=1,y=2]", "null"));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link CollectionUtils2#toArray(java.util.Collection)}.
     */
    @Test
    public void testToArray() {
        try {
            List<Point> points = new ArrayList<>();
            points.add(new Point(1, 2));
            points.add(new Point(2, 0));
            points.add(null);

            Point[] pointsArray = CollectionUtils2.toArray(points);

            Assert.assertNotNull(pointsArray);
            Assert.assertTrue(pointsArray.length > 0);
            Assert.assertThat(pointsArray, Matchers.arrayContaining(points.get(0), points.get(1), null));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoArray(I[], org.apache.commons.collections4.Transformer)}
     * .
     */
    @Test
    public void testTransformIntoArrayIArrayTransformerOfIO() {
        try {
            Point[] points = new Point[3];
            points[0] = new Point(1, 2);
            points[1] = new Point(2, 0);

            String[] pointsArray = CollectionUtils2.transformIntoArray(points, TRANSFORMER);

            Assert.assertNotNull(pointsArray);
            Assert.assertTrue(pointsArray.length > 0);
            Assert.assertThat(pointsArray, Matchers.arrayContaining("1, 2", "2, 0", null));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link CollectionUtils2#transformIntoArray(I[])}.
     */
    @Test
    public void testTransformIntoArrayIArray() {
        try {
            Point[] points = new Point[3];
            points[0] = new Point(1, 2);
            points[1] = new Point(2, 0);

            String[] pointsArray = CollectionUtils2.transformIntoArray(points);

            Assert.assertNotNull(pointsArray);
            Assert.assertTrue(pointsArray.length > 0);
            Assert.assertThat(pointsArray, Matchers.arrayContaining("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", null));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link CollectionUtils2#transformIntoArray(I[], boolean)}
     * .
     */
    @Test
    public void testTransformIntoArrayIArrayB() {
        try {
            Point[] points = new Point[3];
            points[0] = new Point(1, 2);
            points[1] = new Point(2, 0);

            String[] pointsArray = CollectionUtils2.transformIntoArray(points, true);

            Assert.assertNotNull(pointsArray);
            Assert.assertTrue(pointsArray.length > 0);
            Assert.assertThat(pointsArray, Matchers.arrayContaining("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", "null"));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoArray(java.lang.Iterable, org.apache.commons.collections4.Transformer)}
     * .
     */
    @Test
    public void testTransformIntoArrayIterableOfITransformerOfIO() {
        try {
            List<Point> points = new ArrayList<>();
            points.add(new Point(1, 2));
            points.add(new Point(2, 0));
            points.add(null);

            String[] pointsArray = CollectionUtils2.transformIntoArray(points, TRANSFORMER);

            Assert.assertNotNull(pointsArray);
            Assert.assertTrue(pointsArray.length > 0);
            Assert.assertThat(pointsArray, Matchers.arrayContaining("1, 2", "2, 0", null));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoArray(java.lang.Iterable)}.
     */
    @Test
    public void testTransformIntoArrayIterableOfI() {
        try {
            List<Point> points = new ArrayList<>();
            points.add(new Point(1, 2));
            points.add(new Point(2, 0));
            points.add(null);

            String[] pointsArray = CollectionUtils2.transformIntoArray(points);

            Assert.assertNotNull(pointsArray);
            Assert.assertTrue(pointsArray.length > 0);
            Assert.assertThat(pointsArray, Matchers.arrayContaining("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", null));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoArray(java.lang.Iterable, boolean)}.
     */
    @Test
    public void testTransformIntoArrayIterableOfIB() {
        try {
            List<Point> points = new ArrayList<>();
            points.add(new Point(1, 2));
            points.add(new Point(2, 0));
            points.add(null);

            String[] pointsArray = CollectionUtils2.transformIntoArray(points, true);

            Assert.assertNotNull(pointsArray);
            Assert.assertTrue(pointsArray.length > 0);
            Assert.assertThat(pointsArray, Matchers.arrayContaining("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", "null"));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoList(I[], org.apache.commons.collections4.Transformer)}
     * .
     */
    @Test
    public void testTransformIntoListIArrayTransformerOfIO() {
        try {
            Point[] points = new Point[3];
            points[0] = new Point(1, 2);
            points[1] = new Point(2, 0);

            List<String> pointsList = CollectionUtils2.transformIntoList(points, TRANSFORMER);

            Assert.assertNotNull(pointsList);
            Assert.assertTrue(pointsList.size() > 0);
            Assert.assertThat(pointsList, Matchers.contains("1, 2", "2, 0", null));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link CollectionUtils2#transformIntoList(I[])}.
     */
    @Test
    public void testTransformIntoListIArray() {
        try {
            Point[] points = new Point[3];
            points[0] = new Point(1, 2);
            points[1] = new Point(2, 0);

            List<String> pointsList = CollectionUtils2.transformIntoList(points);

            Assert.assertNotNull(pointsList);
            Assert.assertTrue(pointsList.size() > 0);
            Assert.assertThat(pointsList, Matchers.contains("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", null));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link CollectionUtils2#transformIntoList(I[], boolean)}.
     */
    @Test
    public void testTransformIntoListIArrayB() {
        try {
            Point[] points = new Point[3];
            points[0] = new Point(1, 2);
            points[1] = new Point(2, 0);

            List<String> pointsList = CollectionUtils2.transformIntoList(points, true);

            Assert.assertNotNull(pointsList);
            Assert.assertTrue(pointsList.size() > 0);
            Assert.assertThat(pointsList, Matchers.contains("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", "null"));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoSet(I[], org.apache.commons.collections4.Transformer)}
     * .
     */
    @Test
    public void testTransformIntoSetIArrayTransformerOfIO() {
        try {
            Point[] points = new Point[3];
            points[0] = new Point(1, 2);
            points[1] = new Point(2, 0);

            Set<String> pointsList = CollectionUtils2.transformIntoSet(points, TRANSFORMER);

            Assert.assertNotNull(pointsList);
            Assert.assertTrue(pointsList.size() > 0);
            Assert.assertThat(pointsList, Matchers.containsInAnyOrder("1, 2", "2, 0", null));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link CollectionUtils2#transformIntoSet(I[])}.
     */
    @Test
    public void testTransformIntoSetIArray() {
        try {
            Point[] points = new Point[3];
            points[0] = new Point(1, 2);
            points[1] = new Point(2, 0);

            Set<String> pointsList = CollectionUtils2.transformIntoSet(points);

            Assert.assertNotNull(pointsList);
            Assert.assertTrue(pointsList.size() > 0);
            Assert.assertThat(pointsList, Matchers.containsInAnyOrder("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", null));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link CollectionUtils2#transformIntoSet(I[], boolean)}.
     */
    @Test
    public void testTransformIntoSetIArrayB() {
        try {
            Point[] points = new Point[3];
            points[0] = new Point(1, 2);
            points[1] = new Point(2, 0);

            Set<String> pointsList = CollectionUtils2.transformIntoSet(points, true);

            Assert.assertNotNull(pointsList);
            Assert.assertTrue(pointsList.size() > 0);
            Assert.assertThat(pointsList, Matchers.containsInAnyOrder("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", "null"));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link CollectionUtils2#getListClass}
     * {@link CollectionUtils2#getSetClass}
     * {@link CollectionUtils2#getQueueClass}
     * {@link CollectionUtils2#getMapClass}.
     */
    @Test
    public void testGetTypedClass() {
        Class<List<String>> clazz = CollectionUtils2.getListClass(String.class);
        assertNotNull(clazz);
        assertTrue(ArrayList.class.isAssignableFrom(clazz));

        Class<List<Object>> clazz2 = CollectionUtils2.getListClass(null);
        assertNotNull(clazz2);
        assertTrue(ArrayList.class.isAssignableFrom(clazz));

        Class<Set<String>> clazz3 = CollectionUtils2.getSetClass(String.class);
        assertNotNull(clazz3);
        assertTrue(HashSet.class.isAssignableFrom(clazz3));

        Class<Set<Object>> clazz4 = CollectionUtils2.getSetClass(null);
        assertNotNull(clazz4);
        assertTrue(HashSet.class.isAssignableFrom(clazz4));

        Class<Queue<String>> clazz5 = CollectionUtils2.getQueueClass(String.class);
        assertNotNull(clazz5);
        assertTrue(LinkedList.class.isAssignableFrom(clazz5));

        Class<Queue<Object>> clazz6 = CollectionUtils2.getQueueClass(null);
        assertNotNull(clazz6);
        assertTrue(LinkedList.class.isAssignableFrom(clazz6));

        Class<Map<String, Integer>> clazz7 = CollectionUtils2.getMapClass(String.class, Integer.class);
        assertNotNull(clazz7);
        assertTrue(HashMap.class.isAssignableFrom(clazz7));

        Class<Map<Object, Integer>> clazz8 = CollectionUtils2.getMapClass(null, Integer.class);
        assertNotNull(clazz8);
        assertTrue(HashMap.class.isAssignableFrom(clazz8));

        Class<Map<String, Object>> clazz9 = CollectionUtils2.getMapClass(String.class, null);
        assertNotNull(clazz9);
        assertTrue(HashMap.class.isAssignableFrom(clazz9));
    }
}