/*
 * #%L
 * gl-utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package org.gl.utils.commons;

import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.apache.commons.collections4.Transformer;
import org.hamcrest.Matchers;
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
     * 
     * Constructor
     *
     */
    public CollectionUtils2Test() {
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoList(java.lang.Iterable, org.apache.commons.collections4.Transformer)}
     * .
     */
    @Test
    public void testTransformIntoListIterableOfITransformerOfIO() {
        List<Point> points = new ArrayList<>();
        points.add(new Point(1, 2));
        points.add(new Point(2, 0));
        points.add(null);

        List<String> strPoints = CollectionUtils2.transformIntoList(points, TRANSFORMER);

        Assert.that(strPoints, Matchers.contains("1, 2", "2, 0", null));
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoSet(java.lang.Iterable, org.apache.commons.collections4.Transformer)}
     * .
     */
    @Test
    public void testTransformIntoSetIterableOfITransformerOfIO() {
        List<Point> points = new ArrayList<>();
        points.add(new Point(1, 2));
        points.add(new Point(2, 0));
        points.add(null);

        Set<String> strPoints = CollectionUtils2.transformIntoSet(points, TRANSFORMER);

        Assert.that(strPoints, Matchers.containsInAnyOrder("1, 2", "2, 0", null));
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoList(java.lang.Iterable)}.
     */
    @Test
    public void testTransformIntoListIterableOfI() {
        List<Point> points = new ArrayList<>();
        points.add(new Point(1, 2));
        points.add(new Point(2, 0));
        points.add(null);

        List<String> strPoints = CollectionUtils2.transformIntoList(points);

        Assert.that(strPoints, Matchers.contains("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", null));
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoList(java.lang.Iterable, boolean)}.
     */
    @Test
    public void testTransformIntoListIterableOfIB() {
        List<Point> points = new ArrayList<>();
        points.add(new Point(1, 2));
        points.add(new Point(2, 0));
        points.add(null);

        List<String> strPoints = CollectionUtils2.transformIntoList(points, true);

        Assert.that(strPoints, Matchers.contains("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", "null"));
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoSet(java.lang.Iterable)}.
     */
    @Test
    public void testTransformIntoSetIterableOfI() {
        List<Point> points = new ArrayList<>();
        points.add(new Point(1, 2));
        points.add(new Point(2, 0));
        points.add(null);

        Set<String> strPoints = CollectionUtils2.transformIntoSet(points);

        Assert.that(strPoints, Matchers.containsInAnyOrder("java.awt.Point[x=2,y=0]", "java.awt.Point[x=1,y=2]", null));
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoSet(java.lang.Iterable, boolean)}.
     */
    @Test
    public void testTransformIntoSetIterableOfIB() {
        List<Point> points = new ArrayList<>();
        points.add(new Point(1, 2));
        points.add(new Point(2, 0));
        points.add(null);

        Set<String> strPoints = CollectionUtils2.transformIntoSet(points, true);

        Assert.that(strPoints, Matchers.containsInAnyOrder("java.awt.Point[x=2,y=0]", "java.awt.Point[x=1,y=2]", "null"));
    }

    /**
     * Test method for {@link CollectionUtils2#toArray(java.util.Collection)}.
     */
    @Test
    public void testToArray() {
        List<Point> points = new ArrayList<>();
        points.add(new Point(1, 2));
        points.add(new Point(2, 0));
        points.add(null);

        Point[] pointsArray = CollectionUtils2.toArray(points);

        Assert.isNotEmpty(pointsArray);
        Assert.that(pointsArray, Matchers.arrayContaining(points.get(0), points.get(1), null));
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoArray(I[], org.apache.commons.collections4.Transformer)}
     * .
     */
    @Test
    public void testTransformIntoArrayIArrayTransformerOfIO() {
        Point[] points = new Point[3];
        points[0] = new Point(1, 2);
        points[1] = new Point(2, 0);

        String[] pointsArray = CollectionUtils2.transformIntoArray(points, TRANSFORMER);

        Assert.isNotEmpty(pointsArray);
        Assert.that(pointsArray, Matchers.arrayContaining("1, 2", "2, 0", null));
    }

    /**
     * Test method for {@link CollectionUtils2#transformIntoArray(I[])}.
     */
    @Test
    public void testTransformIntoArrayIArray() {
        Point[] points = new Point[3];
        points[0] = new Point(1, 2);
        points[1] = new Point(2, 0);

        String[] pointsArray = CollectionUtils2.transformIntoArray(points);

        Assert.isNotEmpty(pointsArray);
        Assert.that(pointsArray, Matchers.arrayContaining("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", null));
    }

    /**
     * Test method for {@link CollectionUtils2#transformIntoArray(I[], boolean)}
     * .
     */
    @Test
    public void testTransformIntoArrayIArrayB() {
        Point[] points = new Point[3];
        points[0] = new Point(1, 2);
        points[1] = new Point(2, 0);

        String[] pointsArray = CollectionUtils2.transformIntoArray(points, true);

        Assert.isNotEmpty(pointsArray);
        Assert.that(pointsArray, Matchers.arrayContaining("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", "null"));
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoArray(java.lang.Iterable, org.apache.commons.collections4.Transformer)}
     * .
     */
    @Test
    public void testTransformIntoArrayIterableOfITransformerOfIO() {
        List<Point> points = new ArrayList<>();
        points.add(new Point(1, 2));
        points.add(new Point(2, 0));
        points.add(null);

        String[] pointsArray = CollectionUtils2.transformIntoArray(points, TRANSFORMER);

        Assert.isNotEmpty(pointsArray);
        Assert.that(pointsArray, Matchers.arrayContaining("1, 2", "2, 0", null));
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoArray(java.lang.Iterable)}.
     */
    @Test
    public void testTransformIntoArrayIterableOfI() {
        List<Point> points = new ArrayList<>();
        points.add(new Point(1, 2));
        points.add(new Point(2, 0));
        points.add(null);

        String[] pointsArray = CollectionUtils2.transformIntoArray(points);

        Assert.isNotEmpty(pointsArray);
        Assert.that(pointsArray, Matchers.arrayContaining("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", null));
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoArray(java.lang.Iterable, boolean)}.
     */
    @Test
    public void testTransformIntoArrayIterableOfIB() {
        List<Point> points = new ArrayList<>();
        points.add(new Point(1, 2));
        points.add(new Point(2, 0));
        points.add(null);

        String[] pointsArray = CollectionUtils2.transformIntoArray(points, true);

        Assert.isNotEmpty(pointsArray);
        Assert.that(pointsArray, Matchers.arrayContaining("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", "null"));
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoList(I[], org.apache.commons.collections4.Transformer)}
     * .
     */
    @Test
    public void testTransformIntoListIArrayTransformerOfIO() {
        Point[] points = new Point[3];
        points[0] = new Point(1, 2);
        points[1] = new Point(2, 0);

        List<String> pointsList = CollectionUtils2.transformIntoList(points, TRANSFORMER);

        Assert.isNotEmpty(pointsList);
        Assert.that(pointsList, Matchers.contains("1, 2", "2, 0", null));
    }

    /**
     * Test method for {@link CollectionUtils2#transformIntoList(I[])}.
     */
    @Test
    public void testTransformIntoListIArray() {
        Point[] points = new Point[3];
        points[0] = new Point(1, 2);
        points[1] = new Point(2, 0);

        List<String> pointsList = CollectionUtils2.transformIntoList(points);

        Assert.isNotEmpty(pointsList);
        Assert.that(pointsList, Matchers.contains("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", null));
    }

    /**
     * Test method for {@link CollectionUtils2#transformIntoList(I[], boolean)}.
     */
    @Test
    public void testTransformIntoListIArrayB() {
        Point[] points = new Point[3];
        points[0] = new Point(1, 2);
        points[1] = new Point(2, 0);

        List<String> pointsList = CollectionUtils2.transformIntoList(points, true);

        Assert.isNotEmpty(pointsList);
        Assert.that(pointsList, Matchers.contains("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", "null"));
    }

    /**
     * Test method for
     * {@link CollectionUtils2#transformIntoSet(I[], org.apache.commons.collections4.Transformer)}
     * .
     */
    @Test
    public void testTransformIntoSetIArrayTransformerOfIO() {
        Point[] points = new Point[3];
        points[0] = new Point(1, 2);
        points[1] = new Point(2, 0);

        Set<String> pointsList = CollectionUtils2.transformIntoSet(points, TRANSFORMER);

        Assert.isNotEmpty(pointsList);
        Assert.that(pointsList, Matchers.containsInAnyOrder("1, 2", "2, 0", null));
    }

    /**
     * Test method for {@link CollectionUtils2#transformIntoSet(I[])}.
     */
    @Test
    public void testTransformIntoSetIArray() {
        Point[] points = new Point[3];
        points[0] = new Point(1, 2);
        points[1] = new Point(2, 0);

        Set<String> pointsList = CollectionUtils2.transformIntoSet(points);

        Assert.isNotEmpty(pointsList);
        Assert.that(pointsList, Matchers.containsInAnyOrder("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", null));
    }

    /**
     * Test method for {@link CollectionUtils2#transformIntoSet(I[], boolean)}.
     */
    @Test
    public void testTransformIntoSetIArrayB() {
        Point[] points = new Point[3];
        points[0] = new Point(1, 2);
        points[1] = new Point(2, 0);

        Set<String> pointsList = CollectionUtils2.transformIntoSet(points, true);

        Assert.isNotEmpty(pointsList);
        Assert.that(pointsList, Matchers.containsInAnyOrder("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", "null"));
    }
}
