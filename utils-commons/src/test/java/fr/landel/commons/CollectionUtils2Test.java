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
package fr.landel.commons;

import static org.junit.Assert.fail;

import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.apache.commons.collections4.Transformer;
import org.hamcrest.Matchers;
import org.junit.Test;

import fr.landel.utils.commons.AssertUtils;
import fr.landel.utils.commons.CollectionUtils2;

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

            AssertUtils.that(strPoints, Matchers.contains("1, 2", "2, 0", null));
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

            AssertUtils.that(strPoints, Matchers.containsInAnyOrder("1, 2", "2, 0", null));
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

            AssertUtils.that(strPoints, Matchers.contains("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", null));
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

            AssertUtils.that(strPoints, Matchers.contains("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", "null"));
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

            AssertUtils.that(strPoints, Matchers.containsInAnyOrder("java.awt.Point[x=2,y=0]", "java.awt.Point[x=1,y=2]", null));
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

            AssertUtils.that(strPoints, Matchers.containsInAnyOrder("java.awt.Point[x=2,y=0]", "java.awt.Point[x=1,y=2]", "null"));
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

            AssertUtils.isNotEmpty(pointsArray);
            AssertUtils.that(pointsArray, Matchers.arrayContaining(points.get(0), points.get(1), null));
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

            AssertUtils.isNotEmpty(pointsArray);
            AssertUtils.that(pointsArray, Matchers.arrayContaining("1, 2", "2, 0", null));
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

            AssertUtils.isNotEmpty(pointsArray);
            AssertUtils.that(pointsArray, Matchers.arrayContaining("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", null));
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

            AssertUtils.isNotEmpty(pointsArray);
            AssertUtils.that(pointsArray, Matchers.arrayContaining("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", "null"));
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

            AssertUtils.isNotEmpty(pointsArray);
            AssertUtils.that(pointsArray, Matchers.arrayContaining("1, 2", "2, 0", null));
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

            AssertUtils.isNotEmpty(pointsArray);
            AssertUtils.that(pointsArray, Matchers.arrayContaining("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", null));
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

            AssertUtils.isNotEmpty(pointsArray);
            AssertUtils.that(pointsArray, Matchers.arrayContaining("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", "null"));
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

            AssertUtils.isNotEmpty(pointsList);
            AssertUtils.that(pointsList, Matchers.contains("1, 2", "2, 0", null));
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

            AssertUtils.isNotEmpty(pointsList);
            AssertUtils.that(pointsList, Matchers.contains("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", null));
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

            AssertUtils.isNotEmpty(pointsList);
            AssertUtils.that(pointsList, Matchers.contains("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", "null"));
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

            AssertUtils.isNotEmpty(pointsList);
            AssertUtils.that(pointsList, Matchers.containsInAnyOrder("1, 2", "2, 0", null));
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

            AssertUtils.isNotEmpty(pointsList);
            AssertUtils.that(pointsList, Matchers.containsInAnyOrder("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", null));
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

            AssertUtils.isNotEmpty(pointsList);
            AssertUtils.that(pointsList, Matchers.containsInAnyOrder("java.awt.Point[x=1,y=2]", "java.awt.Point[x=2,y=0]", "null"));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }
}
