/*
 * #%L
 * utils-asserts
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.asserts;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import org.hamcrest.Matcher;
import org.hamcrest.Matchers;
import org.junit.Test;

/**
 * Check assert
 *
 * @since 10 dec. 2015
 * @author Gilles Landel
 *
 */
public class AssertorMatcher {

    /**
     * Test method for {@link Expect#that(Object, Matcher)} .
     */
    @Test
    public void testThatOK() {
        try {
            final String red = "red";
            final String green = "green";
            final String blue = "blue";
            final String alpha = "alpha";

            final int max = 255;

            final int nbColors = 4;
            final List<Color> colors = new ArrayList<>(nbColors);

            colors.add(Color.BLACK);
            colors.add(Color.WHITE);
            colors.add(Color.BLUE);
            colors.add(Color.CYAN);

            Matcher<Color> matcherAlpha = Matchers.hasProperty(alpha, Matchers.is(max));

            Matcher<Color> matcherBlack = Matchers.allOf(Matchers.hasProperty(green, Matchers.is(0)),
                    Matchers.hasProperty(red, Matchers.is(0)), Matchers.hasProperty(blue, Matchers.is(0)), matcherAlpha);

            Matcher<Color> matcherWhite = Matchers.allOf(Matchers.hasProperty(green, Matchers.is(max)),
                    Matchers.hasProperty(red, Matchers.is(max)), Matchers.hasProperty(blue, Matchers.is(max)), matcherAlpha);

            Matcher<Color> matcherBlue = Matchers.allOf(Matchers.hasProperty(green, Matchers.is(0)),
                    Matchers.hasProperty(red, Matchers.is(0)), Matchers.hasProperty(blue, Matchers.is(max)), matcherAlpha);

            Matcher<Color> matcherCyan = Matchers.allOf(Matchers.hasProperty(green, Matchers.is(max)),
                    Matchers.hasProperty(red, Matchers.is(0)), Matchers.hasProperty(blue, Matchers.is(max)), matcherAlpha);

            List<Matcher<? super Color>> matcherList = Arrays.<Matcher<? super Color>> asList(matcherBlack, matcherWhite, matcherBlue,
                    matcherCyan);

            Assertor.that(colors).matches(Matchers.hasSize(nbColors)).and().matches(Matchers.contains(matcherList)).toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#that(Object, Matcher)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testThatKO() {
        final List<Color> colors = new ArrayList<>();

        colors.add(Color.BLACK);
        colors.add(Color.WHITE);
        colors.add(Color.BLUE);
        colors.add(Color.CYAN);

        Assertor.that(colors).matches(Matchers.hasSize(colors.size() - 1)).toThrow();
    }

    /**
     * Test method for {@link Assertor#check} .
     */
    public void testCheck() {
        assertTrue(AssertIterable.class.isAssignableFrom(Assertor.that(new ArrayList<Color>()).getClass()));
        assertTrue(AssertObject.class.isAssignableFrom(Assertor.that(Color.BLACK).getClass()));
        assertTrue(AssertMap.class.isAssignableFrom(Assertor.that(new HashMap<String, Integer>()).getClass()));
        assertTrue(AssertNumber.class.isAssignableFrom(Assertor.that(12).getClass()));
        assertTrue(AssertCharSequence.class.isAssignableFrom(Assertor.that("test").getClass()));
        assertTrue(AssertArray.class.isAssignableFrom(Assertor.that(new String[0]).getClass()));
        assertTrue(AssertDate.class.isAssignableFrom(Assertor.that(new Date()).getClass()));
        assertTrue(AssertClass.class.isAssignableFrom(Assertor.that(String.class).getClass()));
    }
}