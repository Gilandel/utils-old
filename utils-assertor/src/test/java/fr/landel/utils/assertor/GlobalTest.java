/*-
 * #%L
 * utils-assertor
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.assertor;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.awt.Color;
import java.io.IOException;

import org.junit.Test;

import fr.landel.utils.assertor.expect.Expect;

/**
 * (Description)
 *
 * @since 3 août 2016
 * @author Gilles
 *
 */
public class GlobalTest extends AbstractTest {

    // private static final int REPEAT = 1_000;

    /**
     * Test method for {@link PredicateStep#isOK(java.lang.Object)}.
     */
    @Test
    public void testIsOK0() {
        assertEquals("", Assertor.that(Color.BLACK).isNotEqual(Color.BLUE).getErrors());
        assertEquals("the object 'java.awt.Color[r=0,g=0,b=0]' should be equal to 'java.awt.Color[r=0,g=0,b=0]'",
                Assertor.that(Color.BLACK).isEqual(Color.BLUE).getErrors());

        assertTrue(Assertor.that(true).isTrue().xor().isFalse().isOK());
        assertTrue(Assertor.that(true).isNotNull().xor().isFalse().isOK());

        assertTrue(Assertor.that(new String[] {"test"}).isNull().or().contains("test").isOK());

        assertFalse(Assertor.that((String) null).contains("test").and().hasLength(23).isOK());
    }

    /**
     * Test method for {@link PredicateStep#isOK(java.lang.Object)}.
     */
    @Test
    public void testIsOK2() {

        // long start = System.nanoTime();
        //
        // for (int i = 1; i < REPEAT; i++) {

        assertTrue(Assertor.that(Color.BLACK).isNotNull().isOK());

        assertEquals("", Assertor.that(Color.BLACK).isNotNull().getErrors());

        assertTrue(Assertor.that(12).isNotNull().and().isGT(10).isOK());
        assertTrue(Assertor.that(12).isNotNull().and("text").isNotNull().isOK());
        Assertor.that(12).isNotNull().and("text").isNotNull().toThrow((errors, parameters) -> new IllegalArgumentException("error"));

        assertFalse(Assertor.that(12).isGT(20).and("text").isNotNull().isOK());
        assertTrue(Assertor.that(12).not().isGT(20).and("text").isNotNull().isOK());

        assertFalse(Assertor.that(12).isNotNull().and().isEqual(10).and("test").contains("r").isOK());
        // }
        //
        // long end = System.nanoTime();

        // System.out.println(String.format("version 2: %,d ns, avg: %,d ns",
        // end - start, (end - start) / REPEAT));

        assertTrue(Assertor.that("e").isNull().or().isNotBlank().isOK());
        assertFalse(Assertor.that(12).isNotNull().and(Assertor.that("e").isNull().or().isNotBlank().isOK()).isNull().isOK());
        assertTrue(Assertor.that(12).isNotNull().and(Assertor.that("e").isNull().or().isNotBlank()).isOK());
        assertFalse(Assertor.that(12).isNull().and(Assertor.that("e").isNull().or().isNotBlank()).isOK());
        assertTrue(Assertor.that(12).isNull().or(Assertor.that("e").isNull().or().isNotBlank()).isOK());
        assertTrue(Assertor.that(12).isNull().or(Assertor.that("e").isNull().or().isNotBlank()).and().isGT(1).isOK());
        assertFalse(Assertor.that(12).isNull().and(Assertor.that("e").isNull().or().isNotBlank().isOK()).isNotNull().isOK());

        assertEquals(
                "the object '12' should be null AND"
                        + " (the object 'e' should be null OR the char sequence 'e' should be null, empty or blank)"
                        + " AND the number '12' should NOT be equal to '12'",
                Assertor.that(12).isNull().and(Assertor.that("e").isNull().or().isBlank()).and().isNotEqual(12).getErrors());

        Expect.exception(() -> {
            Assertor.that(12).isNotNull().and().isEqual(10).toThrow((errors, params) -> new IOException(errors.toString()));
        }, IOException.class, "the number '12' should be equal to '10'", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that(12).isNotNull().and().isEqual(10).and("test").contains("r")
                    .toThrow((errors, params) -> new IOException(errors.toString()));
        }, IOException.class, "the number '12' should be equal to '10' AND the char sequence 'test' should contain 'r'", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that(12).isNotNull().and().not().isEqual(12).and("test").contains("r")
                    .toThrow((errors, params) -> new IOException(errors.toString()));
        }, IOException.class, "the number '12' should NOT be equal to '12' AND the char sequence 'test' should contain 'r'", JUNIT_ERROR);
    }

}
