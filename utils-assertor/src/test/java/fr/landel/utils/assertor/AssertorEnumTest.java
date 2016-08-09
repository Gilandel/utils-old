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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;

import org.junit.Test;

import fr.landel.utils.assertor.expect.Expect;
import fr.landel.utils.commons.EnumChar;

/**
 * Check {@link AssertorEnum}
 *
 * @since 18 juil. 2016
 * @author Gilles
 *
 */
public class AssertorEnumTest extends AbstractTest {

    /**
     * Test method for {@link AssertorEnum#AssertorEnum()} .
     */
    @Test
    public void testConstructor() {
        assertNotNull(new AssertorEnum());
    }

    /**
     * Test method for {@link AssertorEnum} .
     */
    @Test
    public void testValues() {
        Assertor.that(EnumType.values())
                .containsAll(new EnumType[] {EnumType.BOOLEAN, EnumType.NUMBER_INTEGER, EnumType.NUMBER_DECIMAL, EnumType.ARRAY,
                        EnumType.ENUMERATION, EnumType.ITERABLE, EnumType.MAP, EnumType.DATE, EnumType.CHAR_SEQUENCE, EnumType.CLASS,
                        EnumType.CHARACTER, EnumType.UNKNOWN})
                .toThrow();

        Assertor.that(EnumType.BOOLEAN).hasName("BOOLEAN").isOK();
    }

    /**
     * Test method for {@link AssertorEnum#hasName} .
     */
    @Test
    public void testHasName() {
        AssertorResult<EnumChar> assertorResult = new AssertorResult<>(EnumChar.ASTERISK, EnumType.ENUMERATION);
        assertTrue(AssertorEnum.hasName(assertorResult, "ASTERISK", null, null, null).isValid());

        try {
            Assertor.that(EnumChar.ASTERISK).hasName("ASTERISK").toThrow("not found");
            Assertor.that(EnumChar.ASTERISK).not().hasName("ASTERIS").toThrow("not found");
            Assertor.that(EnumChar.ASTERISK).hasName("ASTERISK").toThrow(new IllegalArgumentException(), true);
            Assertor.that(EnumChar.ASTERISK).hasNameIgnoreCase("asTerisK").toThrow("not found");
            Assertor.that(EnumChar.ASTERISK).not().hasNameIgnoreCase("asTeris").toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }

        Expect.exception(() -> {
            Assertor.that(EnumChar.ASTERISK).hasName("asterisk").toThrow("not found");
            fail();
        }, IllegalArgumentException.class, "not found");

        Expect.exception(() -> {
            Assertor.that(EnumChar.ASTERISK).hasName("asterisk", "%s, '%s*'", "not found").toThrow();
            fail();

            // to string = unicode character
        }, IllegalArgumentException.class, "not found, '*'");

        Expect.exception(() -> {
            Assertor.that(EnumChar.ASTERISK).hasName("asterisk").toThrow(new IOException("not found"), true);
            fail();
        }, IOException.class, "not found");

        Expect.exception(() -> {
            Assertor.that((EnumChar) null).hasName("asterisk").toThrow(new IOException("not found"), true);
            fail();
        }, IOException.class, "not found");

        Expect.exception(() -> {
            Assertor.that(EnumChar.ASTERISK).hasName("").toThrow(new IOException("not found"), true);
            fail();
        }, IOException.class, "not found");
    }

    /**
     * Test method for {@link AssertorEnum#hasOrdinal} .
     */
    @Test
    public void testHasOrdinal() {
        try {
            assertTrue(Assertor.that(EnumOperator.OR).hasOrdinal(1).isOK());

            Assertor.that(EnumOperator.OR).isNotNull().toThrow();

            Assertor.that(EnumOperator.OR).hasOrdinal(1).toThrow();
            Assertor.that(EnumOperator.OR).hasOrdinal(1).and(false).not().isTrue().toThrow("not true");
            Assertor.that(EnumOperator.OR).hasOrdinal(1).toThrow(new IllegalArgumentException(), true);

            Assertor.that(EnumOperator.OR).hasOrdinal(1).and().not().hasName("xor").toThrow();
            Assertor.that(EnumOperator.OR).hasOrdinal(1).or().hasName("xor").toThrow();
            Assertor.that(EnumOperator.OR).hasOrdinal(1).xor().hasName("xor").toThrow();

            Assertor.that(EnumOperator.OR).hasOrdinal(1).and(Assertor.that("").isBlank().or().isEqual("r")).toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }

        Expect.exception(() -> {
            Assertor.that(EnumOperator.OR).hasOrdinal(0).toThrow("not correct");
            fail();
        }, IllegalArgumentException.class, "not correct", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that(EnumOperator.OR).hasOrdinal(0, "%s, '%s*'", "not correct").toThrow();
            fail();
        }, IllegalArgumentException.class, "not correct, ' OR '", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that((EnumOperator) null).hasOrdinal(0).toThrow(new IOException("not correct"), true);
            fail();
        }, IOException.class, "not correct", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that(EnumOperator.OR).hasOrdinal(-1).toThrow(new IOException("not correct"), true);
            fail();
        }, IOException.class, "not correct", JUNIT_ERROR);
    }
}
