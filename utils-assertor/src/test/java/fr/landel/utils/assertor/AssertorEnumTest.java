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
     * Test method for
     * {@link AssertorEnum#isName(java.util.function.Supplier, CharSequence, java.util.Locale, CharSequence, Object[])}
     * .
     */
    @Test
    public void testIsName() {
        AssertorResult<EnumChar> assertorResult = new AssertorResult<>(EnumChar.ASTERISK, EnumType.ENUMERATION);
        assertTrue(AssertorEnum.isName(() -> assertorResult, "ASTERISK", null, null, null).get().isValid());

        try {
            Assertor.that(EnumChar.ASTERISK).isName("ASTERISK").toThrow("not found");
            Assertor.that(EnumChar.ASTERISK).not().isName("ASTERIS").toThrow("not found");
            Assertor.that(EnumChar.ASTERISK).isName("ASTERISK").toThrow(new IllegalArgumentException(), true);
            Assertor.that(EnumChar.ASTERISK).isNameIgnoreCase("asTerisK").toThrow("not found");
            Assertor.that(EnumChar.ASTERISK).not().isNameIgnoreCase("asTeris").toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }

        Expect.exception(() -> {
            Assertor.that(EnumChar.ASTERISK).isName("asterisk").toThrow("not found");
            fail();
        }, IllegalArgumentException.class, "not found");

        Expect.exception(() -> {
            Assertor.that(EnumChar.ASTERISK).isName("asterisk", "%s, '%s*'", "not found").toThrow();
            fail();

            // to string = unicode character
        }, IllegalArgumentException.class, "not found, '*'");

        Expect.exception(() -> {
            Assertor.that(EnumChar.ASTERISK).isName("asterisk").toThrow(new IOException("not found"), true);
            fail();
        }, IOException.class, "not found");

        Expect.exception(() -> {
            Assertor.that((EnumChar) null).isName("asterisk").toThrow(new IOException("not found"), true);
            fail();
        }, IOException.class, "not found");

        Expect.exception(() -> {
            Assertor.that(EnumChar.ASTERISK).isName("").toThrow(new IOException("not found"), true);
            fail();
        }, IOException.class, "not found");
    }

    /**
     * Test method for
     * {@link AssertorEnum#isOrdinal(java.util.function.Supplier, int, java.util.Locale, CharSequence, Object[])}
     * .
     */
    @Test
    public void testIsOrdinal() {
        try {
            assertTrue(Assertor.that(EnumOperator.OR).isOrdinal(1).isOK());

            Assertor.that(EnumOperator.OR).isNotNull().toThrow();

            Assertor.that(EnumOperator.OR).isOrdinal(1).toThrow();
            Assertor.that(EnumOperator.OR).isOrdinal(1).and(false).not().isTrue().toThrow("not true");
            Assertor.that(EnumOperator.OR).isOrdinal(1).toThrow(new IllegalArgumentException(), true);

            Assertor.that(EnumOperator.OR).isOrdinal(1).and().not().isName("xor").toThrow();
            Assertor.that(EnumOperator.OR).isOrdinal(1).or().isName("xor").toThrow();
            Assertor.that(EnumOperator.OR).isOrdinal(1).xor().isName("xor").toThrow();

            Assertor.that(EnumOperator.OR).isOrdinal(1).and(Assertor.that("").isBlank().or().isEqual("r")).toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }

        Expect.exception(() -> {
            Assertor.that(EnumOperator.OR).isOrdinal(0).toThrow("not correct");
            fail();
        }, IllegalArgumentException.class, "not correct", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that(EnumOperator.OR).isOrdinal(0, "%s, '%s*'", "not correct").toThrow();
            fail();
        }, IllegalArgumentException.class, "not correct, ' OR '", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that((EnumOperator) null).isOrdinal(0).toThrow(new IOException("not correct"), true);
            fail();
        }, IOException.class, "not correct", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that(EnumOperator.OR).isOrdinal(-1).toThrow(new IOException("not correct"), true);
            fail();
        }, IOException.class, "not correct", JUNIT_ERROR);
    }
}
