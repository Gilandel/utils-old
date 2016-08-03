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

import static org.junit.Assert.assertTrue;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;

import org.junit.Test;

/**
 * Assert generated type object
 *
 * @since 2 août 2016
 * @author Gilles
 *
 */
public class AssertorTest extends AbstractTest {

    /**
     * Test method for {@link fr.landel.utils.assertor.Assertor#that}.
     */
    @Test
    public void testThatN() {
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
