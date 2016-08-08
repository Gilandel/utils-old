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

import org.junit.Test;

public class PredicateStepTest {

    @Test
    public void test() {
        assertTrue(Assertor.that(true).isTrue().isOK());
    }
}
