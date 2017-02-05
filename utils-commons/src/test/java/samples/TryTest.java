/*-
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package samples;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.junit.Test;

import fr.landel.utils.commons.tuple.MutableSingle;
import fr.landel.utils.commons.tuple.Single;

/**
 * Test for {@link Try} sample
 *
 * @since Aug 11, 2016
 * @author Gilles
 *
 */
public class TryTest {

    @Test
    public void testThat() {
        final MutableSingle<Boolean> ok = Single.ofMutable(false);

        Try.that(() -> {
            if ("".isEmpty()) {
                throw new IOException("my message");
            }
        }).ifPresent(catched -> {
            assertNotNull(catched);
            assertNotNull(catched.get());
            assertTrue(catched.has("my message"));
            assertTrue(catched.is(IOException.class));

            ok.set(true);
        });

        assertTrue(ok.get());

        ok.set(false);

        Try.that(() -> {
            ok.set(true); // no exception
        }).ifPresent(catched -> {
            ok.set(false);
        });

        assertTrue(ok.get());
    }
}
