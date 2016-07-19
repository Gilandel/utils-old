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
import static org.junit.Assert.fail;

import java.io.IOException;

import org.junit.Test;

import fr.landel.utils.assertor.ConsumerAssert;
import fr.landel.utils.assertor.Expect;

/**
 * Check the expect class
 *
 * @since 16 juil. 2016
 * @author Gilles
 *
 */
public class ExpectTest {

    /**
     * Test method for {@link Expect#exception(ConsumerAssert, Class)}.
     */
    @Test
    public void testExceptionConsumerAssertOfThrowableClassOfT() {
        Expect.exception(() -> {
            throw new IllegalArgumentException();
        }, IllegalArgumentException.class);

        try {
            Expect.exception(() -> {
                throw new IllegalArgumentException();
            }, IOException.class);
            fail();
        } catch (RuntimeException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Expect#exception(ConsumerAssert, Class, String)}.
     */
    @Test
    public void testExceptionConsumerAssertOfThrowableClassOfTString() {
        Expect.exception(() -> {
            throw new IllegalArgumentException("message");
        }, IllegalArgumentException.class, "message");

        try {
            Expect.exception(() -> {
                throw new IllegalArgumentException("message");
            }, IllegalArgumentException.class, "message2");
            fail();
        } catch (RuntimeException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for
     * {@link Expect#exception(ConsumerAssert, Class, Throwable)}.
     * 
     * @throws IOException
     *             On error
     */
    @Test
    public void testExceptionConsumerAssertOfThrowableClassOfTE() throws IOException {
        Expect.exception(() -> {
            throw new IllegalArgumentException();
        }, IllegalArgumentException.class, new IOException());

        try {
            Expect.exception(() -> {
                throw new IllegalArgumentException();
            }, IOException.class, new IOException());
            fail();
        } catch (IOException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for
     * {@link Expect#exception(ConsumerAssert, Class, String, Throwable)}.
     * 
     * @throws IOException
     */
    @Test
    public void testExceptionConsumerAssertOfThrowableClassOfTStringE() throws IOException {
        Expect.exception(() -> {
            throw new IllegalArgumentException("message");
        }, IllegalArgumentException.class, "message", new IOException());

        try {
            Expect.exception(() -> {
                throw new IllegalArgumentException("message");
            }, IllegalArgumentException.class, "message2", new IOException());
            fail();
        } catch (IOException e) {
            assertNotNull(e);
        }
    }
}
