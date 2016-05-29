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
package fr.landel.utils.commons.mdc;

import java.util.concurrent.Callable;

import org.junit.Assert;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Runnable to test MDC thread safe
 *
 * @since 11 dec. 2015
 * @author Gilles Landel
 *
 */
public class MDCTask implements Callable<Long> {

    private final Logger logger;
    private final long id;
    private final int loop;

    /**
     * Constructor
     * 
     * @param id
     *            The task id
     * @param loop
     *            The number of loops
     */
    public MDCTask(final long id, final int loop) {
        this.logger = LoggerFactory.getLogger(this.getClass());
        this.id = id;
        this.loop = loop;
    }

    @Override
    public Long call() throws Exception {
        final int timeout = 10;
        for (int i = 0; i < this.loop; i++) {
            this.log(i);

            Thread.sleep(Math.round(Math.random() * timeout));
        }
        return this.id;
    }

    private synchronized void log(final int i) {
        String key = "test";
        String keyRemove = "key_to_remove";
        String value = "THREAD: " + this.id + ", task: " + i;

        MDCMT.put(this.id, keyRemove, value);
        Assert.assertTrue(MDCMT.containsKey(this.id, keyRemove));
        MDCMT.clear(this.id);
        Assert.assertFalse(MDCMT.containsKey(this.id, keyRemove));

        MDCMT.put(this.id, key, value);
        Assert.assertTrue(MDCMT.containsKey(this.id, key));
        Assert.assertTrue(MDCMT.containsValue(this.id, value));
        Assert.assertEquals(value, MDCMT.get(this.id, key));

        MDCMT.put(this.id, keyRemove, value);
        Assert.assertTrue(MDCMT.containsKey(this.id, keyRemove));
        MDCMT.remove(this.id, keyRemove);
        Assert.assertFalse(MDCMT.containsKey(this.id, keyRemove));

        MDCMT.trace(this.id, this.logger, "message");
        MDCMT.trace(this.id, this.logger, "message", new IllegalAccessError());
        MDCMT.debug(this.id, this.logger, "message");
        MDCMT.debug(this.id, this.logger, "message", new IllegalArgumentException());
        MDCMT.info(this.id, this.logger, "message");
        MDCMT.info(this.id, this.logger, "message", new IllegalArgumentException());
        MDCMT.warn(this.id, this.logger, "message");
        MDCMT.warn(this.id, this.logger, "message", new IllegalArgumentException());
        MDCMT.error(this.id, this.logger, "message");
        MDCMT.error(this.id, this.logger, "message", new IllegalArgumentException());
    }
}
