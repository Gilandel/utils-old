/*
 * #%L
 * gl-utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package org.gl.utils.commons.mdc;

import java.util.concurrent.Callable;

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
            this.info(i);

            Thread.sleep(Math.round(Math.random() * timeout));
        }
        return this.id;
    }

    private synchronized void info(final int i) {
        MDCTS.put(this.id, "test", "THREAD: " + this.id + ", task: " + i);
        MDCTS.info(this.id, this.logger, "message");
    }
}
