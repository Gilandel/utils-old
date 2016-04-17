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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.LoggerFactory;

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.encoder.PatternLayoutEncoder;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.OutputStreamAppender;

import org.gl.utils.commons.StringUtils;

/**
 * MDC Thread Safe check
 * 
 * @since 11 déc. 2015
 * @author Gilles Landel
 *
 */
public class MDCTSTest {

    private static final org.slf4j.Logger LOGGER = LoggerFactory.getLogger(MDCTSTest.class);

    private static final Pattern VALIDATOR = Pattern.compile("MDCTask \\[THREAD: (\\d+), task: (\\d)\\] message");
    private static final int VALIDATOR_GP_THREAD = 1;
    private static final int VALIDATOR_GP_TASK = 2;

    private Logger logger;
    private ExecutorService executorService;
    private ByteArrayOutputStream stream;
    private OutputStreamAppender<ILoggingEvent> appender;

    /**
     * Default constructor
     */
    public MDCTSTest() {
    }

    /**
     * Init the appender and create the pool executor
     */
    @Before
    public void init() {
        this.executorService = Executors.newCachedThreadPool();

        LoggerContext loggerContext = (LoggerContext) LoggerFactory.getILoggerFactory();

        this.logger = loggerContext.getLogger(MDCTask.class);

        // Destination stream
        this.stream = new ByteArrayOutputStream();

        // Encoder
        PatternLayoutEncoder encoder = new PatternLayoutEncoder();
        encoder.setContext(loggerContext);
        encoder.setPattern("%logger{0} [%mdc{test}] %m%n");
        encoder.setImmediateFlush(true);
        encoder.start();

        // Appender
        this.appender = new OutputStreamAppender<>();
        this.appender.setName("OutputStream Appender " + MDCTSTest.class.getSimpleName());
        this.appender.setContext(loggerContext);
        this.appender.setEncoder(encoder);
        this.appender.setOutputStream(this.stream);
        this.appender.start();

        this.logger.addAppender(this.appender);
    }

    /**
     * Dispose appender and executor
     */
    @After
    public void dispose() {
        this.appender.stop();
        this.executorService.shutdown();
    }

    /**
     * Check the put
     * 
     * @throws InterruptedException
     *             on interrupted thread
     * @throws ExecutionException
     *             on execution failed
     * @throws IOException
     *             on stream error
     */
    @Test
    public void testPut() throws InterruptedException, ExecutionException, IOException {
        this.stream.reset();

        final List<Future<Long>> futures = new ArrayList<>();

        final long max = 20;
        final int loop = 10;
        for (long i = 0; i < max; i++) {
            futures.add(this.executorService.submit(new MDCTask(i, loop)));
        }

        for (Future<Long> future : futures) {
            LOGGER.info("THREAD " + future.get() + " DONE!");
        }

        this.stream.flush();

        LOGGER.info("ALL DONE!");

        final String outputLog = this.stream.toString("UTF-8");

        LOGGER.info(outputLog);

        final String[] lines = outputLog.split(System.lineSeparator());

        assertNotNull(lines);
        assertEquals(max * loop, lines.length);

        final Map<Integer, Set<Integer>> results = new HashMap<>();

        for (int i = 0; i < lines.length; i++) {
            Matcher matcher = VALIDATOR.matcher(lines[i]);

            assertTrue("Validator failed on line: " + i + ", content: " + lines[i], matcher.matches());

            String thread = matcher.group(VALIDATOR_GP_THREAD);
            String task = matcher.group(VALIDATOR_GP_TASK);

            assertTrue(StringUtils.isNumeric(thread));
            assertTrue(StringUtils.isNumeric(task));

            Integer threadInt = Integer.parseInt(thread);
            Integer taskInt = Integer.parseInt(task);

            if (!results.containsKey(threadInt)) {
                results.put(threadInt, new HashSet<Integer>());
            }

            results.get(threadInt).add(taskInt);
        }

        for (int i = 0; i < max; i++) {
            assertEquals(loop, results.get(i).size());
        }
    }
}
