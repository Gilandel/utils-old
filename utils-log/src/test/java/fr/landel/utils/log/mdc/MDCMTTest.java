/*
 * #%L
 * utils-log
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.log.mdc;

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

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.encoder.PatternLayoutEncoder;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.OutputStreamAppender;
import fr.landel.utils.commons.StringUtils;

/**
 * MDC multi-thread helper test
 * 
 * @since 11 dec. 2015
 * @author Gilles Landel
 *
 */
public class MDCMTTest {

    private static final org.slf4j.Logger LOGGER = LoggerFactory.getLogger(MDCMTTest.class);

    private static final Pattern VALIDATOR = Pattern
            .compile("MDCTask \\- (TRACE|DEBUG|INFO|WARN|ERROR) \\[THREAD: (\\d+), task: (\\d)\\] message");
    private static final int VALIDATOR_GP_THREAD = 2;
    private static final int VALIDATOR_GP_TASK = 3;

    private ExecutorService executorService;
    private ByteArrayOutputStream stream;
    private OutputStreamAppender<ILoggingEvent> appender;
    private Logger logger;

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
        encoder.setPattern("%logger{0} - %level [%mdc{test}] %m%n");
        encoder.setImmediateFlush(true);
        encoder.start();

        // Appender
        this.appender = new OutputStreamAppender<>();
        this.appender.setName("OutputStream Appender " + MDCMTTest.class.getSimpleName());
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
     * Global check
     * 
     * @throws InterruptedException
     *             on interrupted thread
     * @throws ExecutionException
     *             on execution failed
     * @throws IOException
     *             on stream error
     */
    @Test
    public void testMain() throws InterruptedException, ExecutionException, IOException {
        // trace, debug, info, warn, error (NB-1, trace logs aren't displayed)
        this.logger.setLevel(Level.DEBUG);
        final int logLevel = 4;
        final int nbLinesByLogLevel = 2; // normal + exception
        final int exceptionLength = 8;

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
        assertEquals(max * loop * logLevel * (nbLinesByLogLevel + exceptionLength), lines.length);

        final Map<Integer, Set<Integer>> results = new HashMap<>();

        int count = 0;
        for (int i = 0; i < lines.length; i++) {
            Matcher matcher = VALIDATOR.matcher(lines[i]);

            if (matcher.matches()) {
                count++;

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
        }

        assertEquals(max * loop * logLevel * nbLinesByLogLevel, count);

        for (int i = 0; i < max; i++) {
            assertEquals(loop, results.get(i).size());
        }
    }

    @Test
    public void testLogTrace() throws InterruptedException, ExecutionException, IOException {
        // trace, debug, info, warn, error (NB-1, trace logs aren't displayed)
        this.logger.setLevel(Level.TRACE);
        final int logLevel = 5;
        final int nbLinesByLogLevel = 2; // normal + exception
        final int exceptionLength = 8;

        this.stream.reset();

        final List<Future<Long>> futures = new ArrayList<>();

        futures.add(this.executorService.submit(new MDCTask(0, 1)));

        for (Future<Long> future : futures) {
            LOGGER.info("THREAD " + future.get() + " DONE!");
        }

        this.stream.flush();

        LOGGER.info("ALL DONE!");

        final String outputLog = this.stream.toString("UTF-8");

        LOGGER.info(outputLog);

        final String[] lines = outputLog.split(System.lineSeparator());

        assertNotNull(lines);
        assertEquals(logLevel * (nbLinesByLogLevel + exceptionLength), lines.length);

        final Map<Integer, Set<Integer>> results = new HashMap<>();

        int count = 0;
        for (int i = 0; i < lines.length; i++) {
            Matcher matcher = VALIDATOR.matcher(lines[i]);

            if (matcher.matches()) {
                count++;

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
        }

        assertEquals(logLevel * nbLinesByLogLevel, count);

        assertEquals(1, results.get(0).size());
    }

    @Test
    public void testLogOFF() throws InterruptedException, ExecutionException, IOException {
        // trace, debug, info, warn, error (NB-1, trace logs aren't displayed)
        this.logger.setLevel(Level.OFF);

        this.stream.reset();

        final List<Future<Long>> futures = new ArrayList<>();

        futures.add(this.executorService.submit(new MDCTask(0, 1)));

        for (Future<Long> future : futures) {
            LOGGER.info("THREAD " + future.get() + " DONE!");
        }

        this.stream.flush();

        LOGGER.info("ALL DONE!");

        final String outputLog = this.stream.toString("UTF-8");

        LOGGER.info(outputLog);

        assertNotNull(outputLog);
        assertEquals(0, outputLog.trim().length());
    }
}
