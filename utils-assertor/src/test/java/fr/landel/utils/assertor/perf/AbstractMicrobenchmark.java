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
/*
 * Copyright 2012 The Netty Project
 *
 * The Netty Project licenses this file to you under the Apache License,
 * version 2.0 (the "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at:
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 */
package fr.landel.utils.assertor.perf;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.util.Collection;

import org.openjdk.jmh.results.BenchmarkResult;
import org.openjdk.jmh.results.IterationResult;
import org.openjdk.jmh.results.RunResult;
import org.openjdk.jmh.results.format.ResultFormatType;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.ChainedOptionsBuilder;
import org.openjdk.jmh.runner.options.OptionsBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.landel.utils.assertor.Assertor;

/**
 * Base class for all JMH benchmarks.
 */
public abstract class AbstractMicrobenchmark {

    protected static final int WARMUP_ITERATIONS = 3;
    protected static final int MEASURE_ITERATIONS = 5;
    protected static final int NUM_FORKS = 2;

    protected static final String JVM_ARGS = "-server";

    protected static final String OUTPUT_DIRECTORY = "target/benchmark/";

    private static final Logger LOGGER = LoggerFactory.getLogger("Microbenchmark");

    public void run() throws IOException, RunnerException {
        final String classPath = getClass().getCanonicalName();

        final ChainedOptionsBuilder runnerOptions = new OptionsBuilder().include(classPath).jvmArgs(this.getJvmArgs())
                .warmupIterations(this.getWarmupIterations()).measurementIterations(this.getMeasureIterations()).forks(this.getNumForks());

        final File file = new File(this.getOutputDirectory(), classPath + ".json");
        if (file.exists()) {
            file.delete();
        } else {
            file.getParentFile().mkdirs();
            file.createNewFile();
        }

        runnerOptions.resultFormat(ResultFormatType.JSON);
        runnerOptions.result(file.getAbsolutePath());

        final Runner runner = new Runner(runnerOptions.build());

        final Collection<RunResult> runResults = runner.run();

        assertNotNull(runResults);
        assertNotEquals(0, runResults.size());

        for (final RunResult runResult : runResults) {
            assertNotEquals(0, runResult.getBenchmarkResults().size());

            for (final BenchmarkResult result : runResult.getBenchmarkResults()) {
                assertEquals(this.getMeasureIterations(), result.getIterationResults().size());

                for (final IterationResult ir : result.getIterationResults()) {
                    final Double avgScore = ir.getRawPrimaryResults().stream().map((r) -> r.getScore()).reduce((s1, s2) -> (s1 + s2) / 2)
                            .get();
                    LOGGER.info("score: {} {}", avgScore, ir.getScoreUnit());
                    Assertor.that(avgScore).isGT(this.getExpectedMinNbOpsPerSeconds())
                            .toThrow((errors, parameters) -> new AssertionError(errors));
                }
            }
        }
    }

    /**
     * @return the warmupIterations
     */
    protected int getWarmupIterations() {
        return AbstractMicrobenchmark.WARMUP_ITERATIONS;
    }

    /**
     * @return the measureIterations
     */
    protected int getMeasureIterations() {
        return AbstractMicrobenchmark.MEASURE_ITERATIONS;
    }

    /**
     * @return the numForks
     */
    protected int getNumForks() {
        return AbstractMicrobenchmark.NUM_FORKS;
    }

    /**
     * @return the jvmArgs
     */
    protected String getJvmArgs() {
        return AbstractMicrobenchmark.JVM_ARGS;
    }

    /**
     * @return the outputDirectory
     */
    protected String getOutputDirectory() {
        return AbstractMicrobenchmark.OUTPUT_DIRECTORY;
    }

    /**
     * @return the expectedMinNbOpsPerSeconds
     */
    protected abstract double getExpectedMinNbOpsPerSeconds();
}