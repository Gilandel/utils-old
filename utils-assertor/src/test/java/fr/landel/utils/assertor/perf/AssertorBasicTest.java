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
package fr.landel.utils.assertor.perf;

import java.io.IOException;

import org.junit.Test;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.runner.RunnerException;

import fr.landel.utils.assertor.Assertor;

/**
 * Checks assertor performance
 *
 * @since Aug 8, 2016
 * @author Gilles
 *
 */
@State(Scope.Thread)
public class AssertorBasicTest extends AbstractMicrobenchmark {

    @Override
    protected double getExpectedMinNbOpsPerSeconds() {
        return 500_000d;
    }

    /**
     * Test method for {@link Assertor}.
     */
    @Benchmark
    public void assertorBasicPerf() {
        Assertor.that(true).isTrue().isOK();
        Assertor.that(true).isTrue().getErrors();
        Assertor.that(true).isTrue().toThrow();

        Assertor.that("text").contains("ex").isOK();
        Assertor.that("text").contains("ex").getErrors();
        Assertor.that("text").contains("ex").toThrow();
    }

    @Test
    public void perfTest() throws IOException, RunnerException {
        super.run();
    }
}
