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

import java.io.IOException;

import org.junit.Test;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.runner.RunnerException;

import fr.landel.utils.assertor.Constants.MSG;

/**
 * Checks assertor performance
 *
 * @since Aug 8, 2016
 * @author Gilles
 *
 */
@State(Scope.Benchmark)
public class HelperMessagePerf extends AbstractMicrobenchmark {

    @Override
    protected double getExpectedMinNbOpsPerSeconds() {
        return 250_000d;
    }

    /**
     * Perf method for {@link HelperMessage#getMessage} with {@code Boolean}.
     */
    @Benchmark
    public void assertorBasicPerf2() {
        final AssertorResult<String> a = new AssertorResult<>("test", EnumType.CHAR_SEQUENCE);
        final AssertorResult<Boolean> b = new AssertorResult<>(true, EnumType.BOOLEAN);

        // precondition: true & true, valid: true & false
        AssertorResult<String> assertorResult1 = new AssertorResult<>(a, true, true, "pre-error1", "error1");
        AssertorResult<Boolean> assertorResult2 = new AssertorResult<>(b, true, false, "pre-error2", "error2");

        AssertorResult<String> assertorResult = new AssertorResult<>(assertorResult1, assertorResult2, EnumOperator.AND);

        HelperMessage.getMessage(assertorResult, null, null, null, MSG.BOOLEAN.TRUE, false, 0);
    }

    @Test
    public void testPerf() throws IOException, RunnerException {
        assertNotNull(super.run());
    }
}
