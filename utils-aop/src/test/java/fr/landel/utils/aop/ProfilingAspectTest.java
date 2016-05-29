/*
 * #%L
 * utils-aop
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.aop;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Test;
import org.springframework.aop.aspectj.annotation.AspectJProxyFactory;

import fr.landel.utils.aop.observable.AOPObservable;
import fr.landel.utils.io.EncodingUtils;

/**
 * Check profiling aspect methods.
 *
 * @since 2 déc. 2015
 * @author Gilles
 *
 */
public class ProfilingAspectTest extends AbstractAspectTest<ProfilingAspect> {

    private static final int MAX_TIMEOUT = 5000;

    /**
     * 
     * Constructor
     *
     */
    public ProfilingAspectTest() {
        super(ProfilingAspect.class);
    }

    /**
     * Check AOP in profile mode (ProceedingJoinPoint)
     * 
     * @throws InterruptedException
     *             If sleep failed
     */
    @Test
    public void profileTest() throws InterruptedException {
        final String expectedLog = ProfilingAspect.class.getSimpleName() + " class " + AOPObservable.class.getCanonicalName()
                + ".testSleep()";
        final Pattern pattern = Pattern.compile(", running time: (\\d+) ms$");

        AOPObservable target = new AOPObservable();

        AspectJProxyFactory factory = new AspectJProxyFactory(target);
        ProfilingAspect aspect = new ProfilingAspect();
        factory.addAspect(aspect);

        AOPObservable proxy = factory.getProxy();

        this.stream.reset();

        proxy.testSleep();

        try {
            String outputLog = this.stream.toString(EncodingUtils.ENCODING_UTF_8);

            if (outputLog.length() > expectedLog.length()) {
                String firstPart = outputLog.substring(0, expectedLog.length());

                assertEquals(expectedLog, firstPart);

                String secondPart = outputLog.substring(expectedLog.length());

                Matcher matcher = pattern.matcher(secondPart);
                assertTrue(matcher.matches());
                assertTrue(MAX_TIMEOUT < Integer.parseInt(matcher.group(1)));
            } else {
                fail("Output stream is too short: " + outputLog);
            }
        } catch (IOException e) {
            fail("Errors occurred in AspectTest#logTest()\n" + e);
        }
    }
}
