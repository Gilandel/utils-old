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

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;

import fr.landel.utils.aop.exception.AOPException;

/**
 * Profile aspect
 *
 * @since 2 déc. 2015
 * @author Gilles
 *
 */
@Aspect
public class ProfilingAspect extends AbstractAspectExtends {

    /**
     * Constructor
     */
    public ProfilingAspect() {
        super();
    }

    /**
     * point cut for all methods of the service layer of the application
     */
    @Pointcut("execution(* " + BASE_PACKAGE + ".*.*(..))")
    public void trace() {
    }

    /**
     * Profile trace service
     * 
     * @param call
     *            The call join point
     * @return The proceeded object
     * @throws AOPException
     *             The exception thrown during proceed
     */
    @Around("trace()")
    public final Object profileService(final ProceedingJoinPoint call) throws AOPException {
        return super.profile(call);
    }
}