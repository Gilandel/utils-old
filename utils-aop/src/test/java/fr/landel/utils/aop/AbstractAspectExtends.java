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

import fr.landel.utils.aop.AbstractAspect;

/**
 * Aspect implementation for tests
 *
 * @since 2 déc. 2015
 * @author Gilles
 *
 */
public abstract class AbstractAspectExtends extends AbstractAspect {

    /**
     * Base package path
     */
    protected static final String BASE_PACKAGE = "fr.landel.utils.aop.observable";

    /**
     * 
     * Constructor
     *
     */
    public AbstractAspectExtends() {
        super();
    }
}
