/*
 * #%L
 * utils-mapper
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.mapper.mappable;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to map a DTO into an entity (and vis versa).
 *
 * @since Jul 14, 2015
 * @author Erwan Ropartz
 * @author Gilles Landel
 *
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface Mappable {

    /**
     * Reverse mapped class
     * 
     * @return The list of mapped remote class
     */
    Class<?>[]value();
}
