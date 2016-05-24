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
 * Annotation for repeatable mappables property.
 *
 * @since 14 juil. 2015
 * @author Gilles
 *
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD, ElementType.METHOD})
public @interface MappablesProperty {

    /**
     * The mapped field name
     * 
     * @return The name of the foreign property, if empty, the property name is
     *         used
     */
    String name() default "";

    /**
     * The sub annotations list
     * 
     * @return The list mappable property annotations
     */
    MappableProperty[]value();
}
