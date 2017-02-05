/*
 * #%L
 * utils-mapper
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
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
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import fr.landel.utils.mapper.DTOIdentifier;
import fr.landel.utils.mapper.EnumMode;

/**
 * Annotation to map properties from a DTO to an entity (and vis versa).
 *
 * @since Jul 14, 2015
 * @author Erwan Ropartz
 * @author Gilles Landel
 *
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD, ElementType.METHOD})
@Repeatable(MappablesProperty.class)
public @interface MappableProperty {

    /**
     * The mapped field name
     * 
     * @return The name of the foreign property, if empty, the property name is
     *         used
     */
    String name() default "";

    /**
     * The mode to map the Entity into DTO or the reverse
     * 
     * @return The list of mode to map the property
     */
    EnumMode[]mode() default {EnumMode.DEFAULT};

    /**
     * The default identifier, if load or save are specified they are used
     * instead of this one
     * 
     * @return The list of identifiers used to load and save
     */
    String[]value() default {};

    /**
     * The deep of object to load or to save. (parent class -&gt; child class
     * -&gt; sub child class = 2). This property override the deep defined by
     * the identifier
     * 
     * @return The deep to load or save
     */
    int deep() default DTOIdentifier.USE_DTO_IDENTIFIER_DEEP;
}
