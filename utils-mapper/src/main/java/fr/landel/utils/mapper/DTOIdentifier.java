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
package fr.landel.utils.mapper;

import java.util.Set;

/**
 * DTO identifier.
 *
 * @since Dec 1, 2015
 * @author Gilles
 *
 */
public interface DTOIdentifier {

    /**
     * Used by the annotation on the mappable property to specify to use the
     * deep defined by the DTO identifier
     */
    int USE_DTO_IDENTIFIER_DEEP = Short.MIN_VALUE;

    /**
     * The maximum to analyze (beware, could be infinite)
     */
    int MAX_DEEP = -1;

    /**
     * The default deep to load objects in list mode
     */
    int DEFAULT_LIST_DEEP = 1;

    /**
     * The default deep to load objects in details mode
     */
    int DEFAULT_DETAILS_DEEP = 2;

    /**
     * The default deep to save objects
     */
    int DEFAULT_SAVE_DEEP = 1;

    /**
     * THE BASE IDENTIFIER
     */
    String BASE_IDENTIFIER = "DTOIdentifier_BASE";

    /**
     * @return The name
     */
    String name();

    /**
     * @return The deep
     */
    int deep();

    /**
     * @return the identifiers
     */
    Set<DTOIdentifier> getIdentifiers();

    /**
     * @return the identifiers list recursively
     */
    Set<DTOIdentifier> getIdentifiersRecursively();
}
