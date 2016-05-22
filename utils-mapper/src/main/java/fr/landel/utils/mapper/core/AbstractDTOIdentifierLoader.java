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
package fr.landel.utils.mapper.core;

import fr.landel.utils.mapper.DTOIdentifier;
import fr.landel.utils.mapper.DTOIdentifierImpl;

/**
 * The abstract DTO identifier loader (to load in the manager all identifiers).
 *
 * @since 30 nov. 2015
 * @author Gilles
 *
 */
public abstract class AbstractDTOIdentifierLoader {

    private DTOIdentifierManager dtoIdentifierManager;

    /**
     * 
     * Constructor
     *
     */
    public AbstractDTOIdentifierLoader() {
    }

    /**
     * Get the base identifier
     * 
     * @return a DTO identifier
     */
    protected final DTOIdentifier getBase() {
        return this.addIdentifier(DTOIdentifier.BASE_IDENTIFIER, DTOIdentifier.DEFAULT_LIST_DEEP);
    }

    /**
     * Add the identifier into the manager
     * 
     * @param name
     *            The identifier name (name used in each mappable class)
     * @param deep
     *            The deep to map
     * @param identifiers
     *            The list of identifiers
     * @return The new identifier instance (added to the manager)
     */
    protected final DTOIdentifier addIdentifier(final String name, final int deep, final DTOIdentifier... identifiers) {
        final DTOIdentifier dtoIdentifier;
        if (this.dtoIdentifierManager.containsKey(name)) {
            dtoIdentifier = this.dtoIdentifierManager.get(name);
        } else {
            dtoIdentifier = new DTOIdentifierImpl(name, deep, identifiers);

            this.dtoIdentifierManager.addIdentifier(dtoIdentifier);
        }
        return dtoIdentifier;
    }
}
