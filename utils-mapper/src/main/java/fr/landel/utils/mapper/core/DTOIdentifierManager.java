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

import org.apache.commons.collections4.bidimap.DualHashBidiMap;

import fr.landel.utils.mapper.DTOIdentifier;

/**
 * The DTO identifiers manager.
 *
 * @since 27 nov. 2015
 * @author Gilles
 *
 */
public class DTOIdentifierManager extends DualHashBidiMap<String, DTOIdentifier> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -801533333527194419L;

    /**
     * 
     * Constructor
     *
     */
    protected DTOIdentifierManager() {
        super();
    }

    /**
     * Add identifier to the reflective mapper
     * 
     * @param identifier
     *            The identifier
     */
    public void addIdentifier(final DTOIdentifier identifier) {
        this.put(identifier.name(), identifier);
    }

    /**
     * Remove identifier
     * 
     * @param identifier
     *            The identifier
     */
    public void removeIdentifier(final DTOIdentifier identifier) {
        if (identifier != null && this.containsKey(identifier.name())) {
            this.remove(identifier.name());
        }
    }

    /**
     * Remove recursively the identifier
     * 
     * @param identifier
     *            The identifier
     */
    public void removeIdentifierRecursively(final DTOIdentifier identifier) {
        if (identifier != null) {
            for (DTOIdentifier id : identifier.getIdentifiersRecursively()) {
                if (this.containsKey(id.name())) {
                    this.remove(id.name());
                }
            }
        }
    }
}
