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

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.collections4.BidiMap;
import org.apache.commons.collections4.bidimap.DualHashBidiMap;

import fr.landel.utils.mapper.core.DTOIdentifierManager;

/**
 * Abstract DTO identifier.
 *
 * @since 27 nov. 2015
 * @author Gilles
 *
 */
public class DTOIdentifierImpl implements DTOIdentifier {

    private final String name;
    private final int deep;
    private final BidiMap<String, DTOIdentifier> identifiers;
    private final Set<DTOIdentifier> allIdentifiers;

    /**
     * 
     * Constructor
     *
     * @param name
     *            The name of the identifier
     * @param deep
     *            The analysis deep
     * @param identifiers
     *            The included identifiers
     */
    public DTOIdentifierImpl(final String name, final int deep, final DTOIdentifier... identifiers) {
        this.name = name;
        this.deep = deep;
        this.identifiers = new DualHashBidiMap<>();

        // init the identifiers
        if (identifiers != null && identifiers.length > 0) {
            for (DTOIdentifier identifier : identifiers) {
                this.identifiers.put(identifier.name(), identifier);
            }
        }

        final Set<DTOIdentifier> modifiableAllIdentifiers = new HashSet<>();
        this.getIdentifiers(modifiableAllIdentifiers, this);
        this.allIdentifiers = Collections.unmodifiableSet(modifiableAllIdentifiers);
    }

    /**
     * 
     * Constructor (max deep)
     *
     * @param manager
     *            the DTO identifier manager
     * @param name
     *            The name of the identifier
     * @param identifiers
     *            The included identifiers
     */
    public DTOIdentifierImpl(final DTOIdentifierManager manager, final String name, final DTOIdentifier... identifiers) {
        this(name, -1, identifiers);
    }

    private void getIdentifiers(final Set<DTOIdentifier> listedIdentifiers, final DTOIdentifier identifier) {
        if (!listedIdentifiers.contains(identifier)) {
            listedIdentifiers.add(identifier);

            for (DTOIdentifier ident : identifier.getIdentifiers()) {
                this.getIdentifiers(listedIdentifiers, ident);
            }
        }
    }

    @Override
    public Set<DTOIdentifier> getIdentifiers() {
        return this.identifiers.values();
    }

    @Override
    public Set<DTOIdentifier> getIdentifiersRecursively() {
        return this.allIdentifiers;
    }

    @Override
    public String name() {
        return this.name;
    }

    @Override
    public int deep() {
        return this.deep;
    }

    @Override
    public String toString() {
        return this.name + ": " + this.deep;
    }
}
