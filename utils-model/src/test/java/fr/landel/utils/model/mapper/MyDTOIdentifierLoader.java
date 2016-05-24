/*
 * #%L
 * utils-model
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.model.mapper;

import javax.annotation.PostConstruct;

import org.springframework.stereotype.Component;

import fr.landel.utils.mapper.DTOIdentifier;
import fr.landel.utils.mapper.core.AbstractDTOIdentifierLoader;

/**
 * DTO identifiers loader for tests.
 *
 * @since 27 nov. 2015
 * @author Gilles
 *
 */
@Component
public class MyDTOIdentifierLoader extends AbstractDTOIdentifierLoader implements MyDTOIdentifier {

    @Override
    @PostConstruct
    public void load() {
        final DTOIdentifier base = this.addIdentifier(DTOIdentifier.BASE_IDENTIFIER, DTOIdentifier.DEFAULT_LIST_DEEP);

        // Load parent identifiers
        final DTOIdentifier parentList = this.addIdentifier(PARENT_LIST, 1, base);
        final DTOIdentifier parentDetails = this.addIdentifier(PARENT_DETAILS, 1, parentList);
        final DTOIdentifier parentMax = this.addIdentifier(PARENT_MAX, DTOIdentifier.MAX_DEEP, parentDetails);
        final DTOIdentifier parentSaveList = this.addIdentifier(PARENT_SAVE_LIST, 1, base);
        final DTOIdentifier parentSaveDetails = this.addIdentifier(PARENT_SAVE_DETAILS, 2, parentSaveList);

        // Load child identifiers
        final DTOIdentifier childList = this.addIdentifier(CHILD_LIST, 1, base);
        final DTOIdentifier childDetails = this.addIdentifier(CHILD_DETAILS, 3, childList);

        // Load list identifiers
        final DTOIdentifier list = this.addIdentifier(LIST, DTOIdentifier.DEFAULT_LIST_DEEP, parentList, childList);

        // Load details identifiers
        final DTOIdentifier details = this.addIdentifier(DETAILS, DTOIdentifier.DEFAULT_DETAILS_DEEP, parentDetails, childDetails);

        // Load save identifiers
        final DTOIdentifier save = this.addIdentifier(SAVE, DTOIdentifier.DEFAULT_SAVE_DEEP, parentSaveList, parentSaveDetails);

        // Load all identifiers
        this.addIdentifier(ALL, DTOIdentifier.MAX_DEEP, list, details, save, parentMax);
    }
}
