/*-
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
package fr.landel.utils.model;

import org.springframework.stereotype.Repository;

import fr.landel.utils.model.dao.AbstractWriteDAO;
import fr.landel.utils.model.mappable.EntityChild;

/**
 * To write child entity
 *
 * @since Aug 12, 2016
 * @author Gilles
 *
 */
@Repository
public class WriteDAOChildImpl extends AbstractWriteDAO<EntityChild, String> implements WriteDAOChild {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -1317226598430946159L;

    /**
     * 
     * Constructor
     *
     */
    public WriteDAOChildImpl() {
        super(EntityChild.class);
    }

}
