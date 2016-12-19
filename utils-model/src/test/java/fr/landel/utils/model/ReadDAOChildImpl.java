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

import fr.landel.utils.model.dao.AbstractReadDAO;
import fr.landel.utils.model.mappable.EntityChild;

/**
 * To read child entity
 *
 * @since Aug 12, 2016
 * @author Gilles
 *
 */
@Repository
public class ReadDAOChildImpl extends AbstractReadDAO<EntityChild, String> implements ReadDAOChild {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -8284491249536892296L;

    /**
     * 
     * Constructor
     *
     */
    public ReadDAOChildImpl() {
        super(EntityChild.class);
    }

}
