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
import fr.landel.utils.model.mappable.EntityParent;

/**
 * To read parent entity
 *
 * @since Aug 12, 2016
 * @author Gilles
 *
 */
@Repository
public class ReadDAOParentImpl extends AbstractReadDAO<EntityParent, String> implements ReadDAOParent {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 4017655301999210038L;

    /**
     * 
     * Constructor
     *
     */
    public ReadDAOParentImpl() {
        super(EntityParent.class);
    }

}
