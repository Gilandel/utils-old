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
import fr.landel.utils.model.mappable.EntityParent;

/**
 * To write parent entity
 *
 * @since Aug 12, 2016
 * @author Gilles
 *
 */
@Repository
public class WriteDAOParentImpl extends AbstractWriteDAO<EntityParent, String> implements WriteDAOParent {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 7861769895061407705L;

    /**
     * 
     * Constructor
     *
     */
    public WriteDAOParentImpl() {
        super(EntityParent.class);
    }

}
