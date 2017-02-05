/*-
 * #%L
 * utils-model
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.model;

import fr.landel.utils.model.dao.ReadDAO;
import fr.landel.utils.model.mappable.EntityParent;

/**
 * To read parent entity
 *
 * @since Aug 12, 2016
 * @author Gilles
 *
 */
public interface ReadDAOParent extends ReadDAO<EntityParent, String> {
}
