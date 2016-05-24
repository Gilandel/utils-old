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
package fr.landel.utils.model.hibernate;

import org.hibernate.Hibernate;
import org.hibernate.HibernateException;
import org.hibernate.proxy.HibernateProxy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.landel.utils.commons.stream.FunctionThrowable;
import fr.landel.utils.model.exception.ModelException;

/**
 * (Description)
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public class HibernateUtils {
    private static final Logger LOGGER = LoggerFactory.getLogger(HibernateUtils.class);

    private static final String ERROR_MSG = "Loading failed";

    /**
     * Try to force the loading of lazy collections
     * 
     * @param object
     *            the object to initialize
     * @return The object initialized (if an Hibernate object) or the object
     * @throws ModelException
     *             On force loading lazy collections failed
     */
    public static Object forceLoadLazyObject(final Object object) throws ModelException {
        if (object != null && HibernateProxy.class.isAssignableFrom(object.getClass())) {
            try {
                Hibernate.initialize(object);
            } catch (HibernateException e) {
                LOGGER.error("Cannot force hibernate loading", e);
                throw new ModelException(ERROR_MSG, e);
            }
        }
        return object;
    }

    /**
     * Try to force the loading of lazy collections
     * 
     * @param object
     *            the object to initialize
     * @return The function that will initialize the object
     * @throws ModelException
     *             On force loading lazy collections failed
     */
    public static FunctionThrowable<Object, Object, ModelException> forceLoadLazyObjectFunction(final Object object) throws ModelException {
        return (o -> forceLoadLazyObject(o));
    }
}
