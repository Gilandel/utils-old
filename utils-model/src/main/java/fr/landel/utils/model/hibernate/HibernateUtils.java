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

import fr.landel.utils.commons.function.SupplierThrowable;
import fr.landel.utils.model.exception.ModelException;

/**
 * Hibernate utility class to force object loading
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public class HibernateUtils {
    private static final String ERROR_MSG = "Cannot force the initialization of the object";

    /**
     * Try to force the loading of lazy collections
     * 
     * @param object
     *            the object to initialize
     * @param <T>
     *            the object type
     * @return The object initialized (if an Hibernate object) or the object
     * @throws ModelException
     *             On force loading lazy collections failed
     */
    public static <T> T forceLoadLazyObject(final T object) throws ModelException {
        try {
            Hibernate.initialize(object);
        } catch (HibernateException e) {
            throw new ModelException(ERROR_MSG, e);
        }
        return object;
    }

    /**
     * Try to force the loading of lazy collections
     * 
     * @param object
     *            the object to initialize
     * @return The function that will initialize the object
     * @param <T>
     *            the object type
     * @throws ModelException
     *             On force loading lazy collections failed
     */
    public static <T> SupplierThrowable<T, ModelException> forceLoadLazyObjectFunction(final T object) throws ModelException {
        return () -> forceLoadLazyObject(object);
    }
}
