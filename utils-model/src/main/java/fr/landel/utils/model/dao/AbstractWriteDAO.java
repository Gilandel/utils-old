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
package fr.landel.utils.model.dao;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import javax.persistence.EntityExistsException;
import javax.persistence.TransactionRequiredException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.ImportResource;

import fr.landel.utils.mapper.ContextConstants;
import fr.landel.utils.mapper.MapperException;
import fr.landel.utils.mapper.utils.ReflectUtils;
import fr.landel.utils.model.AbstractEntity;
import fr.landel.utils.model.exception.ModelException;

/**
 * Abstract write DAO.
 * 
 * @see <a href="http://martinfowler.com/bliki/CQRS.html">Command Query
 *      Responsibility Segregation</a>
 *
 * @since 13 juil. 2015
 * @author Erwan Ropartz
 * @author Gilles Landel
 * @param <E>
 *            Class of the entity
 * @param <K>
 *            Class of the primary key
 */
@ImportResource(ContextConstants.CONTEXT_MAPPER)
public abstract class AbstractWriteDAO<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends AbstractDAO<E, K>
        implements WriteDAO<E, K> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -2629332819516383657L;

    private ConcurrentMap<Class<E>, Map<String, Field>> cache = new ConcurrentHashMap<>();

    @Autowired
    private ReflectUtils reflectionUtil;

    /**
     * Constructor
     *
     * @param entityClass
     *            The class of the entity
     */
    public AbstractWriteDAO(final Class<E> entityClass) {
        super(entityClass);
    }

    @Override
    public E create(final E entity) throws ModelException {
        try {
            this.getEntityManager().persist(entity);
            return entity;
        } catch (TransactionRequiredException | EntityExistsException | IllegalArgumentException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "persisting the entity"), e);
        }
    }

    @Override
    public E update(final E entity) throws ModelException {
        try {
            this.getEntityManager().merge(entity);
            return entity;
        } catch (TransactionRequiredException | IllegalArgumentException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "merging the entity"), e);
        }
    }

    @Override
    public E updateModification(final E entity) throws ModelException {

        try {
            final E oldEntity = this.getEntityManager().find(this.getEntityClass(), entity.getPrimaryKey());

            final Map<String, Field> fields;
            if (this.cache.containsKey(entity.getClass())) {
                fields = this.cache.get(entity.getClass());
            } else {
                fields = this.reflectionUtil.getAllFields(entity.getClass());
                this.cache.put(this.getEntityClass(), fields);
            }

            for (final Field field : fields.values()) {
                try {
                    final Object object = this.reflectionUtil.invokeGetter(field, entity, null);
                    if (object != null) {
                        this.reflectionUtil.invokeSetter(field, oldEntity, object);
                    }
                } catch (final MapperException e) {
                    // Next field
                }
            }
            return this.getEntityManager().merge(oldEntity);

        } catch (TransactionRequiredException | IllegalArgumentException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "merging the entity"), e);
        }
    }

    @Override
    public void delete(final E entity) throws ModelException {
        try {
            this.getEntityManager().remove(entity);
        } catch (TransactionRequiredException | IllegalArgumentException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "deleting the entity"), e);
        }
    }

    @Override
    public void delete(final K pk) throws ModelException {
        try {
            this.getEntityManager().remove(this.find(pk));
        } catch (TransactionRequiredException | IllegalArgumentException e) {
            throw new ModelException(String.format(ERROR_MESSAGE, "deleting the entity"), e);
        }
    }
}
