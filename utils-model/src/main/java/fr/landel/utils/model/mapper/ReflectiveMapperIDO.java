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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.springframework.stereotype.Component;

import fr.landel.utils.asserts.AssertUtils;
import fr.landel.utils.mapper.DTOIdentifier;
import fr.landel.utils.mapper.EnumMode;
import fr.landel.utils.mapper.MapperException;
import fr.landel.utils.mapper.core.AbstractReflectiveMapper;
import fr.landel.utils.model.AbstractDTO;
import fr.landel.utils.model.AbstractEntity;

/**
 * Reflective mapper (entity &lt;-&gt; DTO)
 *
 * @since 29 juil. 2015
 * @author Gilles
 *
 */
@Component
public class ReflectiveMapperIDO extends AbstractReflectiveMapper {

    private static final String ERROR_PARAMETER_MISSING = "%s, The parameter %s is null";
    private static final String ERROR_PARAMETER_EMPTY = "%s, The parameter %s is empty";
    private static final String ERROR_IDENTIFIER_UNKNOWN = "%s, The identifier %s is unknown";
    private static final String ERROR_PARAMETERS_LENGTH = "%s, the parameters %s and %s haven't the same length";

    private static final String METHOD_MAP_TO_DTO = "mapToDTO";
    private static final String METHOD_MAP_TO_ENTITY = "mapToEntity";

    private static final String PARAM_DTO = "dto";
    private static final String PARAM_DTO_LIST = "dtoList";
    private static final String PARAM_ENTITY = "entity";
    private static final String PARAM_ENTITIES = "entities";
    private static final String PARAM_IDENTIIER = "identifier";
    private static final String PARAM_PREVIOUS_DTO = "previousDTO";
    private static final String PARAM_PREVIOUS_DTO_LIST = "previousDTOList";

    /**
     * Transform entity to DTO.
     *
     * @param entity
     *            The entity
     * @param identifierKey
     *            The identifier key
     * @param <D>
     *            The DTO type
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return The dto
     * @throws MapperException
     *             If mapping failed
     * @throws IllegalArgumentException
     *             If parameters are not correctly set
     */
    public <E extends AbstractEntity<E, K>, D extends AbstractDTO<D, K>, K extends Serializable & Comparable<K>> D mapToDTO(final E entity,
            final String identifierKey) throws MapperException, IllegalArgumentException {
        AssertUtils.check(entity).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_ENTITY);
        AssertUtils.check(identifierKey).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_IDENTIIER);
        AssertUtils.check(this.getDtoIdentifierManager().containsKey(identifierKey)).isTrue(ERROR_IDENTIFIER_UNKNOWN, METHOD_MAP_TO_DTO,
                PARAM_IDENTIIER);

        final DTOIdentifier identifier = this.getDtoIdentifierManager().get(identifierKey);

        return this.map(entity, null, identifier, identifier.deep(), EnumMode.LOAD);
    }

    /**
     * Transform entity to DTO.
     *
     * @param entity
     *            The entity
     * @param identifier
     *            The identifier
     * @param <D>
     *            The DTO type
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return The dto
     * @throws MapperException
     *             If mapping failed
     * @throws IllegalArgumentException
     *             If parameters are not correctly set
     */
    public <E extends AbstractEntity<E, K>, D extends AbstractDTO<D, K>, K extends Serializable & Comparable<K>> D mapToDTO(final E entity,
            final DTOIdentifier identifier) throws MapperException, IllegalArgumentException {
        AssertUtils.check(entity).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_ENTITY);
        AssertUtils.check(identifier).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_IDENTIIER);

        return this.map(entity, null, identifier, identifier.deep(), EnumMode.LOAD);
    }

    /**
     * Transform entity to DTO.
     *
     * @param entity
     *            The entity
     * @param previousDTO
     *            The previous DTO
     * @param identifierKey
     *            The identifier key
     * @param <D>
     *            The DTO type
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return The DTO
     * @throws MapperException
     *             If mapping failed
     * @throws IllegalArgumentException
     *             If parameters are not correctly set
     */
    public <E extends AbstractEntity<E, K>, D extends AbstractDTO<D, K>, K extends Serializable & Comparable<K>> D mapToDTO(final E entity,
            final D previousDTO, final String identifierKey) throws MapperException, IllegalArgumentException {
        AssertUtils.check(entity).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_ENTITY);
        AssertUtils.check(previousDTO).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_PREVIOUS_DTO);
        AssertUtils.check(identifierKey).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_IDENTIIER);
        AssertUtils.check(this.getDtoIdentifierManager().containsKey(identifierKey)).isTrue(ERROR_IDENTIFIER_UNKNOWN, METHOD_MAP_TO_DTO,
                PARAM_IDENTIIER);

        final DTOIdentifier identifier = this.getDtoIdentifierManager().get(identifierKey);

        return this.map(entity, previousDTO, identifier, identifier.deep(), EnumMode.LOAD);
    }

    /**
     * Transform entity to DTO.
     *
     * @param entity
     *            The entity
     * @param previousDTO
     *            The previous DTO
     * @param identifier
     *            The identifier
     * @param <D>
     *            The DTO type
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return The dto
     * @throws MapperException
     *             If mapping failed
     * @throws IllegalArgumentException
     *             If parameters are not correctly set
     */
    public <E extends AbstractEntity<E, K>, D extends AbstractDTO<D, K>, K extends Serializable & Comparable<K>> D mapToDTO(final E entity,
            final D previousDTO, final DTOIdentifier identifier) throws MapperException, IllegalArgumentException {
        AssertUtils.check(entity).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_ENTITY);
        AssertUtils.check(previousDTO).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_PREVIOUS_DTO);
        AssertUtils.check(identifier).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_IDENTIIER);

        return this.map(entity, previousDTO, identifier, identifier.deep(), EnumMode.LOAD);
    }

    /**
     * Transform entities to a list of DTO.
     *
     * @param entities
     *            The entities
     * @param identifierKey
     *            The identifier key
     * @param <D>
     *            The DTO type
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return The list of dto
     * @throws MapperException
     *             If mapping failed
     * @throws IllegalArgumentException
     *             If parameters are not correctly set
     */
    public <E extends AbstractEntity<E, K>, D extends AbstractDTO<D, K>, K extends Serializable & Comparable<K>> List<D> mapToDTO(
            final List<E> entities, final String identifierKey) throws MapperException, IllegalArgumentException {
        AssertUtils.check(identifierKey).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_IDENTIIER);
        AssertUtils.check(this.getDtoIdentifierManager().containsKey(identifierKey)).isTrue(ERROR_IDENTIFIER_UNKNOWN, METHOD_MAP_TO_DTO,
                PARAM_IDENTIIER);

        final DTOIdentifier identifier = this.getDtoIdentifierManager().get(identifierKey);

        return this.mapToDTO(entities, identifier);
    }

    /**
     * Transform entities to a list of DTO.
     *
     * @param entities
     *            The entities
     * @param identifier
     *            The identifier
     * @param <D>
     *            The DTO type
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return The list of dto
     * @throws MapperException
     *             If mapping failed
     * @throws IllegalArgumentException
     *             If parameters are not correctly set
     */
    public <E extends AbstractEntity<E, K>, D extends AbstractDTO<D, K>, K extends Serializable & Comparable<K>> List<D> mapToDTO(
            final List<E> entities, final DTOIdentifier identifier) throws MapperException, IllegalArgumentException {
        AssertUtils.check(entities).isNotEmpty(ERROR_PARAMETER_EMPTY, METHOD_MAP_TO_DTO, PARAM_ENTITIES);
        AssertUtils.check(identifier).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_IDENTIIER);

        final List<D> dtos = new ArrayList<>();
        for (E entity : entities) {
            AssertUtils.check(entity).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_ENTITY);

            dtos.add(this.map(entity, (D) null, identifier, identifier.deep(), EnumMode.LOAD));
        }
        return dtos;
    }

    /**
     * Transform entities into a list of DTO.
     *
     * @param entities
     *            The entities
     * @param previousDTOList
     *            The previous DTO list
     * @param identifierKey
     *            The identifier key
     * @param <D>
     *            The DTO type
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return The DTO
     * @throws MapperException
     *             If mapping failed
     * @throws IllegalArgumentException
     *             If parameters are not correctly set
     */
    public <E extends AbstractEntity<E, K>, D extends AbstractDTO<D, K>, K extends Serializable & Comparable<K>> List<D> mapToDTO(
            final List<E> entities, final List<D> previousDTOList, final String identifierKey)
            throws MapperException, IllegalArgumentException {
        AssertUtils.check(identifierKey).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_IDENTIIER);
        AssertUtils.check(this.getDtoIdentifierManager().containsKey(identifierKey)).isTrue(ERROR_IDENTIFIER_UNKNOWN, METHOD_MAP_TO_DTO,
                PARAM_IDENTIIER);

        final DTOIdentifier identifier = this.getDtoIdentifierManager().get(identifierKey);

        return this.mapToDTO(entities, previousDTOList, identifier);
    }

    /**
     * Transform entities to a list of DTO.
     *
     * @param entities
     *            The entities
     * @param previousDTOList
     *            The previous DTO list
     * @param identifier
     *            The identifier
     * @param <D>
     *            The DTO type
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return The DTO
     * @throws MapperException
     *             If mapping failed
     * @throws IllegalArgumentException
     *             If parameters are not correctly set
     */
    public <E extends AbstractEntity<E, K>, D extends AbstractDTO<D, K>, K extends Serializable & Comparable<K>> List<D> mapToDTO(
            final List<E> entities, final List<D> previousDTOList, final DTOIdentifier identifier)
            throws MapperException, IllegalArgumentException {
        AssertUtils.check(entities).isNotEmpty(ERROR_PARAMETER_EMPTY, METHOD_MAP_TO_DTO, PARAM_ENTITIES);
        AssertUtils.check(previousDTOList).isNotEmpty(ERROR_PARAMETER_EMPTY, METHOD_MAP_TO_DTO, PARAM_PREVIOUS_DTO_LIST);
        AssertUtils.check(entities).hasSize(previousDTOList.size(), ERROR_PARAMETERS_LENGTH, METHOD_MAP_TO_DTO, PARAM_ENTITIES,
                PARAM_PREVIOUS_DTO_LIST);

        final List<D> dtos = new ArrayList<>();
        final Iterator<E> itEntity = entities.iterator();
        final Iterator<D> itDTO = previousDTOList.iterator();
        while (itEntity.hasNext() && itDTO.hasNext()) {
            final E entity = itEntity.next();
            final D previousDTO = itDTO.next();

            AssertUtils.check(entity).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_ENTITY);
            AssertUtils.check(previousDTO).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_PREVIOUS_DTO);

            dtos.add(this.map(entity, previousDTO, identifier, identifier.deep(), EnumMode.LOAD));
        }
        return dtos;
    }

    /**
     * Map DTO to entity.
     *
     * @param dto
     *            the DTO
     * @param <D>
     *            The DTO type
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return the entity
     * @throws MapperException
     *             If mapping failed
     * @throws IllegalArgumentException
     *             If parameters are not correctly set
     */
    public <E extends AbstractEntity<E, K>, D extends AbstractDTO<D, K>, K extends Serializable & Comparable<K>> E mapToEntity(final D dto)
            throws MapperException, IllegalArgumentException {
        AssertUtils.check(dto).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_ENTITY, PARAM_DTO);

        return this.map(dto, null, null, DTOIdentifier.MAX_DEEP, EnumMode.SAVE);
    }

    /**
     * Map DTO to entity.
     *
     * @param dto
     *            the DTO
     * @param entity
     *            The destination entity
     * @param identifierKey
     *            The identifier key to load
     * @param <D>
     *            The DTO type
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return the entity
     * @throws MapperException
     *             If mapping failed
     * @throws IllegalArgumentException
     *             If parameters are not correctly set
     */
    public <E extends AbstractEntity<E, K>, D extends AbstractDTO<D, K>, K extends Serializable & Comparable<K>> E mapToEntity(final D dto,
            final E entity, final String identifierKey) throws MapperException, IllegalArgumentException {
        AssertUtils.check(dto).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_ENTITY, PARAM_DTO);
        AssertUtils.check(identifierKey).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_ENTITY, PARAM_IDENTIIER);
        AssertUtils.check(this.getDtoIdentifierManager().containsKey(identifierKey)).isTrue(ERROR_IDENTIFIER_UNKNOWN, METHOD_MAP_TO_ENTITY,
                PARAM_IDENTIIER);

        final DTOIdentifier identifier = this.getDtoIdentifierManager().get(identifierKey);

        return this.map(dto, entity, identifier, identifier.deep(), EnumMode.SAVE);
    }

    /**
     * Map DTO to entity.
     *
     * @param dto
     *            the DTO
     * @param entity
     *            The destination entity
     * @param identifier
     *            The identifier to load
     * @param <D>
     *            The DTO type
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return the entity
     * @throws MapperException
     *             If mapping failed
     * @throws IllegalArgumentException
     *             If parameters are not correctly set
     */
    public <E extends AbstractEntity<E, K>, D extends AbstractDTO<D, K>, K extends Serializable & Comparable<K>> E mapToEntity(final D dto,
            final E entity, final DTOIdentifier identifier) throws MapperException, IllegalArgumentException {
        AssertUtils.check(dto).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_ENTITY, PARAM_DTO);
        AssertUtils.check(identifier).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_ENTITY, PARAM_IDENTIIER);

        return this.map(dto, entity, identifier, identifier.deep(), EnumMode.SAVE);
    }

    /**
     * Map a DTO list into entities.
     *
     * @param dtoList
     *            the DTO list
     * @param <D>
     *            The DTO type
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return the entities
     * @throws MapperException
     *             If mapping failed
     * @throws IllegalArgumentException
     *             If parameters are not correctly set
     */
    public <E extends AbstractEntity<E, K>, D extends AbstractDTO<D, K>, K extends Serializable & Comparable<K>> List<E> mapToEntity(
            final List<D> dtoList) throws MapperException, IllegalArgumentException {
        AssertUtils.check(dtoList).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_ENTITY, PARAM_DTO_LIST)
                .isNotEmpty(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_ENTITY, PARAM_DTO_LIST);

        final List<E> entities = new ArrayList<>();
        for (D dto : dtoList) {
            AssertUtils.check(dto).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_ENTITY, PARAM_DTO);

            entities.add(this.map(dto, (E) null, null, DTOIdentifier.MAX_DEEP, EnumMode.SAVE));
        }
        return entities;
    }

    /**
     * Map a DTO list into entities.
     *
     * @param dtoList
     *            the DTO list
     * @param entities
     *            The destination entities
     * @param identifierKey
     *            The identifier key to load
     * @param <D>
     *            The DTO type
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return the entity
     * @throws MapperException
     *             If mapping failed
     * @throws IllegalArgumentException
     *             If parameters are not correctly set
     */
    public <E extends AbstractEntity<E, K>, D extends AbstractDTO<D, K>, K extends Serializable & Comparable<K>> List<E> mapToEntity(
            final List<D> dtoList, final List<E> entities, final String identifierKey) throws MapperException, IllegalArgumentException {
        AssertUtils.check(identifierKey).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_ENTITY, PARAM_IDENTIIER);
        AssertUtils.check(this.getDtoIdentifierManager().containsKey(identifierKey)).isTrue(ERROR_IDENTIFIER_UNKNOWN, METHOD_MAP_TO_ENTITY,
                PARAM_IDENTIIER);

        final DTOIdentifier identifier = this.getDtoIdentifierManager().get(identifierKey);

        return this.mapToEntity(dtoList, entities, identifier);
    }

    /**
     * Map a DTO list into entities.
     *
     * @param dtoList
     *            the DTO list
     * @param entities
     *            The destination entities
     * @param identifier
     *            The identifier to load
     * @param <D>
     *            The DTO type
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     * @return the entity
     * @throws MapperException
     *             If mapping failed
     * @throws IllegalArgumentException
     *             If parameters are not correctly set
     */
    public <E extends AbstractEntity<E, K>, D extends AbstractDTO<D, K>, K extends Serializable & Comparable<K>> List<E> mapToEntity(
            final List<D> dtoList, final List<E> entities, final DTOIdentifier identifier)
            throws MapperException, IllegalArgumentException {
        AssertUtils.check(dtoList).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_ENTITY, PARAM_DTO_LIST);
        AssertUtils.check(entities).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_ENTITY, PARAM_ENTITIES);
        AssertUtils.check(dtoList).hasSize(entities.size(), ERROR_PARAMETERS_LENGTH, METHOD_MAP_TO_ENTITY, PARAM_DTO_LIST, PARAM_ENTITIES);
        AssertUtils.check(identifier).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_ENTITY, PARAM_IDENTIIER);

        final List<E> entitiesOut = new ArrayList<>();
        final Iterator<E> itEntity = entities.iterator();
        final Iterator<D> itDTO = dtoList.iterator();
        while (itEntity.hasNext() && itDTO.hasNext()) {
            final E entity = itEntity.next();
            final D dto = itDTO.next();

            AssertUtils.check(entity).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_ENTITY);
            AssertUtils.check(dto).isNotNull(ERROR_PARAMETER_MISSING, METHOD_MAP_TO_DTO, PARAM_PREVIOUS_DTO);

            entitiesOut.add(this.map(dto, entity, identifier, identifier.deep(), EnumMode.SAVE));
        }
        return entitiesOut;
    }

    /**
     * Prepare the mapping of the entity into DTO
     * 
     * @param entity
     *            The entity
     * @param dto
     *            the DTO
     * @param identifier
     *            The identifier
     * @param deep
     *            The current deep
     * @param mode
     *            The current mapping mode
     * @param <D>
     *            The DTO type
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     */
    protected <E extends AbstractEntity<E, K>, D extends AbstractDTO<D, K>, K extends Serializable & Comparable<K>> void prepareEntityToDTO(
            E entity, D dto, DTOIdentifier identifier, int deep, EnumMode mode) {
        if (dto != null) {
            dto.setPrimaryKey(entity.getPrimaryKey());
            if (EnumMode.LOAD.equals(mode) || EnumMode.DEFAULT.equals(mode)) {
                dto.setLoaded(true);
            }
        }
    }

    /**
     * Prepare the mapping of the DTO into entity
     * 
     * @param entity
     *            The entity
     * @param dto
     *            the DTO
     * @param identifier
     *            The identifier
     * @param deep
     *            The current deep
     * @param mode
     *            The current mapping mode
     * @param <D>
     *            The DTO type
     * @param <E>
     *            The entity type
     * @param <K>
     *            The primary key type
     */
    protected <E extends AbstractEntity<E, K>, D extends AbstractDTO<D, K>, K extends Serializable & Comparable<K>> void prepareDTOToEntity(
            E entity, D dto, DTOIdentifier identifier, int deep, EnumMode mode) {
        if (entity != null) {
            entity.setPrimaryKey(dto.getPrimaryKey());
        }
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    @Override
    protected void prepareMapping(Object sourceObject, Object targetObject, DTOIdentifier identifier, int deep, EnumMode mode) {
        if (AbstractEntity.class.isAssignableFrom(sourceObject.getClass())) {
            this.prepareEntityToDTO((AbstractEntity) sourceObject, (AbstractDTO) targetObject, identifier, deep, mode);
        } else {
            this.prepareDTOToEntity((AbstractEntity) targetObject, (AbstractDTO) sourceObject, identifier, deep, mode);
        }
    }

    @Override
    protected void postMapping(Object sourceObject, Object targetObject, DTOIdentifier identifier, int deep, EnumMode mode) {
        // DO nothing
    }
}
