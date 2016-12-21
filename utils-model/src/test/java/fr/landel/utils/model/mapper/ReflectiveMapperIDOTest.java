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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.Set;
import java.util.TreeSet;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import fr.landel.utils.mapper.DTOIdentifier;
import fr.landel.utils.mapper.MapperException;
import fr.landel.utils.model.AbstractDTO;
import fr.landel.utils.model.AbstractEntity;
import fr.landel.utils.model.AbstractModelTest;
import fr.landel.utils.model.IDO;
import fr.landel.utils.model.mappable.CommonMethods;
import fr.landel.utils.model.mappable.DTOChild;
import fr.landel.utils.model.mappable.DTOParent;
import fr.landel.utils.model.mappable.EntityChild;
import fr.landel.utils.model.mappable.EntityParent;
import fr.landel.utils.model.mappable.EnumLocale;

/**
 * Reflective mapper tests.
 *
 * @since Nov 27, 2015
 * @author Gilles
 *
 */
@Ignore
public class ReflectiveMapperIDOTest extends AbstractModelTest {

    /**
     * The comparator use to sort children in set (to assertEquals purpose)
     */
    private static final Comparator<IDO<?, String>> COMPARATOR_CHILD = new Comparator<IDO<?, String>>() {
        @Override
        public int compare(IDO<?, String> o1, IDO<?, String> o2) {
            if (o1 != null && o2 != null && o1.getPrimaryKey() != null) {
                return o1.getPrimaryKey().compareTo(o2.getPrimaryKey());
            }
            return 1;
        }
    };

    @Autowired
    private ReflectiveMapperIDO reflectiveMapper;

    private EntityParent entityParent1;

    /**
     * Prepare tests
     */
    @Before
    public void prepare() {
        Date currentDate;
        Date tomorrowDate;

        // Prepare dates
        final Calendar calendar = Calendar.getInstance();

        currentDate = calendar.getTime();

        calendar.add(Calendar.DAY_OF_YEAR, 1);
        tomorrowDate = calendar.getTime();

        // Prepare entities
        this.entityParent1 = new EntityParent();

        final EntityChild child1 = new EntityChild();
        final EntityChild child11 = new EntityChild();
        final EntityChild child12 = new EntityChild();
        final EntityChild child21 = new EntityChild();
        final EntityChild child22 = new EntityChild();
        final EntityChild child23 = new EntityChild();

        this.entityParent1.setPk("parent1");

        this.entityParent1.setValByte((byte) 0);
        this.entityParent1.setValBoolean(Boolean.TRUE);
        this.entityParent1.setValCharacter('p');
        this.entityParent1.setValShort((short) 1);
        this.entityParent1.setValInteger(2);
        this.entityParent1.setValLong(3L);
        this.entityParent1.setValFloat(4.1f);
        this.entityParent1.setValDouble(5.2d);
        this.entityParent1.setName("parent 1 name");
        this.entityParent1.setDate(currentDate);
        this.entityParent1.setLocale(EnumLocale.fr);
        this.entityParent1.setOnlyInEntity(6L);
        this.entityParent1.setChild(child1);
        this.entityParent1.getChildren1().add(child11);
        this.entityParent1.getChildren1().add(child12);
        this.entityParent1.getChildren2().add(child21);
        this.entityParent1.getChildren2().add(child22);
        this.entityParent1.getChildren2().add(child23);

        this.setChild(child1, "child 1", (byte) 1, Boolean.TRUE, 'c', (short) 11, 12);
        this.setChild(child1, 13L, 14.1f, 15.2d, "child 1 name", currentDate, null);

        this.setChild(child11, "child 11", (byte) 11, Boolean.FALSE, 'd', (short) 111, 112);
        this.setChild(child11, 113L, 114.1f, 115.2d, "child 11 name", currentDate, EnumLocale.en);

        this.setChild(child12, "child 12", (byte) 12, Boolean.TRUE, 'e', (short) 121, 122);
        this.setChild(child12, 123L, 124.1f, 125.2d, "child 12 name", tomorrowDate, EnumLocale.en);

        this.setChild(child21, "child 21", (byte) 21, Boolean.FALSE, 'f', (short) 211, 212);
        this.setChild(child21, 213L, 214.1f, 215.2d, "child 21 name", currentDate, EnumLocale.fr);

        this.setChild(child22, "child 22", (byte) 22, Boolean.TRUE, 'g', (short) 221, 222);
        this.setChild(child22, 223L, 224.1f, 225.2d, "child 22 name", currentDate, EnumLocale.fr);

        this.setChild(child23, "child 23", (byte) 23, Boolean.FALSE, 'h', (short) 231, 232);
        this.setChild(child23, 234L, 235.1f, 236.2d, "child 23 name", tomorrowDate, EnumLocale.fr);
    }

    private void setChild(final EntityChild child, final String pk, final byte valByte, final Boolean valBoolean,
            final Character valCharacter, final Short valShort, final Integer valInteger) {

        child.setPk(pk);
        child.setValByte(valByte);
        child.setValBoolean(valBoolean);
        child.setValCharacter(valCharacter);
        child.setValShort(valShort);
        child.setValInteger(valInteger);
    }

    private void setChild(final EntityChild child, final Long valLong, final Float valFloat, final Double valDouble, final String name,
            final Date date, final EnumLocale locale) {
        child.setValLong(valLong);
        child.setValFloat(valFloat);
        child.setValDouble(valDouble);
        child.setName(name);
        child.setDate(date);
        child.setLocale(locale);
    }

    /**
     * Test method for
     * {@link ReflectiveMapperIDO#mapToDTO(AbstractEntity, DTOIdentifier)} .
     */
    @Test
    public void testMapToDTOEEnumDTOIdentifier() {
        try {
            DTOChild dtoChild;
            EntityChild entityChild;

            // Check properties only accessible in list mode (reading and 1 deep
            // analyze)

            DTOParent dtoParent = this.reflectiveMapper.mapToDTO(this.entityParent1, MyDTOIdentifier.PARENT_LIST);
            assertEquals(this.entityParent1.getValByte(), dtoParent.getValByte());
            assertNull(dtoParent.getValCharacter());

            // Check properties accessible in list/details mode (reading and 1
            // deep analyze)

            dtoParent = this.reflectiveMapper.mapToDTO(this.entityParent1, MyDTOIdentifier.PARENT_DETAILS);

            assertEquals(this.entityParent1.getValByte(), dtoParent.getValByte());
            assertEquals(this.entityParent1.getValBoolean(), dtoParent.getValBoolean());
            assertEquals(this.entityParent1.getValCharacter(), dtoParent.getValCharacter());
            assertEquals(this.entityParent1.getValShort(), dtoParent.getValShort());
            assertEquals(this.entityParent1.getValInteger(), dtoParent.getValInteger());
            assertEquals(this.entityParent1.getValLong(), dtoParent.getValLong());
            assertEquals(this.entityParent1.getValFloat(), dtoParent.getValFloat());
            assertEquals(this.entityParent1.getValDouble(), dtoParent.getValDouble());
            assertEquals(this.entityParent1.getName(), dtoParent.getRenamed());
            assertEquals(this.entityParent1.getDate(), dtoParent.getDate());
            assertEquals(this.entityParent1.getLocale(), dtoParent.getLocale());

            dtoChild = dtoParent.getChild();
            assertFalse(dtoChild.getLoaded());
            assertNull(dtoChild.getValByte());

            assertEquals(this.entityParent1.getChildren1().size(), dtoParent.getChildren1().size());
            dtoChild = dtoParent.getChildren1().iterator().next();
            assertFalse(dtoChild.getLoaded());
            assertNull(dtoChild.getValByte());

            assertEquals(this.entityParent1.getChildren2().size(), dtoParent.getChildren2().size());
            dtoChild = dtoParent.getChildren2().iterator().next();
            assertTrue(dtoChild.getLoaded());
            assertNull(dtoChild.getValByte());

            // Check properties accessible in list/details mode (reading and
            // full deep analyze)

            dtoParent = this.reflectiveMapper.mapToDTO(this.entityParent1, MyDTOIdentifier.PARENT_MAX);

            // All properties have to be set, even the valByte because the
            // COMMON_LIST identifier is included in the COMMON identifier
            this.compare(this.entityParent1, dtoParent);
            assertEquals(this.entityParent1.getName(), dtoParent.getRenamed());

            // Child uses COMMON identifier on valByte attribute but the
            // attribute is in preload in parent, so the child has to be empty
            // with only the PK set
            dtoChild = dtoParent.getChild();
            assertFalse(dtoChild.getLoaded());
            assertEquals(this.entityParent1.getChild().getPk(), dtoChild.getPrimaryKey());
            assertNull(dtoChild.getValByte());

            // Child uses COMMON identifier on valByte attribute but the
            // attribute is in preload in parent, so each child has to be empty
            // with only the PK set
            assertEquals(this.entityParent1.getChildren1().size(), dtoParent.getChildren1().size());

            // Sort the sets to compare
            final Set<EntityChild> sortedEntityChildren = new TreeSet<>(COMPARATOR_CHILD);
            sortedEntityChildren.addAll(this.entityParent1.getChildren1());
            final Set<DTOChild> sortedDTOChildren = new TreeSet<>(COMPARATOR_CHILD);
            sortedDTOChildren.addAll(dtoParent.getChildren1());

            dtoChild = sortedDTOChildren.iterator().next();
            entityChild = sortedEntityChildren.iterator().next();
            assertFalse(dtoChild.getLoaded());
            assertEquals(entityChild.getPrimaryKey(), dtoChild.getPrimaryKey());
            assertNull(dtoChild.getValByte());

            // Children has to be loaded, the property valByte must be set and
            // all others have to be null
            assertEquals(this.entityParent1.getChildren2().size(), dtoParent.getChildren2().size());
            dtoChild = dtoParent.getChildren2().iterator().next();
            entityChild = this.entityParent1.getChildren2().iterator().next();
            assertTrue(dtoChild.getLoaded());
            assertEquals(entityChild.getValByte(), dtoChild.getValByte());
            assertNull(dtoChild.getValBoolean());

        } catch (MapperException e) {
            fail("Errors occurred in ReflectiveMapperTest#testMapToDTOEEnumDTOIdentifier()\n" + e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link ReflectiveMapperIDO#mapToDTO(AbstractEntity, AbstractDTO, DTOIdentifier)}
     * .
     */
    @Test
    public void testMapToDTOEDEnumDTOIdentifier() {
        try {
            // Load the previous DTO

            final DTOParent dtoParentPrevious = this.reflectiveMapper.mapToDTO(this.entityParent1, MyDTOIdentifier.PARENT_LIST);

            dtoParentPrevious.setRenamed("parent 1 previous name");

            // Check if the name isn't reset by the mapper

            final DTOParent dtoParent = this.reflectiveMapper.mapToDTO(this.entityParent1, dtoParentPrevious,
                    MyDTOIdentifier.PARENT_DETAILS);

            assertEquals(this.entityParent1.getValByte(), dtoParent.getValByte());
            assertEquals(dtoParentPrevious.getRenamed(), dtoParent.getRenamed());

        } catch (MapperException e) {
            fail("Errors occurred in ReflectiveMapperTest#testMapToDTOEDEnumDTOIdentifier()\n" + e.getMessage());
        }
    }

    /**
     * Test method for {@link ReflectiveMapperIDO#mapToEntity(AbstractDTO)} .
     */
    @Test
    public void testMapToEntityD() {
        try {
            DTOChild dtoChild;
            EntityChild entityChild;

            // Load the DTO (Suppose, it's OK, others tests)

            final DTOParent dtoParent = this.reflectiveMapper.mapToDTO(this.entityParent1, MyDTOIdentifier.ALL);

            dtoParent.setRenamed("parent 1 reference name");

            // Load the entity from the dto, and check the result. The entity
            // has to be fully loaded, except the children1 (preload)

            final EntityParent entityParent = this.reflectiveMapper.mapToEntity(dtoParent);

            this.compare(dtoParent, entityParent);
            assertEquals(dtoParent.getRenamed(), entityParent.getName());

            this.compare(dtoParent.getChild(), entityParent.getChild());
            assertEquals(dtoParent.getChild().getRenamed(), entityParent.getChild().getName());

            assertEquals(dtoParent.getChildren1().size(), entityParent.getChildren1().size());

            // Sort the sets to compare
            final Set<EntityChild> sortedEntityChildren = new TreeSet<>(COMPARATOR_CHILD);
            sortedEntityChildren.addAll(entityParent.getChildren1());
            final Set<DTOChild> sortedDTOChildren = new TreeSet<>(COMPARATOR_CHILD);
            sortedDTOChildren.addAll(dtoParent.getChildren1());

            dtoChild = sortedDTOChildren.iterator().next();
            entityChild = sortedEntityChildren.iterator().next();
            assertEquals(dtoChild.getPrimaryKey(), entityChild.getPrimaryKey());
            assertNull(entityChild.getValByte());

            assertEquals(dtoParent.getChildren2().size(), entityParent.getChildren2().size());
            for (int i = 0; i < entityParent.getChildren2().size(); i++) {
                entityChild = entityParent.getChildren2().get(i);
                dtoChild = dtoParent.getChildren2().get(i);

                this.compare(dtoChild, entityChild);
                assertEquals(dtoChild.getRenamed(), entityChild.getName());
            }

        } catch (MapperException e) {
            fail("Errors occurred in ReflectiveMapperTest#testMapToEntityD()\n" + e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link ReflectiveMapperIDO#mapToEntity(AbstractDTO, AbstractEntity, DTOIdentifier)}
     * .
     */
    @Test
    public void testMapToEntityDEEnumDTOIdentifier() {
        try {
            // Load the DTO (Suppose, it's OK, others tests)

            final DTOParent dtoParent = this.reflectiveMapper.mapToDTO(this.entityParent1, MyDTOIdentifier.DETAILS);

            dtoParent.setRenamed("parent 1 reference name");
            dtoParent.setLocale(EnumLocale.en);
            dtoParent.getChild().setRenamed("new name");

            // Load the entity from the dto, and check the result.

            EntityParent entityParent = this.reflectiveMapper.mapToEntity(dtoParent, null, MyDTOIdentifier.PARENT_SAVE_DETAILS);

            assertEquals(dtoParent.getValBoolean(), entityParent.getValBoolean());
            assertNull(entityParent.getValByte());
            assertNull(entityParent.getValCharacter());
            assertNull(entityParent.getValShort());
            assertNull(entityParent.getValInteger());
            assertNull(entityParent.getValLong());
            assertNull(entityParent.getValFloat());
            assertNull(entityParent.getValDouble());
            assertNull(entityParent.getName());
            assertNull(entityParent.getDate());
            assertEquals(dtoParent.getLocale(), entityParent.getLocale());

            // Children 1 isn't updated, so it's empty
            assertEquals(dtoParent.getChild().getRenamed(), entityParent.getChild().getName());
            assertTrue(entityParent.getChildren1().isEmpty());
            assertEquals(dtoParent.getChildren2().size(), entityParent.getChildren2().size());

            entityParent = this.reflectiveMapper.mapToEntity(dtoParent, null, MyDTOIdentifier.PARENT_SAVE_LIST);

            // Child Children 1 isn't updated, so it's empty
            assertNull(entityParent.getChild().getName());
            assertTrue(entityParent.getChildren1().isEmpty());
            assertEquals(dtoParent.getChildren2().size(), entityParent.getChildren2().size());

            // Load the entity from the dto in update mode, and check the
            // result.

            entityParent = this.reflectiveMapper.mapToEntity(dtoParent, this.entityParent1, MyDTOIdentifier.PARENT_SAVE_DETAILS);

            assertEquals(dtoParent.getValBoolean(), entityParent.getValBoolean());

            assertEquals(this.entityParent1.getValByte(), entityParent.getValByte());
            assertEquals(this.entityParent1.getName(), entityParent.getName());
            assertEquals(dtoParent.getLocale(), entityParent.getLocale());

            // Children 1 isn't updated, but it's already in entityParent1
            assertEquals(dtoParent.getChild().getRenamed(), entityParent.getChild().getName());
            assertEquals(dtoParent.getChildren1().size(), entityParent.getChildren1().size());
            assertEquals(dtoParent.getChildren2().size(), entityParent.getChildren2().size());

        } catch (MapperException e) {
            fail("Errors occurred in ReflectiveMapperTest#testMapToEntityDEEnumDTOIdentifier()\n" + e.getMessage());
        }
    }

    private void compare(final CommonMethods entity, final CommonMethods dto) {
        assertEquals(entity.getValByte(), dto.getValByte());
        assertEquals(entity.getValBoolean(), dto.getValBoolean());
        assertEquals(entity.getValCharacter(), dto.getValCharacter());
        assertEquals(entity.getValShort(), dto.getValShort());
        assertEquals(entity.getValInteger(), dto.getValInteger());
        assertEquals(entity.getValLong(), dto.getValLong());
        assertEquals(entity.getValFloat(), dto.getValFloat());
        assertEquals(entity.getValDouble(), dto.getValDouble());
        assertEquals(entity.getDate(), dto.getDate());
        assertEquals(entity.getLocale(), dto.getLocale());
    }
}
