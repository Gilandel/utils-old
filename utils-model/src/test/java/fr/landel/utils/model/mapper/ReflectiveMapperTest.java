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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

import org.hamcrest.Matchers;
import org.junit.Ignore;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import fr.landel.utils.mapper.DTOIdentifier;
import fr.landel.utils.mapper.DTOIdentifierImpl;
import fr.landel.utils.mapper.EnumMode;
import fr.landel.utils.mapper.MapperException;
import fr.landel.utils.mapper.core.DTOIdentifierManager;
import fr.landel.utils.mapper.core.ReflectiveMapper;
import fr.landel.utils.model.AbstractModelTest;
import fr.landel.utils.model.mappable.Mappable1;
import fr.landel.utils.model.mappable.Mappable2;

/**
 * Reflective mapper tests.
 *
 * @since Nov 27, 2015
 * @author Gilles
 *
 */
@Ignore
public class ReflectiveMapperTest extends AbstractModelTest {

    @Autowired
    private ReflectiveMapper reflectiveMapper;

    @Autowired
    private DTOIdentifierManager dtoIdentifierManager;

    /**
     * Check a simple mapping
     * 
     * @throws MapperException
     *             On mapping exception
     * @throws IllegalArgumentException
     *             On illegal arguments
     */
    @Test
    public void simpleMappingTest() throws MapperException, IllegalArgumentException {
        final Mappable1 mappable1 = new Mappable1();

        final String firstname = "John";
        final int age = 30;

        mappable1.setFirstname(firstname);
        mappable1.setAge(age);
        mappable1.getList().add("Earth");
        mappable1.getList().add("Moon");
        mappable1.getMap().put(0, true);
        mappable1.getMap().put(1, false);

        DTOIdentifier dtoIdentifier = new DTOIdentifierImpl("TEST", 1);

        this.dtoIdentifierManager.addIdentifier(dtoIdentifier);

        Mappable2 mappable2 = this.reflectiveMapper.map(mappable1, null, dtoIdentifier, 1, EnumMode.LOAD);

        assertNotNull(mappable2);
        assertEquals(mappable1.getFirstname(), mappable2.getName());
        assertEquals(mappable1.getAge(), mappable2.getAge());
        assertThat(mappable2.getList(), Matchers.hasSize(mappable1.getList().size()));
        assertThat(mappable2.getList(), Matchers.contains("Earth", "Moon"));
        assertThat(mappable2.getMap().keySet(), Matchers.hasSize(mappable1.getMap().size()));
        assertThat(mappable2.getMap(), Matchers.allOf(Matchers.hasEntry(0, true), Matchers.hasEntry(1, false)));
    }
}
