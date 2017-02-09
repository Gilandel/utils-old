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
package fr.landel.utils.model.hibernate;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;
import java.util.Set;

import org.hibernate.Hibernate;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestExecutionListeners;
import org.springframework.test.context.junit4.AbstractTransactionalJUnit4SpringContextTests;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.support.AnnotationConfigContextLoader;
import org.springframework.test.context.support.DependencyInjectionTestExecutionListener;
import org.springframework.test.context.support.DirtiesContextTestExecutionListener;
import org.springframework.transaction.annotation.Transactional;

import fr.landel.utils.commons.function.SupplierThrowable;
import fr.landel.utils.model.ReadDAOParent;
import fr.landel.utils.model.WriteDAOChild;
import fr.landel.utils.model.WriteDAOParent;
import fr.landel.utils.model.config.TestPersistenceConfig;
import fr.landel.utils.model.exception.ModelException;
import fr.landel.utils.model.mappable.EntityChild;
import fr.landel.utils.model.mappable.EntityParent;

/**
 * Check {@link HibernateUtils}
 *
 * @since Aug 12, 2016
 * @author Gilles
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(loader = AnnotationConfigContextLoader.class, classes = TestPersistenceConfig.class)
@TestExecutionListeners({DependencyInjectionTestExecutionListener.class, DirtiesContextTestExecutionListener.class})
public class HibernateUtilsTest extends AbstractTransactionalJUnit4SpringContextTests {

    @Autowired
    private WriteDAOChild writeDAOchild;

    @Autowired
    private WriteDAOParent writeDAOParent;

    @Autowired
    private ReadDAOParent readDAOParent;

    /**
     * Test method for
     * {@link HibernateUtils#forceLoadLazyObjectFunction(java.lang.Object)}.
     * 
     * @throws ModelException
     *             on error
     */
    @Test
    @Transactional
    public void testForceLoadLazyObjectFunction() throws ModelException {

        // create a test object
        final EntityParent newParent = this.writeDAOParent.create(new EntityParent());
        assertNotNull(newParent);
        final EntityChild newChild1 = this.writeDAOchild.create(new EntityChild());
        assertNotNull(newChild1);
        newParent.setChild(newChild1);
        EntityChild newChild2 = new EntityChild();
        newChild2.setParent1(newParent);
        newChild2 = this.writeDAOchild.create(newChild2);
        assertNotNull(newChild2);

        this.writeDAOParent.flushAndClear();

        // read the tables to find the first result
        final List<EntityParent> parents = this.readDAOParent.findAll();
        assertNotNull(parents);
        assertEquals(1, parents.size());
        EntityParent entity = parents.get(0);

        assertTrue(Hibernate.isInitialized(entity));
        assertTrue(Hibernate.isInitialized(entity.getChild()));
        assertFalse(Hibernate.isInitialized(entity.getChildren1()));

        // create the supplier
        final SupplierThrowable<Set<EntityChild>, ModelException> supplier = HibernateUtils
                .forceLoadLazyObjectFunction(entity.getChildren1());

        assertNotNull(supplier);

        // execute the supplier
        final Set<EntityChild> initChild = supplier.get();

        assertNotNull(initChild);

        assertTrue(Hibernate.isInitialized(entity));
        assertTrue(Hibernate.isInitialized(entity.getChild()));
        assertTrue(Hibernate.isInitialized(entity.getChildren1()));

        assertEquals(1, entity.getChildren1().size());

    }
}
