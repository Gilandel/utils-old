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

import org.hibernate.boot.model.naming.Identifier;
import org.junit.Assert;
import org.junit.Test;

import fr.landel.utils.model.hibernate.ImprovedNamingStrategy;

/**
 * Check ImprovedNamingStrategy
 *
 * @since 7 janv. 2016
 * @author Gilles
 *
 */
public class ImprovedNamingStrategyTest {

    /**
     * 
     * Constructor
     *
     */
    public ImprovedNamingStrategyTest() {
    }

    /**
     * Test method for
     * {@link ImprovedNamingStrategy#toPhysicalTableName(org.hibernate.boot.model.naming.Identifier, org.hibernate.engine.jdbc.env.spi.JdbcEnvironment)}
     * .
     */
    @Test
    public void testToPhysicalTableName() {
        ImprovedNamingStrategy ins = new ImprovedNamingStrategy();

        Identifier identifier = ins.toPhysicalTableName(Identifier.toIdentifier("testIDENTIFIER"), null);

        Assert.assertEquals("test_identifier", identifier.getText());
    }
}
