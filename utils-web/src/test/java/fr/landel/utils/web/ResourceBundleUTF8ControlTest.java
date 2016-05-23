/*
 * #%L
 * OpenLib
 * %%
 * Copyright (C) 2015 Open Groupe
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.web;

import static org.junit.Assert.assertEquals;

import java.util.ResourceBundle;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Value;

/**
 * Check the resources bundle loader (in UTF-8)
 *
 * @since 11 déc. 2015
 * @author Gilles
 *
 */
public class ResourceBundleUTF8ControlTest extends AbstractTest {

    @Value("${utf8.prop1}")
    String prop1;

    /**
     * 
     * Constructor
     *
     */
    public ResourceBundleUTF8ControlTest() {
        super();
    }

    /**
     * Test method for
     * {@link ResourceBundleUTF8Control#newBundle(java.lang.String, java.util.Locale, java.lang.String, java.lang.ClassLoader, boolean)}
     * .
     */
    @Test
    public void testNewBundleStringLocaleStringClassLoaderBoolean() {
        ResourceBundle bundle = ResourceBundle.getBundle("test-utf8", new ResourceBundleUTF8Control());

        assertEquals("text accentu\u00e9", bundle.getString("utf8.prop1"));
    }

    /**
     * Test method for "utf8Properties" bean.
     */
    @Test
    public void testPropertiesFactoryBean() {
        assertEquals("text accentu\u00e9", this.prop1);
    }
}
