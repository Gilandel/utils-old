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

import java.util.regex.Pattern;

import org.hibernate.boot.model.naming.Identifier;
import org.hibernate.boot.model.naming.PhysicalNamingStrategy;
import org.hibernate.engine.jdbc.env.spi.JdbcEnvironment;

import fr.landel.utils.commons.StringUtils;

/**
 * Improve naming strategy for JPA 2.1 and Hibernate 5
 *
 * @since 25 nov. 2015
 * @author Gilles
 *
 */
public class ImprovedNamingStrategy implements PhysicalNamingStrategy {

    private static final Pattern CONVERTER_PATTERN = Pattern.compile("([a-z])([A-Z])");
    private static final String CONVERTER_REPLACEMENT = "$1_$2";

    @Override
    public Identifier toPhysicalCatalogName(final Identifier identifier, final JdbcEnvironment jdbcEnv) {
        return this.convert(identifier);
    }

    @Override
    public Identifier toPhysicalColumnName(final Identifier identifier, final JdbcEnvironment jdbcEnv) {
        return this.convert(identifier);
    }

    @Override
    public Identifier toPhysicalSchemaName(final Identifier identifier, final JdbcEnvironment jdbcEnv) {
        return this.convert(identifier);
    }

    @Override
    public Identifier toPhysicalSequenceName(final Identifier identifier, final JdbcEnvironment jdbcEnv) {
        return this.convert(identifier);
    }

    @Override
    public Identifier toPhysicalTableName(final Identifier identifier, final JdbcEnvironment jdbcEnv) {
        return this.convert(identifier);
    }

    private Identifier convert(final Identifier identifier) {
        if (identifier == null || StringUtils.isBlank(identifier.getText())) {
            return identifier;
        }

        String newName = CONVERTER_PATTERN.matcher(identifier.getText()).replaceAll(CONVERTER_REPLACEMENT).toLowerCase();
        return Identifier.toIdentifier(newName);
    }
}
