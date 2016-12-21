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
package fr.landel.utils.model.config;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.FilterType;
import org.springframework.context.annotation.PropertySource;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import org.springframework.core.env.Environment;
import org.springframework.transaction.annotation.EnableTransactionManagement;

/**
 * Persistence config test
 *
 * @since Aug 12, 2016
 * @author Gilles
 *
 */
@Configuration
@EnableTransactionManagement
@PropertySource({"persistence_test.properties"})
@ComponentScan(basePackages = "fr.landel.utils.model",
        excludeFilters = @ComponentScan.Filter(pattern = "fr\\.landel\\.utils\\.model\\.mapper\\..*", type = FilterType.REGEX))
public class TestPersistenceConfig extends AbstractPersistenceConfig {

    @Autowired
    private Environment env;

    /**
     * Initialize the persistence configuration
     */
    @PostConstruct
    protected void init() {
        packagesToScan = this.env.getProperty(PACKAGES_MODEL_TO_SCAN);
    }

    /**
     * To explain to Spring how to handle ${}
     * 
     * @return The property source configurer
     */
    @Bean
    public static PropertySourcesPlaceholderConfigurer propertyConfigurer() {
        return new PropertySourcesPlaceholderConfigurer();
    }
}
