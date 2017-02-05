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
package fr.landel.utils.model.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.FilterType;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;

/**
 * Test service configuration
 *
 * @since Aug 12, 2016
 * @author Gilles
 *
 */
@Configuration
@ComponentScan(basePackages = "fr.landel.utils.model",
        excludeFilters = {@ComponentScan.Filter(pattern = "fr\\.landel\\.utils\\.model\\.mapper\\..*", type = FilterType.REGEX),
                @ComponentScan.Filter(pattern = "fr\\.landel\\.utils\\.model\\..*?DAO.*", type = FilterType.REGEX),
                @ComponentScan.Filter(value = AbstractPersistenceConfig.class, type = FilterType.ASSIGNABLE_TYPE)})
public class TestConfig {

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
