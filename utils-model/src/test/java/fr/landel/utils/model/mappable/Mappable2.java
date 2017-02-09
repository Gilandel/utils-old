/*
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
package fr.landel.utils.model.mappable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import fr.landel.utils.mapper.mappable.Mappable;
import fr.landel.utils.mapper.mappable.MappableProperty;

/**
 * Mappable test class 2.
 *
 * @since Nov 30, 2015
 * @author Gilles
 *
 */
@Mappable(Mappable1.class)
public class Mappable2 {

    @MappableProperty(value = "TEST", name = "firstname")
    private String name;

    @MappableProperty("TEST")
    private int age;

    @MappableProperty("TEST")
    private List<String> list;

    @MappableProperty("TEST")
    private Map<Integer, Boolean> map;

    /**
     * 
     * Constructor
     *
     */
    public Mappable2() {
        this.list = new ArrayList<>();
        this.map = new HashMap<>();
    }

    /**
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the age
     */
    public int getAge() {
        return this.age;
    }

    /**
     * @param age
     *            the age to set
     */
    public void setAge(int age) {
        this.age = age;
    }

    /**
     * @return the list
     */
    public List<String> getList() {
        return this.list;
    }

    /**
     * @param list
     *            the list to set
     */
    public void setList(List<String> list) {
        this.list = list;
    }

    /**
     * @return the map
     */
    public Map<Integer, Boolean> getMap() {
        return this.map;
    }

    /**
     * @param map
     *            the map to set
     */
    public void setMap(Map<Integer, Boolean> map) {
        this.map = map;
    }
}
