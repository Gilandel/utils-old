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
package fr.landel.utils.model.mappable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import fr.landel.utils.mapper.mappable.Mappable;

/**
 * Mappable test class 1.
 *
 * @since 30 nov. 2015
 * @author Gilles
 *
 */
@Mappable(Mappable2.class)
public class Mappable1 {

    private String firstname;
    private int age;
    private List<String> list;
    private Map<Integer, Boolean> map;

    /**
     * 
     * Constructor
     *
     */
    public Mappable1() {
        this.list = new ArrayList<>();
        this.map = new HashMap<>();
    }

    /**
     * @return the firstname
     */
    public String getFirstname() {
        return this.firstname;
    }

    /**
     * @param firstname
     *            the firstname to set
     */
    public void setFirstname(String firstname) {
        this.firstname = firstname;
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
