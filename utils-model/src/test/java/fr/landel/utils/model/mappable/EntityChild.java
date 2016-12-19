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

import java.util.Date;
import java.util.Map;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;

import org.hibernate.annotations.GenericGenerator;

import fr.landel.utils.mapper.mappable.Mappable;
import fr.landel.utils.model.AbstractEntity;

/**
 * Entity child to test mapper.
 *
 * @since 27 nov. 2015
 * @author Gilles
 *
 */
@Entity
@Mappable(value = DTOChild.class)
public class EntityChild extends AbstractEntity<EntityChild, String> implements CommonMethods {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 3596881374018184721L;

    @Id
    @GeneratedValue(generator = "uuid2")
    @GenericGenerator(name = "uuid2", strategy = "uuid2")
    @Column(name = "id", length = 50)
    private String pk;

    @Column(name = "val_byte")
    private Byte valByte;

    @Column(name = "val_boolean")
    private Boolean valBoolean;

    @Column(name = "val_character")
    private Character valCharacter;

    @Column(name = "val_short")
    private Short valShort;

    @Column(name = "val_integer")
    private Integer valInteger;

    @Column(name = "val_long")
    private Long valLong;

    @Column(name = "val_float")
    private Float valFloat;

    @Column(name = "val_double")
    private Double valDouble;

    @Column(name = "name")
    private String name;

    @Column(name = "date_")
    private Date date;

    @Column(name = "locale")
    @Enumerated(EnumType.STRING)
    private EnumLocale locale;

    @ManyToOne
    @JoinColumn(name = "fk_parent1")
    private EntityParent parent1;

    @ManyToOne
    @JoinColumn(name = "fk_parent2")
    private EntityParent parent2;

    /**
     * 
     * Constructor
     *
     */
    public EntityChild() {
        super(EntityChild.class);
    }

    /**
     * @return the pk
     */
    public String getPk() {
        return this.pk;
    }

    /**
     * @param pk
     *            the pk to set
     */
    public void setPk(String pk) {
        this.pk = pk;
    }

    @Override
    public Byte getValByte() {
        return this.valByte;
    }

    @Override
    public void setValByte(Byte valByte) {
        this.valByte = valByte;
    }

    @Override
    public Boolean getValBoolean() {
        return this.valBoolean;
    }

    @Override
    public void setValBoolean(Boolean valBoolean) {
        this.valBoolean = valBoolean;
    }

    @Override
    public Character getValCharacter() {
        return this.valCharacter;
    }

    @Override
    public void setValCharacter(Character valCharacter) {
        this.valCharacter = valCharacter;
    }

    @Override
    public Short getValShort() {
        return this.valShort;
    }

    @Override
    public void setValShort(Short valShort) {
        this.valShort = valShort;
    }

    @Override
    public Integer getValInteger() {
        return this.valInteger;
    }

    @Override
    public void setValInteger(Integer valInteger) {
        this.valInteger = valInteger;
    }

    @Override
    public Long getValLong() {
        return this.valLong;
    }

    @Override
    public void setValLong(Long valLong) {
        this.valLong = valLong;
    }

    @Override
    public Float getValFloat() {
        return this.valFloat;
    }

    @Override
    public void setValFloat(Float valFloat) {
        this.valFloat = valFloat;
    }

    @Override
    public Double getValDouble() {
        return this.valDouble;
    }

    @Override
    public void setValDouble(Double valDouble) {
        this.valDouble = valDouble;
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

    @Override
    public Date getDate() {
        return this.date;
    }

    @Override
    public void setDate(Date date) {
        this.date = date;
    }

    @Override
    public EnumLocale getLocale() {
        return this.locale;
    }

    @Override
    public void setLocale(EnumLocale locale) {
        this.locale = locale;
    }

    /**
     * @return the parent1
     */
    public EntityParent getParent1() {
        return this.parent1;
    }

    /**
     * @param parent1
     *            the parent1 to set
     */
    public void setParent1(EntityParent parent1) {
        this.parent1 = parent1;
    }

    /**
     * @return the parent2
     */
    public EntityParent getParent2() {
        return this.parent2;
    }

    /**
     * @param parent2
     *            the parent2 to set
     */
    public void setParent2(EntityParent parent2) {
        this.parent2 = parent2;
    }

    @Override
    public String getPrimaryKey() {
        return this.pk;
    }

    @Override
    public void setPrimaryKey(String key) {
        this.pk = key;
    }

    @Override
    protected void overToString(final Map<String, Object> map) {
        map.put("pk", this.pk);
        map.put("byte", this.valByte);
        map.put("boolean", this.valBoolean);
        map.put("character", this.valCharacter);
        map.put("short", this.valShort);
        map.put("integer", this.valInteger);
        map.put("long", this.valLong);
        map.put("float", this.valFloat);
        map.put("double", this.valDouble);
        map.put("name", this.name);
        map.put("date", this.date);
        map.put("locale", this.locale);
    }
}
