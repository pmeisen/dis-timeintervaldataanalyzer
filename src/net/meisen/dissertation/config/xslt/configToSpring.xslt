<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0"
        xmlns="http://www.springframework.org/schema/beans"
        xmlns:cns="http://dev.meisen.net/xsd/dissertation/config" 
        xmlns:cdef="net.meisen.dissertation.config.xslt.DefaultValues"
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="indexFactory://includeXslts" />

  <xsl:output method="xml" indent="yes" />
  
  <xsl:variable name="queryFactoryId" select="cdef:getId('QUERYFACTORY_ID')" />
  <xsl:variable name="handlerId" select="cdef:getId('HANDLER_ID')" />
    
  <xsl:template match="/cns:config">
    <beans xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans-3.0.xsd
                               http://www.springframework.org/schema/util http://www.springframework.org/schema/util/spring-util-2.0.xsd">

      <xsl:variable name="indexFactory">
        <xsl:choose>
          <xsl:when test="//cns:factories/cns:indexes/@implementation"><xsl:value-of select="//cns:config/cns:factories/cns:indexes/@implementation" /></xsl:when>
          <xsl:otherwise><xsl:value-of select="cdef:getDefaultIndexFactory()" /></xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="mapperFactory">
        <xsl:choose>
          <xsl:when test="//cns:factories/cns:mappers/@implementation"><xsl:value-of select="//cns:config/cns:factories/cns:mappers/@implementation" /></xsl:when>
          <xsl:otherwise><xsl:value-of select="cdef:getDefaultMappersFactory()" /></xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="granularityFactory">
        <xsl:choose>
          <xsl:when test="//cns:factories/cns:granularities/@implementation"><xsl:value-of select="//cns:config/cns:factories/cns:granularities/@implementation" /></xsl:when>
          <xsl:otherwise><xsl:value-of select="cdef:getDefaultGranularitiesFactory()" /></xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="queryFactory">
        <xsl:choose>
          <xsl:when test="//cns:factories/cns:granularities/@implementation"><xsl:value-of select="//cns:config/cns:factories/cns:queries/@implementation" /></xsl:when>
          <xsl:otherwise><xsl:value-of select="cdef:getDefaultQueryFactory()" /></xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
                             
      <!-- define the default factories' classes -->
      <bean id="defaultIndexFactoryClass" class="java.lang.String"><constructor-arg value="{$indexFactory}" /></bean>
      <bean id="defaultMapperFactoryClass" class="java.lang.String"><constructor-arg value="{$mapperFactory}" /></bean>
      <bean id="defaultGranularityFactoryClass" class="java.lang.String"><constructor-arg value="{$granularityFactory}" /></bean>
      
      <!-- define the default configuration defined -->
      <bean id="defaultIndexFactoryConfig" class="net.meisen.general.sbconfigurator.factories.BeanReference">
        <property name="bean">
          <xsl:choose>
            <xsl:when test="//cns:config/cns:factories/cns:indexes/node()"><xsl:apply-imports /></xsl:when>
            <xsl:otherwise><null /></xsl:otherwise>
          </xsl:choose>
        </property>
        <property name="class" value="net.meisen.dissertation.impl.indexes.IndexFactoryConfig" />
      </bean>

      <!-- create the queryFactory to be used -->
      <bean id="{$queryFactoryId}" class="{$queryFactory}" />
    
      <!-- define loaders used to load tidaModels -->
      <bean id="{$handlerId}" class="net.meisen.dissertation.model.handler.TidaModelHandler" />
  
      <!-- define the tidaServer -->
      <bean id="tidaServer" class="net.meisen.dissertation.server.TidaServer" />

    </beans>
  </xsl:template>

</xsl:stylesheet>