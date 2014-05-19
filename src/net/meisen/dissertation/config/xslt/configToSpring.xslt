<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0"
        xmlns="http://www.springframework.org/schema/beans"
        xmlns:cns="http://dev.meisen.net/xsd/dissertation/config" 
        xmlns:cdef="net.meisen.dissertation.config.xslt.DefaultValues"
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="classpath://net/meisen/dissertation/config/xslt/utilToSpring.xslt" />

  <xsl:import href="indexFactory://includeXslts" />
  <xsl:import href="mapperFactory://includeXslts" />
  <xsl:import href="cache://includeXslts" />

  <xsl:output method="xml" indent="yes" />
  
  <xsl:variable name="queryFactoryId" select="cdef:getId('QUERYFACTORY_ID')" />
  <xsl:variable name="handlerId" select="cdef:getId('HANDLER_ID')" />
  <xsl:variable name="aggFuncId" select="cdef:getId('AGGREGATIONFUNCTIONHANDLER_ID')" />

  <xsl:template match="/cns:config">
    <beans xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans-3.0.xsd
                               http://www.springframework.org/schema/util http://www.springframework.org/schema/util/spring-util-2.0.xsd">

      <!-- read the values configured -->
      <xsl:variable name="folder">
        <xsl:choose>
          <xsl:when test="//cns:location/@folder"><xsl:value-of select="//cns:location/@folder" /></xsl:when>
          <xsl:otherwise><xsl:value-of select="cdef:getDefaultLocation()" /></xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="cache">
        <xsl:choose>
          <xsl:when test="//cns:cache/@implementation"><xsl:value-of select="//cns:cache/@implementation" /></xsl:when>
          <xsl:otherwise><xsl:value-of select="cdef:getDefaultCache()" /></xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
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

      <!-- define the default location -->
      <bean id="defaultLocation" class="java.lang.String"><constructor-arg value="{$folder}" /></bean>

      <!-- define the default cache class -->
      <bean id="defaultCacheClass" class="java.lang.String"><constructor-arg value="{$cache}" /></bean>

      <!-- read the default cache configuration -->
      <bean id="defaultCacheConfig" class="net.meisen.general.sbconfigurator.factories.BeanReference">
        <property name="bean">
          <xsl:choose>
            <xsl:when test="//cns:cache/node()"><xsl:apply-imports /></xsl:when>
            <xsl:otherwise><null /></xsl:otherwise>
          </xsl:choose>
        </property>
        <property name="class" value="net.meisen.dissertation.model.cache.IBitmapCacheConfig" />
      </bean>

      <!-- define the default factories' classes -->
      <bean id="defaultIndexFactoryClass" class="java.lang.String"><constructor-arg value="{$indexFactory}" /></bean>
      <bean id="defaultMapperFactoryClass" class="java.lang.String"><constructor-arg value="{$mapperFactory}" /></bean>
      <bean id="defaultGranularityFactoryClass" class="java.lang.String"><constructor-arg value="{$granularityFactory}" /></bean>
            
      <!-- define the default configuration defined for the indexFactory -->
      <bean id="defaultIndexFactoryConfig" class="net.meisen.general.sbconfigurator.factories.BeanReference">
        <property name="bean">
          <xsl:choose>
            <xsl:when test="//cns:config/cns:factories/cns:indexes/node()"><xsl:apply-imports /></xsl:when>
            <xsl:otherwise><null /></xsl:otherwise>
          </xsl:choose>
        </property>
        <property name="class" value="net.meisen.dissertation.model.indexes.IIndexFactoryConfig" />
      </bean>
      
      <!-- define the default configuration defined for the mapperFactory -->
      <bean id="defaultMapperFactoryConfig" class="net.meisen.general.sbconfigurator.factories.BeanReference">
        <property name="bean">
          <xsl:choose>
            <xsl:when test="//cns:config/cns:factories/cns:mappers/node()"><xsl:apply-imports /></xsl:when>
            <xsl:otherwise><null /></xsl:otherwise>
          </xsl:choose>
        </property>
        <property name="class" value="net.meisen.dissertation.model.time.mapper.IMapperFactoryConfig" />
      </bean>

      <!-- define the aggregationFunctionHandler to be used -->
      <bean id="{$aggFuncId}" class="net.meisen.dissertation.model.measures.AggregationFunctionHandler">
        <constructor-arg type="java.util.Collection">
        	<bean class="net.meisen.general.sbconfigurator.factories.MergedCollection">
        	  <property name="collections">
        	    <list>
        	      <!-- add the default values first -->
        	      <list>
        	        <xsl:variable name="defAggFunctions" select="cdef:getDefaultAggregationFunctions()" />
        	        <xsl:call-template name="csvToBeans">
                      <xsl:with-param name="list" select="$defAggFunctions" />
                      <xsl:with-param name="itemSeparator" select="','" />
        	        </xsl:call-template>
        	      </list>
        	      <!-- next add the additionally defined functions -->
        	      <xsl:if test="//cns:config/cns:aggregations/cns:function">
        	        <list>
        	          <xsl:for-each select="//cns:config/cns:aggregations/cns:function">
        	            <xsl:variable name="class" select="@implementation" />
        	            <bean class="{$class}" />
        	          </xsl:for-each>
        	        </list>
        	      </xsl:if>
        	    </list>
        	  </property>
        	</bean>
        </constructor-arg>
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