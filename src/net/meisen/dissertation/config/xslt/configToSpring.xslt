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
  <xsl:import href="authmanager://includeXslts" />

  <xsl:output method="xml" indent="yes" />
  
  <xsl:variable name="queryFactoryId" select="cdef:getId('QUERYFACTORY_ID')" />
  <xsl:variable name="modelHandlerId" select="cdef:getId('MODELHANDLER_ID')" />
  <xsl:variable name="dimensionHandlerId" select="cdef:getId('DIMENSIONHANDLER_ID')" />
  <xsl:variable name="aggFuncId" select="cdef:getId('AGGREGATIONFUNCTIONHANDLER_ID')" />
  <xsl:variable name="authManagerId" select="cdef:getId('AUTHMANAGER_ID')" />
  <xsl:variable name="sessionManagerId" select="cdef:getId('SESSIONMANAGER_ID')" />
  <xsl:variable name="timeTemplateManagerId" select="cdef:getId('TIMETEMPLATEMANAGER_ID')" />

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
      <xsl:variable name="identifierCache">
        <xsl:choose>
          <xsl:when test="//cns:caches/cns:identifier/@implementation"><xsl:value-of select="//cns:caches/cns:identifier/@implementation" /></xsl:when>
          <xsl:otherwise><xsl:value-of select="cdef:getDefaultIdentifierCache()" /></xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="metaDataCache">
        <xsl:choose>
          <xsl:when test="//cns:caches/cns:metadata/@implementation"><xsl:value-of select="//cns:caches/cns:metadata/@implementation" /></xsl:when>
          <xsl:otherwise><xsl:value-of select="cdef:getDefaultMetaDataCache()" /></xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="bitmapCache">
        <xsl:choose>
          <xsl:when test="//cns:caches/cns:bitmap/@implementation"><xsl:value-of select="//cns:caches/cns:bitmap/@implementation" /></xsl:when>
          <xsl:otherwise><xsl:value-of select="cdef:getDefaultBitmapCache()" /></xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="factSetsCache">
        <xsl:choose>
          <xsl:when test="//cns:caches/cns:factsets/@implementation"><xsl:value-of select="//cns:caches/cns:factsets/@implementation" /></xsl:when>
          <xsl:otherwise><xsl:value-of select="cdef:getDefaultFactSetsCache()" /></xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="recordsCache">
        <xsl:choose>
          <xsl:when test="//cns:caches/cns:records/@implementation"><xsl:value-of select="//cns:caches/cns:records/@implementation" /></xsl:when>
          <xsl:otherwise><xsl:value-of select="cdef:getDefaultRecordsCache()" /></xsl:otherwise>
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

      <!-- define the default caches class -->
      <bean id="defaultIdentifierCacheClass" class="java.lang.String"><constructor-arg value="{$identifierCache}" /></bean>
      <bean id="defaultMetaDataCacheClass" class="java.lang.String"><constructor-arg value="{$metaDataCache}" /></bean>
      <bean id="defaultBitmapCacheClass" class="java.lang.String"><constructor-arg value="{$bitmapCache}" /></bean>
      <bean id="defaultFactSetsCacheClass" class="java.lang.String"><constructor-arg value="{$factSetsCache}" /></bean>
      <bean id="defaultRecordsCacheClass" class="java.lang.String"><constructor-arg value="{$recordsCache}" /></bean>

      <!-- read the default identifierCache configuration -->
      <bean id="defaultIdentifierCacheConfig" class="net.meisen.general.sbconfigurator.factories.BeanReference">
        <property name="bean">
          <xsl:choose>
            <xsl:when test="//cns:caches/cns:identifier/node()">
              <xsl:for-each select='//cns:caches/cns:identifier/node()'>
                <xsl:apply-imports />
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise><null /></xsl:otherwise>
          </xsl:choose>
        </property>
        <property name="class" value="net.meisen.dissertation.model.cache.IIdentifierCacheConfig" />
      </bean>

      <!-- read the default metaDataCache configuration -->
      <bean id="defaultMetaDataCacheConfig" class="net.meisen.general.sbconfigurator.factories.BeanReference">
        <property name="bean">
          <xsl:choose>
            <xsl:when test="//cns:caches/cns:metadata/node()">
              <xsl:for-each select='//cns:caches/cns:metadata/node()'>
                <xsl:apply-imports />
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise><null /></xsl:otherwise>
          </xsl:choose>
        </property>
        <property name="class" value="net.meisen.dissertation.model.cache.IMetaDataCacheConfig" />
      </bean>

      <!-- read the default bitmapCache configuration -->
      <bean id="defaultBitmapCacheConfig" class="net.meisen.general.sbconfigurator.factories.BeanReference">
        <property name="bean">
          <xsl:choose>
            <xsl:when test="//cns:caches/cns:bitmap/node()">
              <xsl:for-each select='//cns:caches/cns:bitmap/node()'>
                <xsl:apply-imports />
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise><null /></xsl:otherwise>
          </xsl:choose>
        </property>
        <property name="class" value="net.meisen.dissertation.model.cache.IBitmapIdCacheConfig" />
      </bean>
      
      <!-- read the default factSetsCache configuration -->
      <bean id="defaultFactSetsCacheConfig" class="net.meisen.general.sbconfigurator.factories.BeanReference">
        <property name="bean">
          <xsl:choose>
            <xsl:when test="//cns:caches/cns:factsets/node()">
              <xsl:for-each select='//cns:caches/cns:factsets/node()'>
                <xsl:apply-imports />
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise><null /></xsl:otherwise>
          </xsl:choose>
        </property>
        <property name="class" value="net.meisen.dissertation.model.cache.IBitmapIdCacheConfig" />
      </bean>
      
      <!-- read the default recordsCache configuration -->
      <bean id="defaultRecordsCacheConfig" class="net.meisen.general.sbconfigurator.factories.BeanReference">
        <property name="bean">
          <xsl:choose>
            <xsl:when test="//cns:caches/cns:records/node()">
              <xsl:for-each select='//cns:caches/cns:records/node()'>
                <xsl:apply-imports />
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise><null /></xsl:otherwise>
          </xsl:choose>
        </property>
        <property name="class" value="net.meisen.dissertation.model.cache.IDataRecordCacheConfig" />
      </bean>

      <!-- define the default factories' classes -->
      <bean id="defaultIndexFactoryClass" class="java.lang.String"><constructor-arg value="{$indexFactory}" /></bean>
      <bean id="defaultMapperFactoryClass" class="java.lang.String"><constructor-arg value="{$mapperFactory}" /></bean>
      <bean id="defaultGranularityFactoryClass" class="java.lang.String"><constructor-arg value="{$granularityFactory}" /></bean>
            
      <!-- define the default configuration defined for the indexFactory -->
      <bean id="defaultIndexFactoryConfig" class="net.meisen.general.sbconfigurator.factories.BeanReference">
        <property name="bean">
          <xsl:choose>
            <xsl:when test="//cns:config/cns:factories/cns:indexes/node()">
              <xsl:for-each select='//cns:config/cns:factories/cns:indexes/node()'>
                <xsl:apply-imports />
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise><null /></xsl:otherwise>
          </xsl:choose>
        </property>
        <property name="class" value="net.meisen.dissertation.model.indexes.IIndexFactoryConfig" />
      </bean>
      
      <!-- define the default configuration defined for the mapperFactory -->
      <bean id="defaultMapperFactoryConfig" class="net.meisen.general.sbconfigurator.factories.BeanReference">
        <property name="bean">
          <xsl:choose>
            <xsl:when test="//cns:config/cns:factories/cns:mappers/node()">
              <xsl:for-each select='//cns:config/cns:factories/cns:mappers/node()'>
                <xsl:apply-imports />
              </xsl:for-each>
            </xsl:when>
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
      
      <!-- create the authManager -->
      <xsl:choose>
        <xsl:when test="//cns:config/cns:auth/cns:manager/@implementation">
          <xsl:variable name="authManager" select="//cns:config/cns:auth/cns:manager/@implementation" />

          <bean id="{$authManagerId}" class="{$authManager}" init-method="init" destroy-method="release">
            <property name="config">
              <xsl:choose>
                <xsl:when test="//cns:config/cns:auth/cns:manager/node()">
                  <xsl:for-each select='//cns:config/cns:auth/cns:manager/node()'>
                    <xsl:apply-imports />
                  </xsl:for-each>
                </xsl:when>
                <xsl:otherwise><null /></xsl:otherwise>
              </xsl:choose>
            </property>
          </bean>
        </xsl:when>
        <xsl:otherwise>
          <xsl:variable name="authManager" select="cdef:getDefaultAuthManager()" />
          
          <bean id="{$authManagerId}" class="{$authManager}" init-method="init" destroy-method="release">
            <property name="config"><null /></property>
          </bean>
        </xsl:otherwise>
      </xsl:choose>

      <!-- create the queryFactory to be used -->
      <bean id="{$queryFactoryId}" class="{$queryFactory}" />
    
      <!-- define loaders used to load tidaModels -->
      <bean id="{$modelHandlerId}" class="net.meisen.dissertation.model.handler.TidaModelHandler" init-method="init">
        <property name="defaultLocation" ref="defaultLocation" /> 
      </bean>
      
      <!-- define loaders used to load tidaDimensions -->
      <bean id="{$dimensionHandlerId}" class="net.meisen.dissertation.model.handler.TidaDimensionHandler" />
      
      <!-- define the templateManager -->
      <bean id="{$timeTemplateManagerId}" class="net.meisen.dissertation.model.dimensions.templates.TimeLevelTemplateManager" />
      <xsl:for-each select="//cns:config/cns:timetemplates/cns:template">
        <xsl:variable name="class" select="@implementation" />
        <bean class="net.meisen.general.sbconfigurator.factories.MethodExecutorBean">
          <property name="targetMethod" value="addTemplate" />
          <property name="targetObject" ref="{$timeTemplateManagerId}" />
          <property name="type" value="factory" />
          <property name="arguments"><bean class="{$class}" /></property>
        </bean>
      </xsl:for-each>

      <!-- set the server properties, those are set prior to any further loading -->
      <xsl:if test="//cns:config/cns:server/node()">
        <bean class="net.meisen.general.sbconfigurator.config.PropertyInjectorBean">
          <property name="properties">
            <props>
              <xsl:if test="//cns:config/cns:server/cns:http/@port">
                <prop key="tida.server.http.port"><xsl:value-of select="//cns:config/cns:server/cns:http/@port" /></prop>
              </xsl:if>
              <xsl:if test="//cns:config/cns:server/cns:http/@enable">
                <prop key="tida.server.http.enabled"><xsl:value-of select="//cns:config/cns:server/cns:http/@enable" /></prop>
              </xsl:if>
              <xsl:if test="//cns:config/cns:server/cns:http/@docroot">
                <prop key="tida.server.http.docroot"><xsl:value-of select="//cns:config/cns:server/cns:http/@docroot" /></prop>
              </xsl:if>
              <xsl:if test="//cns:config/cns:server/cns:tsql/@port">
                <prop key="tida.server.tsql.port"><xsl:value-of select="//cns:config/cns:server/cns:tsql/@port" /></prop>
              </xsl:if>
              <xsl:if test="//cns:config/cns:server/cns:tsql/@enable">
                <prop key="tida.server.tsql.enabled"><xsl:value-of select="//cns:config/cns:server/cns:tsql/@enable" /></prop>
              </xsl:if>
              <xsl:if test="//cns:config/cns:server/cns:tsql/@timeout">
                <prop key="tida.server.tsql.timeout"><xsl:value-of select="//cns:config/cns:server/cns:tsql/@timeout" /></prop>
              </xsl:if>
              <xsl:if test="//cns:config/cns:server/cns:control/@port">
                <prop key="tida.server.control.port"><xsl:value-of select="//cns:config/cns:server/cns:control/@port" /></prop>
              </xsl:if>
              <xsl:if test="//cns:config/cns:server/cns:control/@enable">
                <prop key="tida.server.control.enabled"><xsl:value-of select="//cns:config/cns:server/cns:control/@enable" /></prop>
              </xsl:if>
            </props>
          </property>
        </bean>
      </xsl:if>
      
      <!-- define the SessionManager, which is only needed on HTTP connections -->
      <bean id="{$sessionManagerId}" class="net.meisen.dissertation.server.sessions.SessionManager" destroy-method="release">
      	<xsl:if test="//cns:config/cns:server/cns:http/@enable">
      	  <xsl:variable name="httpEnabled" select="//cns:config/cns:server/cns:http/@enable" />
      	  <constructor-arg type="boolean" value="{$httpEnabled}"/>
      	</xsl:if>
        <xsl:if test="//cns:config/cns:server/cns:http/@timeout">
          <xsl:variable name="timeOutInMin" select="//cns:config/cns:server/cns:http/@timeout" />
          <property name="timeOutInMin" value="{$timeOutInMin}" />
        </xsl:if>
        <xsl:if test="//cns:config/cns:server/cns:http/@tmproot">
          <xsl:variable name="tmproot" select="//cns:config/cns:server/cns:http/@tmproot" />
          <property name="tmpDir" value="{$tmproot}" />
        </xsl:if>
      </bean>
            
      <!-- define the tidaServer -->
      <bean id="tidaServer" class="net.meisen.dissertation.server.TidaServer" />

    </beans>
  </xsl:template>

</xsl:stylesheet>