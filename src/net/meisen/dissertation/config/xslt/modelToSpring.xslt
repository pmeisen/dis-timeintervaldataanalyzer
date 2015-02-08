<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0"
        xmlns="http://www.springframework.org/schema/beans"
        xmlns:mns="http://dev.meisen.net/xsd/dissertation/model"
        xmlns:dim="http://dev.meisen.net/xsd/dissertation/dimension" 
        xmlns:uuid="java.util.UUID"
        xmlns:mdef="net.meisen.dissertation.config.xslt.DefaultValues"
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="classpath://net/meisen/dissertation/config/xslt/dimensionToSpring.xslt" />

  <xsl:import href="descriptors://includeXslts" />
  <xsl:import href="dataretriever://includeXslts" />
  <xsl:import href="indexFactory://includeXslts" />
  <xsl:import href="mapperFactory://includeXslts" />
  <xsl:import href="cache://includeXslts" />
  <xsl:import href="preprocessor://includeXslts" />

  <xsl:output method="xml" indent="yes" />
  
  <xsl:variable name="metaDataModelId" select="mdef:getId('METADATAMODEL_ID')" />
  <xsl:variable name="metaDataCollectionId" select="mdef:getId('METADATACOLLECTION_ID')" />
  <xsl:variable name="dataModelId" select="mdef:getId('DATAMODEL_ID')" />
  <xsl:variable name="intervalModelId" select="mdef:getId('INTERVALMODEL_ID')" />
  <xsl:variable name="dimensionModelId" select="mdef:getId('DIMENSIONMODEL_ID')" />
  <xsl:variable name="dataStructureId" select="mdef:getId('DATASTRUCTURE_ID')" />
  <xsl:variable name="indexFactoryId" select="mdef:getId('INDEXFACTORY_ID')" />
  <xsl:variable name="mapperFactoryId" select="mdef:getId('MAPPERFACTORY_ID')" />
  <xsl:variable name="dataRecordFactoryId" select="mdef:getId('DATARECORDFACTORY_ID')" />
  <xsl:variable name="granularityFactoryId" select="mdef:getId('GRANULARITYFACTORY_ID')" />
  <xsl:variable name="timelineDefinitionId" select="mdef:getId('TIMELINEDEFINITION_ID')" />
  <xsl:variable name="identifierCacheId" select="mdef:getId('IDENTIFIERCACHE_ID')" />
  <xsl:variable name="metaDataCacheId" select="mdef:getId('METADATACACHE_ID')" />
  <xsl:variable name="bitmapCacheId" select="mdef:getId('BITMAPCACHE_ID')" />
  <xsl:variable name="recordsCacheId" select="mdef:getId('DATARECORDCACHE_ID')" />
  <xsl:variable name="factSetsCacheId" select="mdef:getId('FACTSETSCACHE_ID')" />
  <xsl:variable name="preProcessorId" select="mdef:getId('PREPROCESSOR_ID')" />
  <xsl:variable name="tidaModelId" select="mdef:getId('TIDAMODEL_ID')" /> 

  <xsl:template match="/mns:model">
    <xsl:variable name="modelId">
      <xsl:choose>
        <xsl:when test="@id"><xsl:value-of select="@id" /></xsl:when>
        <xsl:otherwise><xsl:value-of select="uuid:randomUUID()" /></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="modelName">
      <xsl:choose>
        <xsl:when test="@name"><xsl:value-of select="@name" /></xsl:when>
        <xsl:otherwise><xsl:value-of select="$modelId" /></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="modelLocation">
      <xsl:choose>
        <xsl:when test="@folder"><xsl:value-of select="@folder" /></xsl:when>
        <xsl:otherwise></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="metahandling">
      <xsl:choose>
        <xsl:when test="mns:data/@metahandling"><xsl:value-of select="mns:data/@metahandling"/></xsl:when>
        <xsl:otherwise></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="intervalhandling">
      <xsl:choose>
        <xsl:when test="mns:data/@intervalhandling"><xsl:value-of select="mns:data/@intervalhandling"/></xsl:when>
        <xsl:otherwise></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="offlinemode">
      <xsl:choose>
        <xsl:when test="@offlinemode"><xsl:value-of select="@offlinemode"/></xsl:when>
        <xsl:otherwise></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
      
    <beans xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans-3.0.xsd
                               http://www.springframework.org/schema/util http://www.springframework.org/schema/util/spring-util-2.0.xsd">

      <!-- create the identifierCache to be used -->     
      <xsl:choose>
        <xsl:when test="//mns:config/mns:caches/mns:identifier/@implementation">
          <xsl:variable name="identifierCache" select="//mns:config/mns:caches/mns:identifier/@implementation" />
          <bean id="{$identifierCacheId}" class="{$identifierCache}" destroy-method="release">
            <property name="config">
              <xsl:choose>
                <xsl:when test="//mns:config/mns:caches/mns:identifier/node()">
                  <xsl:for-each select='//mns:config/mns:caches/mns:identifier/node()'>
                    <xsl:apply-imports />
                  </xsl:for-each>
                </xsl:when>
                <xsl:otherwise><ref bean="defaultIdentifierCacheConfig" /></xsl:otherwise>
              </xsl:choose>
            </property>
          </bean>
        </xsl:when>
        <xsl:otherwise>
          <bean id="{$identifierCacheId}" class="net.meisen.general.sbconfigurator.factories.BeanCreator">
            <property name="beanClass" ref="defaultIdentifierCacheClass" />
            <property name="properties">
              <map><entry key="config" value-ref="defaultIdentifierCacheConfig" /></map>
            </property>
          </bean>
        </xsl:otherwise>
      </xsl:choose>

      <!-- create the metaDataCache to be used -->     
      <xsl:choose>
        <xsl:when test="//mns:config/mns:caches/mns:metadata/@implementation">
          <xsl:variable name="metaDataCache" select="//mns:config/mns:caches/mns:metadata/@implementation" />
          <bean id="{$metaDataCacheId}" class="{$metaDataCache}" destroy-method="release">
            <property name="config">
              <xsl:choose>
                <xsl:when test="//mns:config/mns:caches/mns:metadata/node()">
                  <xsl:for-each select='//mns:config/mns:caches/mns:metadata/node()'>
                    <xsl:apply-imports />
                  </xsl:for-each>
                </xsl:when>
                <xsl:otherwise><ref bean="defaultMetaDataCacheConfig" /></xsl:otherwise>
              </xsl:choose>
            </property>
          </bean>
        </xsl:when>
        <xsl:otherwise>
          <bean id="{$metaDataCacheId}" class="net.meisen.general.sbconfigurator.factories.BeanCreator">
            <property name="beanClass" ref="defaultMetaDataCacheClass" />
            <property name="properties">
              <map><entry key="config" value-ref="defaultMetaDataCacheConfig" /></map>
            </property>
          </bean>
        </xsl:otherwise>
      </xsl:choose>

      <!-- create the bitmapCache to be used -->     
      <xsl:choose>
        <xsl:when test="//mns:config/mns:caches/mns:bitmap/@implementation">
          <xsl:variable name="bitmapCache" select="//mns:config/mns:caches/mns:bitmap/@implementation" />
          <bean id="{$bitmapCacheId}" class="{$bitmapCache}" destroy-method="release">
            <property name="config">
              <xsl:choose>
                <xsl:when test="//mns:config/mns:caches/mns:bitmap/node()">
                  <xsl:for-each select='//mns:config/mns:caches/mns:bitmap/node()'>
                    <xsl:apply-imports />
                  </xsl:for-each>
                </xsl:when>
                <xsl:otherwise><ref bean="defaultBitmapCacheConfig" /></xsl:otherwise>
              </xsl:choose>
            </property>
          </bean>
        </xsl:when>
        <xsl:otherwise>
          <bean id="{$bitmapCacheId}" class="net.meisen.general.sbconfigurator.factories.BeanCreator">
            <property name="beanClass" ref="defaultBitmapCacheClass" />
            <property name="properties">
              <map><entry key="config" value-ref="defaultBitmapCacheConfig" /></map>
            </property>
          </bean>
        </xsl:otherwise>
      </xsl:choose>
      
      <!-- create the factSetsCache to be used -->     
      <xsl:choose>
        <xsl:when test="//mns:config/mns:caches/mns:factsets/@implementation">
          <xsl:variable name="factSetsCache" select="//mns:config/mns:caches/mns:factsets/@implementation" />
          <bean id="{$factSetsCacheId}" class="{$factSetsCache}" destroy-method="release">
            <property name="config">
              <xsl:choose>
                <xsl:when test="//mns:config/mns:caches/mns:factsets/node()">
                  <xsl:for-each select='//mns:config/mns:caches/mns:factsets/node()'>
                    <xsl:apply-imports />
                  </xsl:for-each>
                </xsl:when>
                <xsl:otherwise><ref bean="defaultFactSetsCacheConfig" /></xsl:otherwise>
              </xsl:choose>
            </property>
          </bean>
        </xsl:when>
        <xsl:otherwise>
          <bean id="{$factSetsCacheId}" class="net.meisen.general.sbconfigurator.factories.BeanCreator">
            <property name="beanClass" ref="defaultFactSetsCacheClass" />
            <property name="properties">
              <map><entry key="config" value-ref="defaultFactSetsCacheConfig" /></map>
            </property>
          </bean>
        </xsl:otherwise>
      </xsl:choose>
      
      <!-- create the dataRecordCache to be used -->
      <xsl:choose>
        <xsl:when test="//mns:config/mns:caches/mns:records/@implementation">
          <xsl:variable name="recordsCache" select="//mns:config/mns:caches/mns:records/@implementation" />
          <bean id="{$recordsCacheId}" class="{$recordsCache}" destroy-method="release">
            <property name="config">
              <xsl:choose>
                <xsl:when test="//mns:config/mns:caches/mns:records/node()">
                  <xsl:for-each select='//mns:config/mns:caches/mns:records/node()'>
                    <xsl:apply-imports />
                  </xsl:for-each>
                </xsl:when>
                <xsl:otherwise><ref bean="defaultRecordsCacheConfig" /></xsl:otherwise>
              </xsl:choose>
            </property>
          </bean>
        </xsl:when>
        <xsl:otherwise>        
          <bean id="{$recordsCacheId}" class="net.meisen.general.sbconfigurator.factories.BeanCreator">
            <property name="beanClass" ref="defaultRecordsCacheClass" />
            <property name="properties">
              <map><entry key="config" value-ref="defaultRecordsCacheConfig" /></map>
            </property>
          </bean>
        </xsl:otherwise>
      </xsl:choose>
            
      <!-- create the indexFactory to be used -->
      <xsl:choose>
        <xsl:when test="//mns:config/mns:factories/mns:indexes/@implementation">
          <xsl:variable name="indexFactory" select="//mns:config/mns:factories/mns:indexes/@implementation" />
          <bean id="{$indexFactoryId}" class="{$indexFactory}">
            <property name="config">
              <xsl:choose>
                <xsl:when test="//mns:config/mns:factories/mns:indexes/node()">
                  <xsl:for-each select='//mns:config/mns:factories/mns:indexes/node()'>
                    <xsl:apply-imports />
                  </xsl:for-each>
                </xsl:when>
                <xsl:otherwise><ref bean="defaultIndexFactoryConfig" /></xsl:otherwise>
              </xsl:choose>
            </property>
          </bean>
        </xsl:when>
        <xsl:otherwise>
          <bean id="{$indexFactoryId}" class="net.meisen.general.sbconfigurator.factories.BeanCreator">
            <property name="beanClass" ref="defaultIndexFactoryClass" />
            <property name="properties">
              <map><entry key="config" value-ref="defaultIndexFactoryConfig" /></map>
            </property>
          </bean>
        </xsl:otherwise>
      </xsl:choose>
      
      <!-- create the mapperFactory to be used -->
      <xsl:choose>
        <xsl:when test="//mns:config/mns:factories/mns:mappers/@implementation">
          <xsl:variable name="mapperFactory" select="//mns:config/mns:factories/mns:mappers/@implementation" />
          <bean id="{$mapperFactoryId}" class="{$mapperFactory}">
            <property name="config">
              <xsl:choose>
                <xsl:when test="//mns:config/mns:factories/mns:mappers/node()">
                  <xsl:for-each select='//mns:config/mns:factories/mns:mappers/node()'>
                    <xsl:apply-imports />
                  </xsl:for-each>
                </xsl:when>
                <xsl:otherwise><ref bean="defaultMapperFactoryConfig" /></xsl:otherwise>
              </xsl:choose>
            </property>
          </bean>
        </xsl:when>
        <xsl:otherwise>          
          <bean id="{$mapperFactoryId}" class="net.meisen.general.sbconfigurator.factories.BeanCreator">
            <property name="beanClass" ref="defaultMapperFactoryClass" />
            <property name="properties">
              <map><entry key="config" value-ref="defaultMapperFactoryConfig" /></map>
            </property>
          </bean>
        </xsl:otherwise>        
      </xsl:choose>

      <!-- create the granularityFactory to be used -->
      <xsl:choose>
        <xsl:when test="//mns:config/mns:factories/mns:granularities/@implementation">
          <xsl:variable name="granularityFactory" select="//mns:config/mns:factories/mns:granularities/@implementation" />
          <bean id="{$granularityFactoryId}" class="{$granularityFactory}" />        
        </xsl:when>
        <xsl:otherwise>
          <bean id="{$granularityFactoryId}" class="net.meisen.general.sbconfigurator.factories.BeanCreator">
            <property name="beanClass" ref="defaultGranularityFactoryClass" />
          </bean>
        </xsl:otherwise>
      </xsl:choose>
      
      <!-- create the pre-processor to be used -->
      <xsl:if test="//mns:config/mns:preprocessor/@implementation">
        <xsl:variable name="preProcessor" select="//mns:config/mns:preprocessor/@implementation" />
        <bean id="{$preProcessorId}" class="{$preProcessor}">
          <xsl:if test="//mns:config/mns:preprocessor/node()">
            <property name="config">
              <xsl:for-each select="mns:config/mns:preprocessor">
                <xsl:apply-templates />
              </xsl:for-each>
            </property>
          </xsl:if>
        </bean>
      </xsl:if>      
                  
      <!-- create all the defined dataRetrievers -->
      <xsl:for-each select="mns:config/mns:dataretrievers/mns:dataretriever">
        <xsl:variable name="id" select="@id" />
        <xsl:variable name="implementation">
          <xsl:choose>
            <xsl:when test="@implementation"><xsl:value-of select="@implementation" /></xsl:when>
            <xsl:otherwise><xsl:value-of select="mdef:getDefaultDataRetriever(@id)"/></xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
            
        <bean id="dataretriever-{$id}" class="{$implementation}" destroy-method="release">
          <constructor-arg type="java.lang.String" value="{$id}" />
          <constructor-arg type="net.meisen.dissertation.model.dataretriever.IDataRetrieverConfig">
            <xsl:choose>
              <xsl:when test="node()"><xsl:apply-imports /></xsl:when>
              <xsl:otherwise><null /></xsl:otherwise>
            </xsl:choose>
          </constructor-arg>
        </bean>
      </xsl:for-each>
      
      <!-- create the timelineDefinition -->
      <bean id="{$timelineDefinitionId}" class="net.meisen.dissertation.model.time.timeline.TimelineDefinition">
        <constructor-arg type="java.lang.String">
          <xsl:choose>
            <xsl:when test="mns:time/mns:timeline/@start"><value><xsl:value-of select="mns:time/mns:timeline/@start" /></value></xsl:when>
            <xsl:otherwise><null /></xsl:otherwise>
          </xsl:choose>            
        </constructor-arg>
        <constructor-arg type="java.lang.String">
          <xsl:choose>
            <xsl:when test="mns:time/mns:timeline/@end"><value><xsl:value-of select="mns:time/mns:timeline/@end" /></value></xsl:when>
            <xsl:otherwise><null /></xsl:otherwise>
          </xsl:choose>            
        </constructor-arg>
        <constructor-arg type="net.meisen.dissertation.model.time.granularity.ITimeGranularity">
          <xsl:variable name="granularity">
            <xsl:choose>
              <xsl:when test="mns:time/mns:timeline/@granularity"><xsl:value-of select="mns:time/mns:timeline/@granularity" /></xsl:when>
              <xsl:otherwise><xsl:value-of select="mdef:getDefaultGranularity()" /></xsl:otherwise>
            </xsl:choose>
          </xsl:variable>
          <bean factory-bean="{$granularityFactoryId}" factory-method="find">
            <constructor-arg type="java.lang.String" value="{$granularity}" />
          </bean>
        </constructor-arg>
      </bean>
      
      <!-- modify the timelineDefinition if needed -->
      <xsl:if test="mns:time/mns:timeline/@duration">
        <xsl:variable name="durationgranularity">
          <xsl:choose>
            <xsl:when test="mns:time/mns:timeline/@durationgranularity"><xsl:value-of select="mns:time/mns:timeline/@durationgranularity" /></xsl:when>
            <xsl:when test="mns:time/mns:timeline/@granularity"><xsl:value-of select="mns:time/mns:timeline/@granularity" /></xsl:when>
            <xsl:otherwise><xsl:value-of select="mdef:getDefaultGranularity()" /></xsl:otherwise>
          </xsl:choose>
        </xsl:variable>

        <bean class="net.meisen.general.sbconfigurator.factories.MethodExecutorBean">
          <property name="targetMethod" value="setDuration" />
          <property name="targetObject" ref="{$timelineDefinitionId}" />
          <property name="type" value="factory" />
          <property name="arguments">
            <list>
              <value type=""><xsl:value-of select="mns:time/mns:timeline/@duration" /></value>
              <value type="net.meisen.dissertation.model.time.timeline.TimelineDefinition.Position">START</value>
              <bean factory-bean="{$granularityFactoryId}" factory-method="find">
                <constructor-arg type="java.lang.String" value="{$durationgranularity}" />
              </bean>
            </list>
          </property>
        </bean>
      </xsl:if>
      
      <!-- create all the descriptorModels -->
      <xsl:for-each select="mns:meta/mns:descriptors">
        <xsl:apply-templates />
      </xsl:for-each>
      
      <!-- create the metaDataCollection -->
      <bean id="{$metaDataCollectionId}" class="net.meisen.dissertation.model.data.metadata.ConfiguredMetaDataCollection">
        <property name="metaData">
          <list value-type="net.meisen.dissertation.model.data.metadata.IMetaData">

            <xsl:for-each select="mns:meta/mns:entries/mns:entry[not(@dataretriever)]">
              <xsl:variable name="descriptorValue">
                <xsl:choose>
                  <xsl:when test="@value"><xsl:value-of select="@value" /></xsl:when>
                  <xsl:otherwise></xsl:otherwise>
                </xsl:choose>
              </xsl:variable> 
              <xsl:variable name="descriptorModel" select="@descriptor" />
              
              <bean class="net.meisen.dissertation.impl.data.metadata.DirectlyDefinedMetaData">
                <constructor-arg type="java.lang.String" value="{$descriptorModel}" />
                <constructor-arg><array value-type="java.lang.Object"><value><xsl:value-of select="$descriptorValue" /></value></array></constructor-arg>
              </bean>
            </xsl:for-each>
                  
            <xsl:for-each select="mns:meta/mns:entries/mns:entry[@dataretriever]">
              <xsl:variable name="descriptorModel" select="@descriptor" />
              <xsl:variable name="dataretriever" select="@dataretriever" />

              <bean class="net.meisen.dissertation.impl.data.metadata.DbMetaData">
                <constructor-arg type="java.lang.String" value="{$descriptorModel}" />
                <constructor-arg type="net.meisen.dissertation.model.dataretriever.BaseDataRetriever" ref="dataretriever-{$dataretriever}" />
                <constructor-arg type="net.meisen.dissertation.model.dataretriever.IQueryConfiguration">
                  <xsl:choose>
                    <xsl:when test="node()"><xsl:apply-imports /></xsl:when>
                    <xsl:otherwise><null /></xsl:otherwise>
                  </xsl:choose>
                </constructor-arg>
                
                <property name="offlineModeByString" value="{$offlinemode}" />
              </bean>
            </xsl:for-each>

          </list>
        </property>
      </bean>
      
      <!-- create the MetaDataModel -->
      <bean id="{$metaDataModelId}" class="net.meisen.dissertation.model.data.MetaDataModel">
      	<property name="offlineModeByString" value="{$offlinemode}" />
      </bean>
      
      <!-- add the descriptorModels to the metaDataModel -->
      <bean class="net.meisen.general.sbconfigurator.factories.MethodExecutorBean">
        <property name="targetMethod" value="addDescriptorModels" />
        <property name="targetObject" ref="{$metaDataModelId}" />

        <property name="type" value="init" />
        
        <property name="arguments">
          <list value-type="net.meisen.dissertation.model.descriptors.DescriptorModel">
            <xsl:for-each select="mns:meta/mns:descriptors/*">
              <xsl:variable name="id" select="@id" />
              <ref bean="descriptormodel-{$id}" />
            </xsl:for-each>    
          </list>
        </property>
      </bean>
      
      <!-- create the intervalModel -->
      <bean id="{$intervalModelId}" class="net.meisen.dissertation.model.data.IntervalModel">
        <constructor-arg type="net.meisen.dissertation.model.time.timeline.TimelineDefinition">
          <ref bean="{$timelineDefinitionId}" />
        </constructor-arg>
      </bean>
      
      <!-- create the dimensions -->
      <bean id="{$dimensionModelId}" class="net.meisen.dissertation.model.data.DimensionModel" />
      <xsl:choose>
        <xsl:when test="//dim:dimensions">
          <xsl:for-each select='//dim:dimensions/dim:dimension'>
            <xsl:apply-imports />
          </xsl:for-each>
          <xsl:for-each select='//dim:dimensions/dim:timedimension'>
            <xsl:apply-imports />
          </xsl:for-each>
        </xsl:when>
      </xsl:choose>
            
      <!-- create the dataStructure -->
      <bean id="{$dataStructureId}" class="net.meisen.dissertation.model.data.DataStructure">
        <constructor-arg type="net.meisen.dissertation.model.datastructure.StructureEntry[]">
          <xsl:choose>
          
            <xsl:when test="mns:structure/node()">
              <array value-type="net.meisen.dissertation.model.datastructure.StructureEntry">
                <xsl:for-each select="mns:structure/node()[not(self::text()[not(normalize-space())])]">
                  <xsl:choose>
                  
                    <xsl:when test="local-name() = 'meta'">
                      <bean class="net.meisen.dissertation.model.datastructure.MetaStructureEntry">
                        <constructor-arg type="java.lang.String"><value><xsl:value-of select="@descriptor" /></value></constructor-arg>
                        <xsl:call-template name="beanStructureEntryConstructorArgs" />
                      </bean>
                    </xsl:when>
                                        
                    <xsl:when test="local-name() = 'interval'">
                      <bean class="net.meisen.dissertation.model.datastructure.IntervalStructureEntry">
                        <constructor-arg type="java.lang.String"><value><xsl:value-of select="@type" /></value></constructor-arg>
                        <xsl:if test="@inclusive">
                          <constructor-arg type="boolean"><value><xsl:value-of select="@inclusive" /></value></constructor-arg>
                        </xsl:if>
                        <xsl:call-template name="beanStructureEntryConstructorArgs" />
                      </bean>
                    </xsl:when>
                    
                    <xsl:otherwise><xsl:message terminate="no">Invalid element with name '<xsl:value-of select="local-name()" />' found, it will be skipped!</xsl:message></xsl:otherwise>
                  </xsl:choose>
                </xsl:for-each>
              </array>
            </xsl:when>
            
            <xsl:otherwise><null /></xsl:otherwise>
          </xsl:choose>
        </constructor-arg>
      </bean>
      
      <!-- create the dataModel -->
      <bean id="{$dataModelId}" class="net.meisen.dissertation.model.data.DataModel">
        <property name="offlineModeByString" value="{$offlinemode}" />
      </bean>
      
      <!-- add the static dataSets -->
      <xsl:for-each select="mns:data/mns:dataset[not(@dataretriever)]">
        <bean class="net.meisen.dissertation.impl.datasets.SingleStaticDataSet">
          <constructor-arg type="net.meisen.dissertation.impl.datasets.SingleStaticDataSetEntry[]">
            <array value-type="net.meisen.dissertation.impl.datasets.SingleStaticDataSetEntry">
              <xsl:for-each select="mns:entry">
              
                <bean class="net.meisen.dissertation.impl.datasets.ConfiguredSingleStaticDataSetEntry">
                  <constructor-arg type="int">
                    <value><xsl:choose><xsl:when test="@position"><xsl:value-of select="@position" /></xsl:when><xsl:otherwise>-1</xsl:otherwise></xsl:choose></value>
                  </constructor-arg>
                  <constructor-arg type="java.lang.String">
                    <value><xsl:choose><xsl:when test="@name"><xsl:value-of select="@name" /></xsl:when><xsl:otherwise><xsl:value-of select="uuid:randomUUID()" /></xsl:otherwise></xsl:choose></value>
                  </constructor-arg>
                  <constructor-arg type="java.lang.Object">
                    <xsl:choose>
                      <xsl:when test="@value">
                        <xsl:variable name="valueClass">
                          <xsl:choose>  
                            <xsl:when test="@class and @type">
                              <xsl:message terminate="yes">Please specify only a class or a type!</xsl:message>
                            </xsl:when>
                            <xsl:when test="@class"><xsl:value-of select="@class" /></xsl:when>
                            <xsl:when test="@type"><xsl:value-of select="mdef:determineTypeClass(@type)" /></xsl:when>
                            <xsl:otherwise><xsl:value-of select="mdef:determineValueClass(@value)" /></xsl:otherwise>
                          </xsl:choose>
                        </xsl:variable>
                        
                        <value type="{$valueClass}"><xsl:value-of select="@value" /></value>
                      </xsl:when>
                      <xsl:otherwise><null /></xsl:otherwise>
                    </xsl:choose>
                  </constructor-arg>
                </bean>
              </xsl:for-each>
            </array>
          </constructor-arg>
        </bean>
      </xsl:for-each>
      
      <!-- add the dataSets getting the data from dataRetrievers -->
      <xsl:for-each select="mns:data/mns:dataset[@dataretriever]">
        <xsl:variable name="dataretriever" select="@dataretriever" />

        <bean class="net.meisen.dissertation.impl.datasets.DataRetrieverDataSet">
          <constructor-arg type="net.meisen.dissertation.model.dataretriever.BaseDataRetriever">
              <ref bean="dataretriever-{$dataretriever}" />
          </constructor-arg>
          <constructor-arg type="net.meisen.dissertation.model.dataretriever.IQueryConfiguration">
            <xsl:choose>
              <xsl:when test="node()"><xsl:apply-imports /></xsl:when>
              <xsl:otherwise><null /></xsl:otherwise>
            </xsl:choose>
          </constructor-arg>
        </bean>
      </xsl:for-each>
      
      <!-- create the dataRecordFactory -->
      <bean id="{$dataRecordFactoryId}" class="net.meisen.dissertation.impl.indexes.datarecord.IndexedDataRecordFactory" />
      
      <!-- create the tidaModel -->      
      <bean id="{$tidaModelId}" class="net.meisen.dissertation.model.data.TidaModel">
        <constructor-arg type="java.lang.String" value="{$modelId}" />
        <constructor-arg type="java.lang.String" value="{$modelName}" />
        <constructor-arg type="java.io.File">
          <bean class="java.io.File">
            <xsl:choose>
              <xsl:when test="normalize-space($modelLocation) = ''">
                <constructor-arg type="java.lang.String" ref="defaultLocation" />
                <constructor-arg type="java.lang.String" value="{$modelId}" />             
              </xsl:when>
              <xsl:otherwise>
                <constructor-arg type="java.lang.String" value="{$modelLocation}" />
              </xsl:otherwise>
            </xsl:choose>
          </bean>
        </constructor-arg>
        
        <property name="metaDataHandlingByString" value="{$metahandling}" />
        <property name="intervalDataHandlingByString" value="{$intervalhandling}" />
      	<property name="offlineModeByString" value="{$offlinemode}" />
      </bean>
    </beans>
  </xsl:template>
    
  <!--
    Template to create a descriptorModel which contains descriptors of the specified class
    -->
  <xsl:template name="beanDescriptorModel">
    <xsl:param name="class"/>
    
    <xsl:variable name="id" select="@id" />
    <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="@name"><xsl:value-of select="@name"/></xsl:when>
        <xsl:otherwise><xsl:value-of select="@id"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="supportsNullDescriptor">
      <xsl:choose>
        <xsl:when test="@null"><xsl:value-of select="@null"/></xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="failOnDuplicates">
      <xsl:choose>
        <xsl:when test="@failonduplicates"><xsl:value-of select="@failonduplicates"/></xsl:when>
        <xsl:otherwise>true</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="idFactory">
      <xsl:choose>
        <xsl:when test="@idfactory"><xsl:value-of select="@idfactory"/></xsl:when>
        <xsl:otherwise><xsl:value-of select="mdef:getDefaultIdFactory()"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="offlinemode">
      <xsl:choose>
        <xsl:when test="//@offlinemode"><xsl:value-of select="//@offlinemode"/></xsl:when>
        <xsl:otherwise></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    
    <bean id="descriptormodel-{$id}" class="net.meisen.dissertation.model.descriptors.DescriptorModel">
      <constructor-arg type="java.lang.String" value="{$id}" />
      <constructor-arg type="java.lang.String" value="{$name}" />
      <constructor-arg type="java.lang.Class" value="{$class}" />
      <constructor-arg type="net.meisen.dissertation.model.idfactories.IIdsFactory"><bean class="{$idFactory}" /></constructor-arg>
      
      <property name="supportsNullDescriptor" value="{$supportsNullDescriptor}" />
      <property name="failOnDuplicates" value="{$failOnDuplicates}" />
      <property name="offlineModeByString" value="{$offlinemode}" />
    </bean>
  </xsl:template>
  
  <!--
    Template to create the common constructor values of a StructureEntry
    -->
  <xsl:template name="beanStructureEntryConstructorArgs">
    <xsl:if test="not(@name) and not(@position)">
      <xsl:message terminate="yes">A structure must have at least a name or a position.</xsl:message>
    </xsl:if>
  
    <constructor-arg type="int">
      <xsl:choose>
        <xsl:when test="@position"><value><xsl:value-of select="@position" /></value></xsl:when>
        <xsl:otherwise><value>-1</value></xsl:otherwise>
      </xsl:choose>
    </constructor-arg>
    <constructor-arg type="java.lang.String">
      <xsl:choose>
        <xsl:when test="@name"><value><xsl:value-of select="@name" /></value></xsl:when>
        <xsl:otherwise><null /></xsl:otherwise>
      </xsl:choose>
    </constructor-arg>
  </xsl:template>
  
  <!--
    Add the default DescriptorModels
    -->
  <xsl:template match="mns:object">
    <xsl:call-template name="beanDescriptorModel">
      <xsl:with-param name="class">net.meisen.dissertation.impl.descriptors.GeneralDescriptor</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template match="mns:string">
    <xsl:call-template name="beanDescriptorModel">
      <xsl:with-param name="class">net.meisen.dissertation.impl.descriptors.GeneralDescriptor</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template match="mns:double">
    <xsl:call-template name="beanDescriptorModel">
      <xsl:with-param name="class">net.meisen.dissertation.impl.descriptors.DoubleDescriptor</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template match="mns:long">
    <xsl:call-template name="beanDescriptorModel">
      <xsl:with-param name="class">net.meisen.dissertation.impl.descriptors.LongDescriptor</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template match="mns:integer">
    <xsl:call-template name="beanDescriptorModel">
      <xsl:with-param name="class">net.meisen.dissertation.impl.descriptors.IntegerDescriptor</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
</xsl:stylesheet>