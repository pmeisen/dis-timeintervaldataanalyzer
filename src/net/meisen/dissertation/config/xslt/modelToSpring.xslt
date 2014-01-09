<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0"
        xmlns="http://www.springframework.org/schema/beans"
        xmlns:mns="http://dev.meisen.net/xsd/dissertation/model" 
        xmlns:uuid="java.util.UUID"
        xmlns:mdef="net.meisen.dissertation.config.xslt.DefaultValues"
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="dataretriever://includeXslts" />

  <xsl:output method="xml" indent="yes" />

  <xsl:template match="/mns:model">
    <xsl:variable name="generatedModuleName" select="mdef:getGeneratedModuleName()" />
    <xsl:variable name="uniqueIdPrefix" select="uuid:randomUUID()" />
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
    <xsl:variable name="indexedFactory">
      <xsl:choose>
        <xsl:when test="//mns:config/mns:factories/mns:descriptor/@implementation"><xsl:value-of select="//mns:config/mns:factories/mns:indexes/@implementation" /></xsl:when>
        <xsl:otherwise><xsl:value-of select="mdef:getIndexedCollectionFactoryImplementation()" /></xsl:otherwise>
      </xsl:choose>
    </xsl:variable> 
    <xsl:variable name="descriptorsFactory">
      <xsl:choose>
        <xsl:when test="//mns:config/mns:factories/mns:descriptor/@implementation"><xsl:value-of select="//mns:config/mns:factories/mns:descriptor/@implementation" /></xsl:when>
        <xsl:otherwise><xsl:value-of select="mdef:getDescriptorsFactoryImplementation()" /></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>    
    <xsl:variable name="resourcesFactory">
      <xsl:choose>
        <xsl:when test="//mns:config/mns:factories/mns:resource/@implementation"><xsl:value-of select="//mns:config/mns:factories/mns:resource/@implementation" /></xsl:when>
        <xsl:otherwise><xsl:value-of select="mdef:getResourcesFactoryImplementation()" /></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="descriptorsFactoryIdFactory">
      <xsl:choose>
        <xsl:when test="//mns:config/mns:factories/mns:descriptor/@idfactory"><xsl:value-of select="//mns:config/mns:factories/mns:descriptor/@idfactory" /></xsl:when>
        <xsl:otherwise><xsl:value-of select="mdef:getResourcesFactoryIdFactory()" /></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="resourcesFactoryIdFactory">
      <xsl:choose>
        <xsl:when test="//mns:config/mns:factories/mns:resource/@idfactory"><xsl:value-of select="//mns:config/mns:factories/mns:resource/@idfactory" /></xsl:when>
        <xsl:otherwise><xsl:value-of select="mdef:getResourcesFactoryIdFactory()" /></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
  
    <beans xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans-3.0.xsd
                               http://www.springframework.org/schema/util http://www.springframework.org/schema/util/spring-util-2.0.xsd">
      
      <bean id="indexedFactory-{$uniqueIdPrefix}" class="{$indexedFactory}" />
      
      <bean id="descriptorsFactory-{$uniqueIdPrefix}" class="{$descriptorsFactory}">
        <constructor-arg type="net.meisen.dissertation.data.IIdsFactory">
          <bean class="{$descriptorsFactoryIdFactory}" />
        </constructor-arg>
        <constructor-arg type="java.util.Map">
          <map key-type="java.lang.Class" value-type="java.lang.Class">
            <xsl:call-template name="mapToEntries">
              <xsl:with-param name="list" select="mdef:getCsv('getDescriptors', ',', '|')" />
              <xsl:with-param name="itemSeparator" select="','" />
              <xsl:with-param name="keyValueSeparator" select="'|'" />
            </xsl:call-template>

            <xsl:for-each select="mns:config/mns:descriptors/mns:descriptor">
              <xsl:variable name="dataType"><xsl:value-of select="@dataType"/></xsl:variable>
              <xsl:variable name="implementation"><xsl:value-of select="@implementation"/></xsl:variable>
              
              <entry key="{$dataType}" value="{$implementation}" />
            </xsl:for-each>
          </map>
        </constructor-arg>
      </bean>
         
      <xsl:for-each select="mns:config/mns:dataretrievers/mns:dataretriever">
        <xsl:call-template name="beanDataRetriever">
          <xsl:with-param name="uniqueIdPrefix"><xsl:value-of select="$uniqueIdPrefix" /></xsl:with-param>
        </xsl:call-template>
      </xsl:for-each>
      
      <bean id="resourcesFactory-{$uniqueIdPrefix}" class="{$resourcesFactory}">
        <constructor-arg type="net.meisen.dissertation.data.IIdsFactory">
          <bean class="{$resourcesFactoryIdFactory}" />
        </constructor-arg>
      </bean>

      <xsl:for-each select="mns:meta/mns:resources/mns:resource">
        <xsl:call-template name="beanResourceModel">
          <xsl:with-param name="uniqueIdPrefix"><xsl:value-of select="$uniqueIdPrefix" /></xsl:with-param>
        </xsl:call-template>
      </xsl:for-each>
      
      <xsl:for-each select="mns:meta/mns:descriptors/mns:descriptor">
        <xsl:call-template name="beanDescriptorModel">
          <xsl:with-param name="uniqueIdPrefix"><xsl:value-of select="$uniqueIdPrefix" /></xsl:with-param>
        </xsl:call-template>
      </xsl:for-each>
      
      <bean id="resources-{$uniqueIdPrefix}" class="net.meisen.general.sbconfigurator.factories.MergedCollection">
        <property name="collections">
          <list value-type="java.util.Collection">
        
            <list value-type="net.meisen.dissertation.models.impl.data.Resource">
              <xsl:for-each select="mns:meta/mns:values/mns:resource[@value]">
                <xsl:call-template name="beanResource">
                  <xsl:with-param name="uniqueIdPrefix"><xsl:value-of select="$uniqueIdPrefix" /></xsl:with-param>
                </xsl:call-template>
              </xsl:for-each>
            </list>
          
            <xsl:for-each select="mns:meta/mns:values/mns:resource[@dataretriever]">
              <xsl:call-template name="beanDataRetrieverResource">
                <xsl:with-param name="uniqueIdPrefix"><xsl:value-of select="$uniqueIdPrefix" /></xsl:with-param>
              </xsl:call-template>
            </xsl:for-each>
          </list>
        </property>
      </bean>
      
      <bean id="descriptors-{$uniqueIdPrefix}" class="net.meisen.general.sbconfigurator.factories.MergedCollection">
        <property name="collections">
          <list value-type="java.util.Collection">
            
            <list value-type="net.meisen.dissertation.models.impl.data.Descriptor">
              <xsl:for-each select="mns:meta/mns:values/mns:descriptor[@value]">
                <xsl:call-template name="beanDescriptor">
                  <xsl:with-param name="uniqueIdPrefix"><xsl:value-of select="$uniqueIdPrefix" /></xsl:with-param>
                </xsl:call-template>
              </xsl:for-each>
            </list>
          
            <xsl:for-each select="mns:meta/mns:values/mns:descriptor[@dataretriever]">
              <xsl:call-template name="beanDataRetrieverDescriptor">
                <xsl:with-param name="uniqueIdPrefix"><xsl:value-of select="$uniqueIdPrefix" /></xsl:with-param>
              </xsl:call-template>
            </xsl:for-each>
          </list>
        </property>
      </bean>
      
      <bean id="{$generatedModuleName}" class="net.meisen.dissertation.models.impl.data.MetaDataModel">
        <constructor-arg type="java.lang.String"><value><xsl:value-of select="$modelId" /></value></constructor-arg>
        <constructor-arg type="java.lang.String"><value><xsl:value-of select="$modelName" /></value></constructor-arg>
        <constructor-arg type="java.util.Collection">
          <list value-type="net.meisen.dissertation.models.impl.data.ResourceModel">
            <xsl:for-each select="mns:meta/mns:resources/mns:resource">
              <xsl:call-template name="beanReferenceResourceModel">
                <xsl:with-param name="uniqueIdPrefix"><xsl:value-of select="$uniqueIdPrefix" /></xsl:with-param>
              </xsl:call-template>
            </xsl:for-each>
          </list>
        </constructor-arg>
        <constructor-arg type="java.util.Collection">
          <list value-type="net.meisen.dissertation.models.impl.data.DescriptorModel">
            <xsl:for-each select="mns:meta/mns:descriptors/mns:descriptor">
              <xsl:call-template name="beanReferenceDescriptorModel">
                <xsl:with-param name="uniqueIdPrefix"><xsl:value-of select="$uniqueIdPrefix" /></xsl:with-param>
              </xsl:call-template>
            </xsl:for-each>
          </list>
        </constructor-arg>
        <constructor-arg type="net.meisen.dissertation.data.impl.resources.ResourcesFactory">
          <ref bean="resourcesFactory-{$uniqueIdPrefix}" />
        </constructor-arg>
        <constructor-arg type="net.meisen.dissertation.data.impl.descriptors.DescriptorsFactory">
          <ref bean="descriptorsFactory-{$uniqueIdPrefix}" />
        </constructor-arg>
        <constructor-arg type="net.meisen.dissertation.data.impl.indexes.IndexedCollectionFactory">
          <ref bean="indexedFactory-{$uniqueIdPrefix}" />
        </constructor-arg>
        <constructor-arg type="java.util.Collection">
          <ref bean="resources-{$uniqueIdPrefix}" />
        </constructor-arg>
        <constructor-arg type="java.util.Collection">
          <ref bean="descriptors-{$uniqueIdPrefix}" />
        </constructor-arg>
      </bean>
    </beans>
  </xsl:template>

  <xsl:template name="beanResourceModel">
    <xsl:param name="uniqueIdPrefix"></xsl:param>
  
    <xsl:variable name="id">
      <xsl:value-of select="@id"/>
    </xsl:variable>
    <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="@name"><xsl:value-of select="@name"/></xsl:when>
        <xsl:otherwise><xsl:value-of select="@id"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    
    <bean id="resourcemodel-{$uniqueIdPrefix}-{$id}" class="net.meisen.dissertation.models.impl.data.ResourceModel">
      <constructor-arg type="java.lang.String"><value><xsl:value-of select="$id" /></value></constructor-arg>
      <constructor-arg type="java.lang.String"><value><xsl:value-of select="$name" /></value></constructor-arg>
    </bean>
  </xsl:template>
  
  <xsl:template name="beanReferenceResourceModel">
    <xsl:param name="uniqueIdPrefix"></xsl:param>
  
    <xsl:variable name="id"><xsl:value-of select="@id"/></xsl:variable>
    
    <ref bean="resourcemodel-{$uniqueIdPrefix}-{$id}" />
  </xsl:template>
  
  <xsl:template name="beanDescriptorModel">
    <xsl:param name="uniqueIdPrefix"></xsl:param>
  
    <xsl:variable name="id">
      <xsl:value-of select="@id"/>
    </xsl:variable>
    <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="@name"><xsl:value-of select="@name"/></xsl:when>
        <xsl:otherwise><xsl:value-of select="@id"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="dataType">
      <xsl:value-of select="@dataType"/>
    </xsl:variable>
    
    <bean id="descriptormodel-{$uniqueIdPrefix}-{$id}" class="net.meisen.dissertation.models.impl.data.DescriptorModel">
      <constructor-arg type="java.lang.String"><value><xsl:value-of select="$id" /></value></constructor-arg>
      <constructor-arg type="java.lang.String"><value><xsl:value-of select="$name" /></value></constructor-arg>
      <constructor-arg type="java.lang.Class"><value><xsl:value-of select="$dataType" /></value></constructor-arg>
    </bean>
  </xsl:template>
  
  <xsl:template name="beanReferenceDescriptorModel">
    <xsl:param name="uniqueIdPrefix"></xsl:param>
  
    <xsl:variable name="id"><xsl:value-of select="@id"/></xsl:variable>
    
    <ref bean="descriptormodel-{$uniqueIdPrefix}-{$id}" />
  </xsl:template>
  
  <xsl:template name="beanResource">
    <xsl:param name="uniqueIdPrefix"></xsl:param>
    
    <xsl:variable name="model" select="@model" />
    
    <bean factory-bean="resourcesFactory-{$uniqueIdPrefix}" factory-method="createResource">
      <constructor-arg type="net.meisen.dissertation.models.impl.data.ResourceModel">
        <ref bean="resourcemodel-{$uniqueIdPrefix}-{$model}" />
      </constructor-arg>
      <constructor-arg type="java.lang.String">
        <value><xsl:value-of select="@value"/></value>
      </constructor-arg>
    </bean>
  </xsl:template>
  
  <xsl:template name="beanDataRetrieverResource">
    <xsl:param name="uniqueIdPrefix"></xsl:param>
    
    <xsl:variable name="model" select="@model" />
    <xsl:variable name="dataretriever" select="@dataretriever" />
    
    <bean factory-bean="resourcesFactory-{$uniqueIdPrefix}" factory-method="createResources">
      <constructor-arg type="net.meisen.dissertation.models.impl.data.ResourceModel">
        <ref bean="resourcemodel-{$uniqueIdPrefix}-{$model}" />
      </constructor-arg>
      <constructor-arg type="java.util.Collection">
        <bean class="net.meisen.general.sbconfigurator.factories.MethodInvokingFactoryBean">
          <property name="targetMethod" value="transform" />
          <property name="targetObject">
            <bean factory-bean="dataretriever-{$uniqueIdPrefix}-{$dataretriever}" factory-method="retrieve">
              <constructor-arg type="net.meisen.dissertation.models.impl.dataretriever.IQueryConfiguration">
                <xsl:choose>
                  <xsl:when test="node()"><xsl:apply-imports /></xsl:when>
                  <xsl:otherwise><null /></xsl:otherwise>
                </xsl:choose>
              </constructor-arg>
            </bean>
          </property>
          
          <property name="postExecutionMethod" value="release" />
        </bean>
      </constructor-arg>
    </bean>
  </xsl:template>
  
  <xsl:template name="beanDescriptor">
    <xsl:param name="uniqueIdPrefix"></xsl:param>
    
    <xsl:variable name="model" select="@model" />
    
    <bean factory-bean="descriptorsFactory-{$uniqueIdPrefix}" factory-method="createDescriptor">
      <constructor-arg type="net.meisen.dissertation.models.impl.data.DescriptorModel">
        <ref bean="descriptormodel-{$uniqueIdPrefix}-{$model}" />
      </constructor-arg>
      <constructor-arg type="java.lang.Object">
        <value><xsl:value-of select="@value"/></value>
      </constructor-arg>
    </bean>
  </xsl:template>
  
  <xsl:template name="beanDataRetrieverDescriptor">
    <xsl:param name="uniqueIdPrefix"></xsl:param>
    
    <xsl:variable name="model" select="@model" />
    <xsl:variable name="dataretriever" select="@dataretriever" />
    
    <bean factory-bean="descriptorsFactory-{$uniqueIdPrefix}" factory-method="createDescriptors">
      <constructor-arg type="net.meisen.dissertation.models.impl.data.DescriptorModel">
        <ref bean="descriptormodel-{$uniqueIdPrefix}-{$model}" />
      </constructor-arg>
      <constructor-arg type="java.util.Collection">
        <bean class="net.meisen.general.sbconfigurator.factories.MethodInvokingFactoryBean">
          <property name="targetMethod" value="transform" />
          <property name="targetObject">
            <bean factory-bean="dataretriever-{$uniqueIdPrefix}-{$dataretriever}" factory-method="retrieve">
              <constructor-arg type="net.meisen.dissertation.models.impl.dataretriever.IQueryConfiguration">
                <xsl:choose>
                  <xsl:when test="node()"><xsl:apply-imports /></xsl:when>
                  <xsl:otherwise><null /></xsl:otherwise>
                </xsl:choose>
              </constructor-arg>
            </bean>
          </property>
          
          <property name="postExecutionMethod" value="release" />
        </bean>
      </constructor-arg>
    </bean>
  </xsl:template>
  
  <xsl:template name="beanDataRetriever">
    <xsl:param name="uniqueIdPrefix"></xsl:param>

    <xsl:variable name="id" select="@id" />
    
    <!-- get the implementation of the dataretriever -->  
    <xsl:variable name="implementation">
      <xsl:choose>
        <xsl:when test="@implementation"><xsl:value-of select="@implementation" /></xsl:when>
        <xsl:otherwise><xsl:value-of select="mdef:getDefaultDataRetrieverImplementation(@id)"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
        
    <!-- define the bean -->
    <bean id="dataretriever-{$uniqueIdPrefix}-{$id}" class="{$implementation}" destroy-method="release">
      <constructor-arg type="net.meisen.dissertation.models.impl.dataretriever.IDataRetrieverConfig">
        <xsl:choose>
          <xsl:when test="node()"><xsl:apply-imports /></xsl:when>
          <xsl:otherwise><null /></xsl:otherwise>
        </xsl:choose>
      </constructor-arg>
    </bean>
  </xsl:template>
    
  <xsl:template name="mapToEntries">
    <xsl:param name="list" />
    <xsl:param name="itemSeparator" />
    <xsl:param name="keyValueSeparator" />
    
    <xsl:variable name="item" select="normalize-space(substring-before(concat($list, $itemSeparator), $itemSeparator))" />
       
    <xsl:if test="$item">
      <xsl:variable name="key" select="substring-before($item, $keyValueSeparator)" />
      <xsl:variable name="value" select="substring-after($item, $keyValueSeparator)" />
    
      <entry key="{$key}" value="{$value}" />

      <xsl:call-template name="mapToEntries">
        <xsl:with-param name="list" select="substring-after($list, $itemSeparator)" />
        <xsl:with-param name="itemSeparator" select="$itemSeparator" />
        <xsl:with-param name="keyValueSeparator" select="$keyValueSeparator" />
      </xsl:call-template>
    </xsl:if>
  </xsl:template>
  
</xsl:stylesheet>