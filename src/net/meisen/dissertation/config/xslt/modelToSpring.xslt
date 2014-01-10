<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0"
        xmlns="http://www.springframework.org/schema/beans"
        xmlns:mns="http://dev.meisen.net/xsd/dissertation/model" 
        xmlns:uuid="java.util.UUID"
        xmlns:mdef="net.meisen.dissertation.config.xslt.DefaultValues"
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="dataretriever://includeXslts" />

  <xsl:output method="xml" indent="yes" />
  
  <xsl:variable name="metaDataModelId" select="mdef:getId('METADATAMODEL_ID')" />
  <xsl:variable name="resourcesFactoryId" select="mdef:getId('RESOURCESFACTORY_ID')" />
  <xsl:variable name="descriptorsFactoryId" select="mdef:getId('DESCRIPTORSFACTORY_ID')" />
  <xsl:variable name="indexFactoryId" select="mdef:getId('INDEXFACTORY_ID')" />

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
    <xsl:variable name="indexedFactory">
      <xsl:choose>
        <xsl:when test="//mns:config/mns:factories/mns:descriptor/@implementation"><xsl:value-of select="//mns:config/mns:factories/mns:indexes/@implementation" /></xsl:when>
        <xsl:otherwise><xsl:value-of select="mdef:getIndexedCollectionFactoryImplementation()" /></xsl:otherwise>
      </xsl:choose>
    </xsl:variable> 
    <xsl:variable name="descriptorsFactoryClass">
      <xsl:choose>
        <xsl:when test="//mns:config/mns:factories/mns:descriptor/@implementation"><xsl:value-of select="//mns:config/mns:factories/mns:descriptor/@implementation" /></xsl:when>
        <xsl:otherwise><xsl:value-of select="mdef:getDescriptorsFactoryImplementation()" /></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>    
    <xsl:variable name="resourcesFactoryClass">
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
      
      <bean id="{$indexFactoryId}" class="{$indexedFactory}" />
      
      <bean id="{$descriptorsFactoryId}" class="{$descriptorsFactoryClass}">
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
        <xsl:call-template name="beanDataRetriever" />
      </xsl:for-each>
      
      <bean id="{$resourcesFactoryId}" class="{$resourcesFactoryClass}">
        <constructor-arg type="net.meisen.dissertation.data.IIdsFactory">
          <bean class="{$resourcesFactoryIdFactory}" />
        </constructor-arg>
      </bean>

      <xsl:for-each select="mns:meta/mns:resources/mns:resource">
        <xsl:call-template name="beanResourceModel" />
      </xsl:for-each>
      
      <xsl:for-each select="mns:meta/mns:descriptors/mns:descriptor">
        <xsl:call-template name="beanDescriptorModel" />
      </xsl:for-each>
      
      <bean id="resources" class="net.meisen.general.sbconfigurator.factories.MergedCollection">
        <property name="collections">
          <list value-type="java.util.Collection">
        
            <list value-type="net.meisen.dissertation.models.impl.data.Resource">
              <xsl:for-each select="mns:meta/mns:values/mns:resource[@value]">
                <xsl:call-template name="beanResource" />
              </xsl:for-each>
            </list>
          
            <xsl:for-each select="mns:meta/mns:values/mns:resource[@dataretriever]">
              <xsl:call-template name="beanDataRetrieverResource" />
            </xsl:for-each>
          </list>
        </property>
      </bean>
      
      <bean id="descriptors" class="net.meisen.general.sbconfigurator.factories.MergedCollection">
        <property name="collections">
          <list value-type="java.util.Collection">
            
            <list value-type="net.meisen.dissertation.models.impl.data.Descriptor">
              <xsl:for-each select="mns:meta/mns:values/mns:descriptor[@value]">
                <xsl:call-template name="beanDescriptor" />
              </xsl:for-each>
            </list>
          
            <xsl:for-each select="mns:meta/mns:values/mns:descriptor[@dataretriever]">
              <xsl:call-template name="beanDataRetrieverDescriptor" />
            </xsl:for-each>
          </list>
        </property>
      </bean>
      
      <!-- generate the MetaDataModel -->
      <bean id="{$metaDataModelId}" class="net.meisen.dissertation.models.impl.data.MetaDataModel" init-method="init">
        <constructor-arg type="java.lang.String"><value><xsl:value-of select="$modelId" /></value></constructor-arg>
        <constructor-arg type="java.lang.String"><value><xsl:value-of select="$modelName" /></value></constructor-arg>
      </bean>
      
      <bean class="net.meisen.dissertation.models.impl.data.MetaData">
        <constructor-arg type="java.util.Collection">
          <list value-type="net.meisen.dissertation.models.impl.data.ResourceModel">
            <xsl:for-each select="mns:meta/mns:resources/mns:resource">
              <xsl:call-template name="beanReferenceResourceModel" />
            </xsl:for-each>
          </list>
        </constructor-arg>
        <constructor-arg type="java.util.Collection">
          <list value-type="net.meisen.dissertation.models.impl.data.DescriptorModel">
            <xsl:for-each select="mns:meta/mns:descriptors/mns:descriptor">
              <xsl:call-template name="beanReferenceDescriptorModel" />
            </xsl:for-each>
          </list>
        </constructor-arg>
        <constructor-arg type="java.util.Collection">
          <ref bean="resources"/>
        </constructor-arg>
        <constructor-arg type="java.util.Collection">
          <ref bean="descriptors"/>
        </constructor-arg>
      </bean>
    </beans>
  </xsl:template>

  <xsl:template name="beanResourceModel">

    <xsl:variable name="id">
      <xsl:value-of select="@id"/>
    </xsl:variable>
    <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="@name"><xsl:value-of select="@name"/></xsl:when>
        <xsl:otherwise><xsl:value-of select="@id"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    
    <bean id="resourcemodel-{$id}" class="net.meisen.dissertation.models.impl.data.ResourceModel">
      <constructor-arg type="java.lang.String"><value><xsl:value-of select="$id" /></value></constructor-arg>
      <constructor-arg type="java.lang.String"><value><xsl:value-of select="$name" /></value></constructor-arg>
    </bean>
  </xsl:template>
  
  <xsl:template name="beanReferenceResourceModel">
    <xsl:variable name="id"><xsl:value-of select="@id"/></xsl:variable>
    
    <ref bean="resourcemodel-{$id}" />
  </xsl:template>
  
  <xsl:template name="beanDescriptorModel">
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
    
    <bean id="descriptormodel-{$id}" class="net.meisen.dissertation.models.impl.data.DescriptorModel">
      <constructor-arg type="java.lang.String"><value><xsl:value-of select="$id" /></value></constructor-arg>
      <constructor-arg type="java.lang.String"><value><xsl:value-of select="$name" /></value></constructor-arg>
      <constructor-arg type="java.lang.Class"><value><xsl:value-of select="$dataType" /></value></constructor-arg>
    </bean>
  </xsl:template>
  
  <xsl:template name="beanReferenceDescriptorModel">
    <xsl:variable name="id"><xsl:value-of select="@id"/></xsl:variable>
    
    <ref bean="descriptormodel-{$id}" />
  </xsl:template>
  
  <xsl:template name="beanResource">
    <xsl:variable name="model" select="@model" />
    
    <bean factory-bean="{$resourcesFactoryId}" factory-method="createResource">
      <constructor-arg type="net.meisen.dissertation.models.impl.data.ResourceModel">
        <ref bean="resourcemodel-{$model}" />
      </constructor-arg>
      <constructor-arg type="java.lang.String">
        <value><xsl:value-of select="@value"/></value>
      </constructor-arg>
    </bean>
  </xsl:template>
  
  <xsl:template name="beanDataRetrieverResource">
    <xsl:variable name="model" select="@model" />
    <xsl:variable name="dataretriever" select="@dataretriever" />
    
    <bean factory-bean="{$resourcesFactoryId}" factory-method="createResources">
      <constructor-arg type="net.meisen.dissertation.models.impl.data.ResourceModel">
        <ref bean="resourcemodel-{$model}" />
      </constructor-arg>
      <constructor-arg type="java.util.Collection">
        <bean class="net.meisen.general.sbconfigurator.factories.MethodInvokingFactoryBean">
          <property name="targetMethod" value="transform" />
          <property name="targetObject">
            <bean factory-bean="dataretriever-{$dataretriever}" factory-method="retrieve">
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
    <xsl:variable name="model" select="@model" />
    
    <bean factory-bean="{$descriptorsFactoryId}" factory-method="createDescriptor">
      <constructor-arg type="net.meisen.dissertation.models.impl.data.DescriptorModel">
        <ref bean="descriptormodel-{$model}" />
      </constructor-arg>
      <constructor-arg type="java.lang.Object">
        <value><xsl:value-of select="@value"/></value>
      </constructor-arg>
    </bean>
  </xsl:template>
  
  <xsl:template name="beanDataRetrieverDescriptor">
    
    <xsl:variable name="model" select="@model" />
    <xsl:variable name="dataretriever" select="@dataretriever" />
    
    <bean factory-bean="{$descriptorsFactoryId}" factory-method="createDescriptors">
      <constructor-arg type="net.meisen.dissertation.models.impl.data.DescriptorModel">
        <ref bean="descriptormodel-{$model}" />
      </constructor-arg>
      <constructor-arg type="java.util.Collection">
        <bean class="net.meisen.general.sbconfigurator.factories.MethodInvokingFactoryBean">
          <property name="targetMethod" value="transform" />
          <property name="targetObject">
            <bean factory-bean="dataretriever-{$dataretriever}" factory-method="retrieve">
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

    <xsl:variable name="id" select="@id" />
    
    <!-- get the implementation of the dataretriever -->  
    <xsl:variable name="implementation">
      <xsl:choose>
        <xsl:when test="@implementation"><xsl:value-of select="@implementation" /></xsl:when>
        <xsl:otherwise><xsl:value-of select="mdef:getDefaultDataRetrieverImplementation(@id)"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
        
    <!-- define the bean -->
    <bean id="dataretriever-{$id}" class="{$implementation}" destroy-method="release">
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