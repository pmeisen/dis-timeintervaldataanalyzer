<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0"
        xmlns="http://www.springframework.org/schema/beans"
        xmlns:mns="http://dev.meisen.net/xsd/dissertation/model" 
        xmlns:uuid="java.util.UUID"
        xmlns:mdef="net.meisen.dissertation.config.xslt.DefaultValues"
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="descriptors://includeXslts" />
  <xsl:import href="dataretriever://includeXslts" />

  <xsl:output method="xml" indent="yes" />
  
  <xsl:variable name="metaDataModelId" select="mdef:getId('METADATAMODEL_ID')" />
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
        <xsl:when test="//mns:config/mns:factories/mns:indexes/@implementation"><xsl:value-of select="//mns:config/mns:factories/mns:indexes/@implementation" /></xsl:when>
        <xsl:otherwise><xsl:value-of select="mdef:getIndexedCollectionFactoryImplementation()" /></xsl:otherwise>
      </xsl:choose>
    </xsl:variable> 
  
    <beans xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans-3.0.xsd
                               http://www.springframework.org/schema/util http://www.springframework.org/schema/util/spring-util-2.0.xsd">
      
      <!-- create the IndexFactory to be used -->
      <bean id="{$indexFactoryId}" class="{$indexedFactory}" />
      
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
          <constructor-arg type="net.meisen.dissertation.models.impl.dataretriever.IDataRetrieverConfig">
            <xsl:choose>
              <xsl:when test="node()"><xsl:apply-imports /></xsl:when>
              <xsl:otherwise><null /></xsl:otherwise>
            </xsl:choose>
          </constructor-arg>
        </bean>
      </xsl:for-each>
      
      <!-- create all the descriptorModels -->
      <xsl:for-each select="mns:meta/mns:descriptors">
        <xsl:apply-templates />
      </xsl:for-each>
      
      <!-- create the descriptors using the model as factory -->
      <bean id="descriptors" class="net.meisen.general.sbconfigurator.factories.MergedCollection">
        <property name="collections">
          <list value-type="java.util.Collection">
            
            <list value-type="net.meisen.dissertation.models.impl.data.Descriptor">
              <xsl:for-each select="mns:meta/mns:entries/mns:entry[@value]">
                <xsl:variable name="descriptorValue" select="@value" />
                <xsl:variable name="descriptorModel" select="@descriptor" />
                
                <bean factory-bean="descriptormodel-{$descriptorModel}" factory-method="createDescriptor">
                  <constructor-arg type="java.lang.Object" value="{$descriptorValue}" />
                </bean>
              </xsl:for-each>
            </list>
          
            <xsl:for-each select="mns:meta/mns:entries/mns:entry[@dataretriever]">
              <xsl:variable name="descriptorModel" select="@descriptor" />
              <xsl:variable name="dataretriever" select="@dataretriever" />
              
              <bean factory-bean="descriptormodel-{$descriptorModel}" factory-method="createDescriptors">
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
            </xsl:for-each>
          </list>
        </property>
      </bean>
      
      <!-- generate the MetaDataModel -->
      <bean id="{$metaDataModelId}" class="net.meisen.dissertation.models.impl.data.MetaDataModel">
        <constructor-arg type="java.lang.String" value="{$modelId}" />
        <constructor-arg type="java.lang.String" value="{$modelName}" />
      </bean>
      
      <!-- add the descriptorModels to the MetaDataModel -->
      <bean class="net.meisen.general.sbconfigurator.factories.MethodExecutorBean">
        <property name="targetMethod" value="addDescriptorModels" />
        <property name="targetObject" ref="{$metaDataModelId}" />

        <property name="type" value="init" />
        
        <property name="arguments">
          <list value-type="net.meisen.dissertation.models.impl.data.DescriptorModel">
            <xsl:for-each select="mns:meta/mns:descriptors/*">
              <xsl:variable name="id" select="@id" />
              <ref bean="descriptormodel-{$id}" />
            </xsl:for-each>    
          </list>
        </property>
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
    <xsl:variable name="idFactory">
      <xsl:choose>
        <xsl:when test="@idfactory"><xsl:value-of select="@idfactory"/></xsl:when>
        <xsl:otherwise><xsl:value-of select="mdef:getDefaultIdFactory()"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    
    <bean id="descriptormodel-{$id}" class="net.meisen.dissertation.models.impl.data.DescriptorModel">
      <constructor-arg type="java.lang.String" value="{$id}" />
      <constructor-arg type="java.lang.String" value="{$name}" />
      <constructor-arg type="java.lang.Class" value="{$class}" />
      <constructor-arg type="net.meisen.dissertation.models.IIdsFactory"><bean class="{$idFactory}" /></constructor-arg>
    </bean>
  </xsl:template>
  
  <!--
    Add the default DescriptorModels
    -->
  <xsl:template match="mns:object">
    <xsl:call-template name="beanDescriptorModel">
      <xsl:with-param name="class">net.meisen.dissertation.data.impl.descriptors.GeneralDescriptor</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template match="mns:string">
    <xsl:call-template name="beanDescriptorModel">
      <xsl:with-param name="class">net.meisen.dissertation.data.impl.descriptors.GeneralDescriptor</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template match="mns:double">
    <xsl:call-template name="beanDescriptorModel">
      <xsl:with-param name="class">net.meisen.dissertation.data.impl.descriptors.DoubleDescriptor</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template match="mns:long">
    <xsl:call-template name="beanDescriptorModel">
      <xsl:with-param name="class">net.meisen.dissertation.data.impl.descriptors.LongDescriptor</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template match="mns:integer">
    <xsl:call-template name="beanDescriptorModel">
      <xsl:with-param name="class">net.meisen.dissertation.data.impl.descriptors.IntegerDescriptor</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template match="mns:resource">
    <xsl:call-template name="beanDescriptorModel">
      <xsl:with-param name="class">net.meisen.dissertation.data.impl.descriptors.ResourceDescriptor</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <!--
    Helper methods
    -->
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