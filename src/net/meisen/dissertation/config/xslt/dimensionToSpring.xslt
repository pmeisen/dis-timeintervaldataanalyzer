<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns="http://www.springframework.org/schema/beans" 
                              xmlns:dim="http://dev.meisen.net/xsd/dissertation/dimension"
                              xmlns:uuid="java.util.UUID"
                              xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" indent="yes" />
  
  <xsl:template match="/dim:dimensions">
    <beans xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans-3.0.xsd
                               http://www.springframework.org/schema/util  http://www.springframework.org/schema/util/spring-util-2.0.xsd">
                               
      <!-- parse the time-dimension -->
      <xsl:for-each select="dim:timedimension">
        <xsl:apply-templates select="." />
      </xsl:for-each>
      
      <!-- Iterate over the dimensions -->
      <xsl:for-each select="dim:dimension">
        <xsl:apply-templates select="." />
      </xsl:for-each>
    </beans>
  </xsl:template>
  
  <!--
    ++ Template for TimeDimension
    -->
  <xsl:template match="dim:timedimension">
  
    <!-- generate an id for the dimension -->
    <xsl:variable name="dimensionId">timedimension-<xsl:value-of select="uuid:randomUUID()" /></xsl:variable>
    
    <bean id="{$dimensionId}" class="net.meisen.dissertation.model.dimensions.TimeDimension">
      <constructor-arg type="java.lang.String"><value><xsl:value-of select="@id" /></value></constructor-arg>
      <xsl:if test="@name">
        <constructor-arg type="java.lang.String"><value><xsl:value-of select="@name" /></value></constructor-arg>
      </xsl:if>
    </bean>
    
    <xsl:for-each select="dim:hierarchy">
      <xsl:variable name="hierarchyId">timehierarchy-<xsl:value-of select="uuid:randomUUID()" /></xsl:variable>

      <bean id="{$hierarchyId}" factory-bean="{$dimensionId}" factory-method="addHierarchy">
        <constructor-arg type="java.lang.String"><value><xsl:value-of select="@id" /></value></constructor-arg>
        <constructor-arg type="java.lang.String">
          <xsl:choose>
            <xsl:when test="@name"><value><xsl:value-of select="@name" /></value></xsl:when>
            <xsl:otherwise><null /></xsl:otherwise>
          </xsl:choose>
        </constructor-arg>
        <constructor-arg type="java.lang.String">
          <xsl:choose>
            <xsl:when test="@timezone"><value><xsl:value-of select="@timezone" /></value></xsl:when>
            <xsl:otherwise><null /></xsl:otherwise>
          </xsl:choose>
        </constructor-arg>
      </bean>
      
      <!-- modify the all level if defined -->
      <xsl:if test="@all">
        <bean class="net.meisen.general.sbconfigurator.factories.MethodExecutorBean">
          <property name="targetMethod" value="modifyLevel" />
          <property name="targetObject" ref="{$hierarchyId}" />
          <property name="type" value="factory" />
          <property name="arguments">
            <list>
              <value>*</value>
              <value><xsl:value-of select="@all" /></value>
            </list>
          </property>
        </bean>
      </xsl:if>
      
      <xsl:for-each select="dim:level">
        <xsl:variable name="levelId">timelevel-<xsl:value-of select="uuid:randomUUID()" /></xsl:variable>
        <bean id="{$levelId}" factory-bean="{$hierarchyId}" factory-method="addLevel">
          <xsl:variable name="lazy">
            <xsl:choose>
              <xsl:when test="@lazy"><xsl:value-of select="@lazy" /></xsl:when>
              <xsl:otherwise>true</xsl:otherwise>
            </xsl:choose>
          </xsl:variable>
        
          <constructor-arg type="java.lang.String"><value><xsl:value-of select="@id" /></value></constructor-arg>
          <constructor-arg type="java.lang.String"><value><xsl:value-of select="@template" /></value></constructor-arg>
          <constructor-arg type="boolean" value="{$lazy}" />
        </bean>
        
        <!-- modify the levels name, if one is defined -->
        <xsl:if test="@name">
          <bean class="net.meisen.general.sbconfigurator.factories.MethodExecutorBean">
            <property name="targetMethod" value="modifyLevel" />
            <property name="targetObject" ref="{$hierarchyId}" />
            <property name="type" value="factory" />
            <property name="arguments">
              <list>
                <value><xsl:value-of select="@id" /></value>
                <value><xsl:value-of select="@name" /></value>
              </list>
            </property>
          </bean>
        </xsl:if>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>
  
  <!--
    ++ Template for DescriptorDimensions
    -->
  <xsl:template match="dim:dimension">
  
    <!-- generate an id for the dimension -->
    <xsl:variable name="dimensionId">dimension-<xsl:value-of select="uuid:randomUUID()" /></xsl:variable>
    
    <bean id="{$dimensionId}" class="net.meisen.dissertation.model.dimensions.DescriptorDimension">
      <constructor-arg type="java.lang.String"><value><xsl:value-of select="@id" /></value></constructor-arg>
      <constructor-arg type="java.lang.String"><value><xsl:value-of select="@descriptor" /></value></constructor-arg>
      <xsl:if test="@name">
        <constructor-arg type="java.lang.String"><value><xsl:value-of select="@name" /></value></constructor-arg>
      </xsl:if>
    </bean>
    
    <xsl:for-each select="dim:hierarchy">
      <xsl:variable name="hierarchyId">hierarchy-<xsl:value-of select="uuid:randomUUID()" /></xsl:variable>

      <bean id="{$hierarchyId}" factory-bean="{$dimensionId}" factory-method="addHierarchy">
        <constructor-arg type="java.lang.String"><value><xsl:value-of select="@id" /></value></constructor-arg>
        <xsl:if test="@name">
          <constructor-arg type="java.lang.String"><value><xsl:value-of select="@name" /></value></constructor-arg>
        </xsl:if>
      </bean>
      
      <!-- modify the all level if defined -->
      <xsl:if test="@all">
        <bean class="net.meisen.general.sbconfigurator.factories.MethodExecutorBean">
          <property name="targetMethod" value="modifyLevel" />
          <property name="targetObject" ref="{$hierarchyId}" />
          <property name="type" value="factory" />
          <property name="arguments">
            <list>
              <value>*</value>
              <value><xsl:value-of select="@all" /></value>
            </list>
          </property>
        </bean>
      </xsl:if>
      
      <xsl:for-each select="dim:level">
        <xsl:variable name="levelId"><xsl:value-of select="@id" /></xsl:variable>
        <xsl:for-each select="dim:member">
        
          <xsl:variable name="memberTypeFunction">
            <xsl:choose>
              <xsl:when test="@reg and @value"><xsl:message terminate="yes">Member has a reg and value defined, please specify only one value!</xsl:message></xsl:when>
              <xsl:when test="@value and @null"><xsl:message terminate="yes">The null-attribute is only allowed alone, or with the reg-attribute!</xsl:message></xsl:when>
              <xsl:when test="@reg">addPatternMember</xsl:when>
              <xsl:when test="@value">addDescriptorMember</xsl:when>
              <xsl:when test="@null">addPatternMember</xsl:when>
              <xsl:otherwise>addMember</xsl:otherwise>
            </xsl:choose>
          </xsl:variable>
        
          <bean factory-bean="{$hierarchyId}" factory-method="{$memberTypeFunction}">
            <!-- id -->
            <constructor-arg type="java.lang.String"><value><xsl:value-of select="@id" /></value></constructor-arg>
            
            <!-- name -->
            <xsl:if test="@name">
              <constructor-arg type="java.lang.String"><value><xsl:value-of select="@name" /></value></constructor-arg>
            </xsl:if>
            
            <!-- reg or value -->
            <xsl:if test="@reg">
              <constructor-arg type="java.lang.String"><value><xsl:value-of select="@reg" /></value></constructor-arg>
            </xsl:if>
            <xsl:if test="@value">
              <constructor-arg type="java.lang.String"><value><xsl:value-of select="@value" /></value></constructor-arg>
            </xsl:if>
            
            <!-- if reg and null -->
            <xsl:choose>
              <!-- check if we have reg and null -->
              <xsl:when test="@reg and @null">
                <constructor-arg type="boolean"><value><xsl:value-of select="@null" /></value></constructor-arg>
              </xsl:when>
              <!-- we have to add the regular expression and the null-value -->
              <xsl:when test="@null">
                <constructor-arg type="java.lang.String" value=" ^" />
                <constructor-arg type="boolean"><value><xsl:value-of select="@null" /></value></constructor-arg>
              </xsl:when>
              <xsl:when test="@reg">
                <constructor-arg type="boolean" value="false" />
              </xsl:when>
            </xsl:choose>
            
            <!-- levelId -->
            <constructor-arg type="java.lang.String" value="{$levelId}" />
            
            <!-- rollUpTo -->
            <constructor-arg>
              <bean class="org.springframework.util.StringUtils" factory-method="delimitedListToStringArray">
                <constructor-arg type="java.lang.String"><value><xsl:value-of select="@rollUpTo" /></value></constructor-arg>
                <constructor-arg type="java.lang.String" value="," />
                <constructor-arg type="java.lang.String" value=" " />
              </bean>
            </constructor-arg>
          </bean>
        </xsl:for-each>
        
        <!-- modify the levels name, if one is defined -->
        <xsl:if test="@name">
          <bean class="net.meisen.general.sbconfigurator.factories.MethodExecutorBean">
            <property name="targetMethod" value="modifyLevel" />
            <property name="targetObject" ref="{$hierarchyId}" />
            <property name="type" value="factory" />
            <property name="arguments">
              <list>
                <value><xsl:value-of select="$levelId" /></value>
                <value><xsl:value-of select="@name" /></value>
              </list>
            </property>
          </bean>
        </xsl:if>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>