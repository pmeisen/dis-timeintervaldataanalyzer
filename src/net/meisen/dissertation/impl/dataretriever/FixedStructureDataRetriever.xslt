<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                              xmlns="http://www.springframework.org/schema/beans"
                              xmlns:fxd="http://dev.meisen.net/xsd/dissertation/model/fxd">

  <xsl:template match="fxd:record">
    <bean class="net.meisen.dissertation.impl.dataretriever.FixedStructureDataRetrieverConfig">
      <constructor-arg type="java.util.Collection">
      
        <list value-type="net.meisen.dissertation.impl.dataretriever.FixedStructureDataRetrieverConfigEntry">
          <xsl:for-each select="fxd:field">
            <xsl:variable name="type" select="@type" />
            
            <bean class="net.meisen.dissertation.impl.dataretriever.FixedStructureDataRetrieverConfigEntry">  
              <property name="name"><value><xsl:value-of select="@name" /></value></property>
              <property name="type"><value><xsl:value-of select="@type" /></value></property>
              <xsl:if test="@value"><property name="value"><value type="{$type}"><xsl:value-of select="@value" /></value></property></xsl:if>
              <xsl:if test="@random"><property name="random"><value><xsl:value-of select="@random" /></value></property></xsl:if>
            </bean>         
          </xsl:for-each>
        </list>
      </constructor-arg>
    </bean>
  </xsl:template>
  
  <xsl:template match="fxd:query">
    <bean class="net.meisen.dissertation.impl.dataretriever.FixedStructureQueryConfig">
      <xsl:variable name="amount" select="@amount" />
      
      <property name="amount" value="{$amount}"/>
    </bean>
  </xsl:template>
</xsl:stylesheet>