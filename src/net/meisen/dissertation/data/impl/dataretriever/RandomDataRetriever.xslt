<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                              xmlns="http://www.springframework.org/schema/beans"
                              xmlns:rnd="http://dev.meisen.net/xsd/dissertation/model/rnd">

  <xsl:template match="rnd:data">
    <bean class="net.meisen.dissertation.data.impl.dataretriever.RandomDataRetrieverConfig">
      <xsl:variable name="amount" select="@amount" />
      <xsl:variable name="type" select="@type" />
    
      <property name="amount" value="{$amount}"/>
      <property name="type" value="{$type}"/>
    </bean>
  </xsl:template> 
</xsl:stylesheet>