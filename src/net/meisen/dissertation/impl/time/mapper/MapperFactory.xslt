<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                              xmlns="http://www.springframework.org/schema/beans"
                              xmlns:map="http://dev.meisen.net/xsd/dissertation/model/mapper">

  <xsl:template match="map:config">
    <bean class="net.meisen.dissertation.impl.time.mapper.MapperFactoryConfig">

      <xsl:if test="@inheritDefault and @inheritDefault='true'">
        <constructor-arg ref="defaultMapperFactoryConfig" />
      </xsl:if>
      
      <property name="mapper">
        <list value-type="java.lang.Class">
          <xsl:for-each select="map:mapper">
            <value type="java.lang.Class"><xsl:value-of select="@implementation" /></value>
          </xsl:for-each>
        </list>
      </property>
    </bean>
  </xsl:template>
  
</xsl:stylesheet>