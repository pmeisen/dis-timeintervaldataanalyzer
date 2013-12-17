<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                              xmlns="http://www.springframework.org/schema/beans"
                              xmlns:db="http://dev.meisen.net/xsd/dissertation/model/db">

  <xsl:template match="db:connection">
    <bean id="lala" class="test.class" />
  </xsl:template>          
</xsl:stylesheet>