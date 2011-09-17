<?xml version="1.0"?>
<xsl:stylesheet id="quotes" version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="quotelist">
   <html>
     <body>
          <h1>Quotes</h1>
	  <xsl:apply-templates/>
     </body>
   </html>
</xsl:template>

<xsl:template match="quote | aphorism">
   <blockquote>
      <xsl:apply-templates/>
   </blockquote>
</xsl:template>

<xsl:template match="body">
   <p><xsl:apply-templates/></p>
</xsl:template>

<xsl:template match="source">
   <p align="right"><xsl:apply-templates/></p>
</xsl:template>

</xsl:stylesheet>






		