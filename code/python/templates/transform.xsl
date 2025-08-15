<!-- -*- mode: xml -*- -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:param name="pubdate" />
  <xsl:param name="fname" />
  <xsl:param name="link" />

  <!-- by default copy all elements unchanged -->
  <xsl:template match="@*|node()">
    <xsl:copy><xsl:apply-templates select="@*|node()"/></xsl:copy>
  </xsl:template>


  <!-- transform a custom SUB-HEADER element into a proper H3 -->
  <xsl:template match="sub-header">
    <h3 class="sub"><xsl:apply-templates select="@*|node()" /></h3>
  </xsl:template>

  <xsl:template match="h3">
    <h3 class="sub">
      <xsl:choose>
        <xsl:when test="count(./@id) > 0">
          <xsl:attribute name="id">
            <xsl:value-of select="concat(concat($fname, '-'), ./@id)" />
          </xsl:attribute>
        </xsl:when>
      </xsl:choose>
      <xsl:apply-templates select="@*[local-name() != 'id']|node()" />
    </h3>
  </xsl:template>

  <!-- new-style header, <my-header>; works with, for example:            -->
  <!-- <my-header>Changes on the blog <time>2015-08-25</time></my-header> -->
  <xsl:template match="my-header">
    <header>
      <h1 class="entry-title">
        <a>
          <xsl:attribute name="class">title-link</xsl:attribute>
          <xsl:attribute name="id"><xsl:value-of select="$fname" /></xsl:attribute>
          <xsl:attribute name="href"><xsl:value-of select="concat('/posts/', $link, '.html')" /></xsl:attribute>
          <xsl:attribute name="title">Read on separate page</xsl:attribute>
          <xsl:apply-templates select="l|text()" />
          <!-- <xsl:value-of select="text()"/> -->
        </a>
        <a>
          <xsl:attribute name="href"><xsl:value-of select="concat('#', $link)" /></xsl:attribute>
          <xsl:attribute name="class">robaczek</xsl:attribute>
          <xsl:attribute name="title">Link to this place on current page</xsl:attribute>
          ¶
        </a>
      </h1>

      <xsl:choose>
        <xsl:when test="count(./sub-title/node()) > 0">
          <h3>
            <xsl:attribute name="class">subtitle</xsl:attribute>
            <xsl:apply-templates select="./sub-title/node()" />
          </h3>
        </xsl:when>
      </xsl:choose>
      <p class="meta">
        Last updated on: <time><xsl:value-of select="./time" /></time>
      </p>

    </header>
  </xsl:template>


  <!-- backwards compat, expected format is like this: -->
  <!-- <header>                                       -->
  <!--   <h1 class="entry-title">Welcome!</h1>        -->
  <!--   <p class="meta"> <time></time></p>           -->
  <!-- </header>                                      -->
  <xsl:template match="header">
    <header>
      <h1 class="entry-title">
        <a>
          <xsl:attribute name="class">title-link</xsl:attribute>
          <xsl:attribute name="id"><xsl:value-of select="$fname" /></xsl:attribute>
          <xsl:attribute name="href"><xsl:value-of select="concat('posts/', $link, '.html')" /></xsl:attribute>
          <xsl:attribute name="title">Read on separate page</xsl:attribute>
          <xsl:value-of select="h1/text()"/>
        </a>
        <a>
          <xsl:attribute name="href"><xsl:value-of select="concat('#', $link)" /></xsl:attribute>
          <xsl:attribute name="class">robaczek</xsl:attribute>
          <xsl:attribute name="title">Link to this place on current page</xsl:attribute>
          ¶
        </a>
      </h1>
      <p class="meta">
        <time><xsl:value-of select="./p/time/text()" /></time>
      </p>
    </header>
  </xsl:template>

  <!-- <MY-IMG> accepts all attributes that original IMG would;             -->
  <!-- it just wraps an IMG tag with some divs and elements to use Fancybox -->
  <!-- automatically                                                        -->
  <xsl:template match="my-img">
    <div style="text-align: center" class="post-image">
      <a>
        <xsl:attribute name="title">click to enlarge</xsl:attribute>
        <xsl:attribute name="href">
          <xsl:value-of select="./@src" />
        </xsl:attribute>
        <img>
          <xsl:apply-templates select="./@*" />
          <xsl:attribute name="src">
            <xsl:value-of select="./@src" />
          </xsl:attribute>
        </img>
      </a>
    </div>
  </xsl:template>

  <!-- shortcut for creating BLOCKQUOTE tags -->
  <xsl:template match="bq">
    <div>
      <blockquote class="no-quote">
        <code><xsl:apply-templates /></code>
      </blockquote>
    </div>
  </xsl:template>

  <!-- support for creating a list of alerts on top of a post               -->
  <!-- <notice>                                                             -->
  <!--   <note>                                                             -->
  <!--     All the code is available on                                     -->
  <!--     <a href="https://github.com/piotrklibert/lantalk">GitHub</a>     -->
  <!--   </note>                                                            -->
  <!-- </notice>                                                            -->
  <xsl:template match="notice">
    <div class="alert alert-info"><xsl:apply-templates /></div>
  </xsl:template>

  <xsl:template match="note">
    <div class="note"><strong>NOTE: </strong><xsl:apply-templates /></div>
  </xsl:template>

  <xsl:template match="note[contains(@class, 'outdated')]">
    <div>
      <xsl:attribute name="class">
        <xsl:value-of select="./@class" />
      </xsl:attribute>
      <div class="outdated">
        <strong>NOTE:</strong>
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

</xsl:stylesheet>
