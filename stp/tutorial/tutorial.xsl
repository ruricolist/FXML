<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html"
	      indent="yes"
	      doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
	      doctype-system="http://www.w3.org/TR/html4/loose.dtd"/>

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="/">
    <xsl:apply-templates select="/tutorial/page"/>
    <xsl:call-template name="examples"/>
    <html>
      <head>
	<title>STP Tutorial</title>
	<link rel="stylesheet" type="text/css" href="tutorial.css"/>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
      </head>
      <body>
	<xsl:call-template name="header"/>
	<div class="main">
	  <h1>Contents</h1>
	  <table cellspacing="0" cellpadding="0">
	    <xsl:for-each select="/tutorial/page">
	      <tr>
		<td align="right">
		  <xsl:value-of select="position()"/>
		</td>
		<td>
		  <xsl:text>&#160;&#160;</xsl:text>
		  <a class="blue"
		     href="pages/{position()}.html">
		    <xsl:value-of select="@title"/>
		  </a>
		  <xsl:if test="@description">
		    <xsl:text>: </xsl:text>
		    <xsl:value-of select="@description"/>
		  </xsl:if>
		</td>
	      </tr>
	    </xsl:for-each>
	  </table>
	</div>
	<div class="navigation">
	  start
	  <a class="icon" href="pages/1.html">
	    &#x2b17;
	  </a>
	</div>
      </body>
    </html>
  </xsl:template>

  <xsl:template name="examples">
    <xsl:document href="examples.html"
		  method="html"
		  indent="yes"
		  doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
		  doctype-system="http://www.w3.org/TR/html4/loose.dtd">
      <html>
	<head>
	  <title>
	    STP examples
	  </title>
	  <link rel="stylesheet" type="text/css" href="tutorial.css"/>
	  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
	</head>
	<body>
	  <div class="main" style="font-size: 8pt;">
	    <h1>STP examples</h1>
	    <p>
	      The following simple examples and code snippets have been
	      taken from the <a href="index.html">STP tutorial</a>.  The
	      containing tutorial slide is linked from each example.
	    </p>
	    <xsl:for-each select="//page">
	      <xsl:variable name="i" select="position()"/>
	      <xsl:for-each select=".//example">
		<div class="example">
		  <div class="example-header">
		    <a href="pages/{$i}.html"
		       style="color: #aaaaaa">
		      (Page <xsl:value-of select="$i"/>)&#160;
		    </a>
		    <xsl:value-of select="@title"/>
		  </div>
		  <xsl:apply-templates/>
		</div>
	      </xsl:for-each>
	    </xsl:for-each>
<!--
	    <xsl:for-each select="//example">
	      <div class="example">
		<div class="example-header">
		  <xsl:value-of select="@title"/>
		</div>
		<xsl:apply-templates/>
	      </div>
	      <xsl:for-each select="ancestor::page">
		<a href="pages/{position()}.html">
		  <xsl:value-of select="position()"/>
		</a>
	      </xsl:for-each>
	    </xsl:for-each>
-->
	  </div>
	</body>
      </html>
    </xsl:document>
  </xsl:template>

  <xsl:template match="page">
    <xsl:document href="pages/{position()}.html"
		  method="html"
		  indent="yes"
		  doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
		  doctype-system="http://www.w3.org/TR/html4/loose.dtd">
      <html>
	<head>
	  <title>
	    Page
	    <xsl:value-of select="position()"/>
	    --
	    <xsl:value-of select="@title"/>
	  </title>
	  <link rel="stylesheet" type="text/css" href="../tutorial.css"/>
	  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
	</head>
	<body>
	  <xsl:call-template name="header">
	    <xsl:with-param name="base" select="'../'"/>
	  </xsl:call-template>
	  <div class="main">
	    <h1>
	      <xsl:value-of select="@title"/>
	    </h1>
	    <xsl:apply-templates/>
	  </div>
	  <div class="navigation">
	    <center>
	      <a class="icon" href="../index.html">
		&#x2b18;
	      </a>
	    </center>
	    <xsl:choose>
	      <xsl:when test="position() > 1">
		<a class="icon" href="{position()-1}.html">&#x2b16;</a>
	      </xsl:when>
	      <xsl:otherwise>
		<span class="icon" style="color: #aaaaaa">&#x2b16;</span>
	      </xsl:otherwise>
	    </xsl:choose>
	    <xsl:value-of select="position()"/>
	    <xsl:text>/</xsl:text>
	    <xsl:value-of select="count(../page)"/>
	    <xsl:choose>
	      <xsl:when test="position() &lt; count(../page)">
		<a class="icon" href="{position()+1}.html">&#x2b17;</a>
	      </xsl:when>
	      <xsl:otherwise>
		<span class="icon" style="color: #aaaaaa">&#x2b17;</span>
	      </xsl:otherwise>
	    </xsl:choose>
	  </div>
	</body>
      </html>
    </xsl:document>
  </xsl:template>

  <xsl:template match="code">
    <pre class="code">
      <xsl:apply-templates/>
    </pre>
  </xsl:template>

  <xsl:template match="inline-code">
    <tt class="inline-code">
      <xsl:apply-templates/>
    </tt>
  </xsl:template>

  <xsl:template match="repl">
    <pre class="repl">
      <xsl:apply-templates/>
    </pre>
  </xsl:template>

  <xsl:template match="result">
    <pre class="result">
      <xsl:apply-templates/>
    </pre>
  </xsl:template>

  <xsl:template match="spacy-list">
    <ul class="spacy">
      <xsl:apply-templates/>
    </ul>
  </xsl:template>

  <xsl:template match="em">
    <span class="em">
      <xsl:apply-templates/>
    </span>
  </xsl:template>

  <xsl:template name="header">
    <xsl:param name="base"/>
    <div id="header">
      <table cellspacing="0" cellpadding="0" width="100%">
	<tr>
	  <td valign="center">
	    <div style="margin-left: 30px">
	      <a href="{$base}../index.html">
		<b>
		  <span>cxml&#x2013;</span>
		  <span style="color: #55a6e0">stp</span>
		</b>
	      </a>
	    </div>
	  </td>
	  <td valign="center" align="right">
            <b>Tutorial</b>
	  </td>
	</tr>
      </table>
    </div>
  </xsl:template>
</xsl:stylesheet>
