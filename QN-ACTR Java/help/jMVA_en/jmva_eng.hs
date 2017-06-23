<?xml version='1.0' encoding='ISO-8859-1' ?>
<!DOCTYPE helpset PUBLIC "-//Sun Microsystems Inc.//DTD JavaHelp HelpSet Version 1.0//EN" "http://java.sun.com/products/javahelp/helpset_1_0.dtd">

<helpset version="1.0">
  <title>jmva_eng</title>
   <maps>
    <homeID>first_topic_htm</homeID>
    <mapref location="jmva_eng_map.xml" />
  </maps>

  <view mergetype="javax.help.UniteAppendMerge">
    <name>TOC</name>
    <label>Table of Contents</label>
    <type>javax.help.TOCView</type>
    <data>jmva_eng_toc.xml</data>
  </view>

  <view>
    <name>Index</name>
    <label>Index</label>
    <type>javax.help.IndexView</type>
    <data>jmva_eng_ndx.xml</data>
  </view>

  <view>
    <name>Search</name>
    <label>Search</label>
    <type>javax.help.SearchView</type>
    <data engine="com.sun.java.help.search.DefaultSearchEngine">jmva_eng_JavaHelpSearch</data>
  </view>


</helpset>
