---
layout: page
title: Language
permalink: /language/
---

<div id="docbook_div">
If you are seeing this sentence, it means that there is a bug
in how we render docbook documents.
</div>

<script src="{{ site.baseurl }}/assets/docbook.js"></script>
<style>@import url("{{ site.baseurl }}/assets/docbook.css")</style>
<script>
"use strict";
async function show_docbook_file(element, url) {
    const headers = { 'Cache-Control': 'no-cache' }; // disable caching while we test changes
    const response = await fetch(url, { 'headers': headers });
    const str = await response.text();
    const xml = await new DOMParser().parseFromString(str, "text/xml").documentElement;
    element.innerHTML = patch_docbook(xml, false).innerHTML;
}
const div = document.getElementById("docbook_div");
show_docbook_file(div, "ISA_LRM.db");
</script>
