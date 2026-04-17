"use strict";

// Copyright (C) 2025-2026 Intel Corporation

// Copy an element and change the tag
function copyElementWithNewTag(e, tag) {
    let new_e = document.createElement(tag);
    for (const attr of e.attributes) {
        new_e.setAttribute(attr.name, attr.value);
    }
    for (const c of Array.from(e.childNodes)) {
        new_e.append(c)
    }
    return new_e;
}

// Add a permalink at end of an element
function add_permalink(element, id) {
    let permalink = document.createElement('a');
    permalink.setAttribute('href', '#' + id);
    permalink.innerHTML = " 🔗";
    permalink.setAttribute('style', "text-decoration: none;");
    element.insertAdjacentElement('beforeend', permalink);
}

// convert DocBook links to HTML hrefs
function patch_links(xml, ids) {
    for (const x of Array.from(xml.getElementsByTagName('link'))) {
        const linkend = x.getAttribute('linkend');
        const endterm = x.getAttribute('endterm');
        const endterm_element = endterm !== null ? ids.get(CSS.escape(endterm)) : x;
        const is_global = linkend.startsWith("http"); // crude heuristic
        let new_x = document.createElement('a');
        new_x.innerHTML = endterm_element ? endterm_element.innerHTML : "broken-link";
        new_x.setAttribute('href', is_global ? linkend : ('#' + linkend));
        x.parentNode.replaceChild(new_x, x);
    }
}

// convert DocBook anchors to HTML spans with an 'id'
function patch_anchors(xml) {
    for (const x of Array.from(xml.getElementsByTagName('anchor'))) {
        const new_x = copyElementWithNewTag(x, 'span');
        new_x.setAttribute('id', x.getAttribute('id'));
        x.replaceWith(new_x);
    }
}

// convert DocBook footnote references to superscripts
function patch_footnoterefs(xml) {
    for (const x of Array.from(xml.getElementsByTagName('footnoteref'))) {
        const linkend = x.getAttribute('linkend');
        let footmark = document.createElement('superscript');
        footmark.innerHTML = linkend;
        x.parentNode.replaceChild(footmark, x);
    }
}

function patch_tables(xml) {
    for (const x of Array.from(xml.getElementsByTagName('table'))) {
        patch_table(x);
    }
    for (const x of Array.from(xml.getElementsByTagName('informaltable'))) {
        patch_table(x);
    }
}

// convert DocBook table to HTML.
// The essential feature of this is the handling of footnotes
// - there can be multiple references to a single footnote
// - the footnotes are displayed below the table
function patch_table(x) {
    let footnotes = [];
    for(const footnote of Array.from(x.getElementsByTagName('footnote'))) {
        const fnum = footnotes.length + 1;
        const mark = document.createTextNode(fnum.toString());
        const sup = document.createElement('superscript');
        sup.appendChild(mark);
        footnote.parentNode.replaceChild(sup, footnote);
        footnotes.push([fnum, footnote]);
    }
    if (footnotes.length > 0) {
        const footer = document.createElement('tfoot');
        footer.innerHTML = '<tr><th class="table-notes">NOTES:</th></tr>';

        const row = document.createElement('tr');
        footer.appendChild(row);

        const cell = document.createElement('td');
        cell.setAttribute('colspan', '7');
        row.appendChild(cell);

        const footdiv = document.createElement('div');
        footdiv.setAttribute('class', 'footnotes');
        cell.appendChild(footdiv);

        for (const [fnum, footnote] of footnotes) {
            const mark = document.createTextNode(fnum.toString() + ".");

            const markdiv = document.createElement('span');
            markdiv.setAttribute('class', 'footmark');
            markdiv.appendChild(mark);
            footdiv.appendChild(markdiv);

            const note = copyElementWithNewTag(footnote, 'span');
            note.setAttribute('class', 'footnote');
            footdiv.appendChild(note);
        }
        x.appendChild(footer);
    }
}

// Converts mathphrase to italic (this leaves a lot of room for improvement!)
function patch_mathphrases(xml) {
    for (const x of Array.from(xml.getElementsByTagName('mathphrase'))) {
        // The 'correct' conversion would be to use the 'math' tag but this is
        // inconsistently supported in current browsers so, for now, we just
        // use italics and hope that the formulae are not too complex.
        let new_x = document.createElement('italic');
        new_x.innerHTML = x.innerHTML;
        x.parentNode.replaceChild(new_x, x);
    }
}

function count_matching_ancestors(xml, tag) {
    let count = 0;
    while (xml) {
        xml = xml.parentElement;
        if (xml && xml.tagName == tag) {
            count = count + 1;
        }
    }
    return count;
}

// Convert sections to HTML headers
// Optionally, close subsections
function patch_sections(xml, close_subsections) {
    for (const x of Array.from(xml.getElementsByTagName('section'))) {
        const level = count_matching_ancestors(x, 'details') + 2;
        const title = x.getElementsByTagName('title')[0];
        if (title) {
            const is_open = !close_subsections || level == 1;
            const heading = copyElementWithNewTag(title, 'h' + level);
            if (x.getAttribute('id')) {
                add_permalink(heading, x.getAttribute('id'));
            }
            const summary = document.createElement('summary');
            summary.append(heading);
            const details = document.createElement('details');
            details.append(summary);
            for (const attr of x.attributes) {
                details.setAttribute(attr.name, attr.value);
            }
            for (const c of Array.from(x.childNodes)) {
                if (c != title) {
                    details.append(c);
                }
            }
            details.setAttribute('open', is_open);
            x.replaceWith(details);
        }
    }
    // Handle any remaining titles that are outside a section
    // (so that it is not confused with the very different HTML title element)
    for (const title of Array.from(xml.getElementsByTagName('title'))) {
        const heading = copyElementWithNewTag(title, 'h1');
        title.replaceWith(heading);
    }
}

// Patch long URLs: inserting <wbr> ('word break opportunity') at slashes
function patch_long_urls(xml) {
    for (const x of Array.from(xml.getElementsByTagName('href'))) {
        // Replace '/' with '<wbr>/'
        if (x.children.length == 0) { // Only try to patch hrefs with no children
            let url = x.textContent;
            let pieces = url.split('/');
            if (pieces.length > 1) {
                x.textContent = pieces[0];
                for (const i = 1; i < pieces.length; i++) {
                    x.appendChild(document.createElement('wbr'));
                    x.appendChild(document.createTextNode('/' + pieces[i]));
                }
            }
        }
    }
}

// Remove the first blank line at the start and end of a programlisting
function patch_programlisting(xml) {
    for (const code of Array.from(xml.getElementsByTagName("programlisting"))) {
        code.textContent = code.textContent.replace(/^\n/,'').replace(/[ \t\n]+$/,'');
    }
}

// Change xml:id attributes to id attributes
function patch_ids(xml) {
    const ids = new Map();
    console.log("Patching ids");
    for (const x of xml.querySelectorAll('*')) {
        const id = x.getAttribute('xml:id');
        if (id) {
            x.setAttribute('id', CSS.escape(id));
            ids.set(CSS.escape(id), x);
        }
    }
    return ids;
}

// Change xml:id attributes to id attributes
function patch_profiles(xml, show_profiles) {
    for (const x of xml.querySelectorAll('*[status]')) {
        x.remove();
    }
}

// Convert DocBook XML to HTML
function patch_docbook(xml, use_details) {
    // start by deleting anything not in the current profile
    // (at present, this means anything with a 'status' attribute)
    patch_profiles(xml);
    // Note that the xml may not have been added to the DOM yet - so we
    // build our own table mapping IDs to elements
    const ids = patch_ids(xml);
    patch_footnoterefs(xml);
    patch_tables(xml);
    patch_mathphrases(xml);
    patch_links(xml, ids);
    patch_sections(xml, use_details);
    patch_anchors(xml);
    patch_long_urls(xml);
    patch_programlisting(xml);
    return xml;
}
