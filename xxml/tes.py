import html5lib
import lxml.html

doc = html5lib.parse('''
    <table>
        <tr>aaa<td>Header</td></tr>
        <tr><td>Want This
    </table>

''', treebuilder='lxml', namespaceHTMLElements=False)

print lxml.html.tostring(doc)

