"""Concatenate the AEJ manuscript and its Online Appendix, keeping the cross-document links.

Both documents link to each other's anchors through plain xr (appendix anchors are prefixed "app-").
Generic tools (pdfunite, gs, mutool) drop the named destinations of the second file, which breaks these links;
this script merges the destination name trees and the bookmarks.

Usage (from papers/, requires `pip install pikepdf`):
    python3 merge_AEJ.py [main.pdf] [appendix.pdf] [output.pdf]
"""
import sys
import pikepdf
from pikepdf import NameTree, OutlineItem

main_path = sys.argv[1] if len(sys.argv) > 1 else 'build/support_global_redistr_AEJ.pdf'
app_path = sys.argv[2] if len(sys.argv) > 2 else 'build/support_global_redistr_AEJ_appendix.pdf'
out_path = sys.argv[3] if len(sys.argv) > 3 else 'build/support_global_redistr_AEJ_with_appendix.pdf'

main = pikepdf.open(main_path)
app = pikepdf.open(app_path)
n_main = len(main.pages)
app_page_index = {p.objgen: i for i, p in enumerate(app.pages)}


def dest_array(dest):
    """Explicit destination array of a named destination (either an array or a dictionary with /D)."""
    return dest.D if isinstance(dest, pikepdf.Dictionary) else dest


def remap(dest):
    """Copy an appendix destination array, pointing it to the corresponding page of the merged document."""
    dest = dest_array(dest)
    i = app_page_index.get(dest[0].objgen)
    if i is None:
        return None
    return pikepdf.Array([main.pages[n_main + i].obj] + [main.copy_foreign(x) if isinstance(x, pikepdf.Object) and x.is_indirect else x for x in list(dest)[1:]])


app_dests = dict(NameTree(app.Root.Names.Dests).items()) if '/Names' in app.Root and '/Dests' in app.Root.Names else {}

main.pages.extend(app.pages)

# Named destinations: add the appendix's. pdfTeX replaces each unresolved destination (here, the other document's anchors)
# by a placeholder to page 1, so "app-" names come from the appendix, and other names from the main text (e.g. cite.*)
if '/Names' not in main.Root:
    main.Root.Names = pikepdf.Dictionary()
if '/Dests' not in main.Root.Names:
    main.Root.Names.Dests = NameTree.new(main).obj
tree = NameTree(main.Root.Names.Dests)
existing = set(tree.keys())
for name, dest in app_dests.items():
    if name.startswith('app-') or name not in existing:
        new = remap(dest)
        if new is not None:
            tree[name] = main.make_indirect(new)


# Bookmarks: append the appendix's outline
def app_target(item):
    d = item.destination
    if d is None and item.action is not None and item.action.get('/S') == '/GoTo':
        d = item.action.D
    if isinstance(d, (pikepdf.String, pikepdf.Name)):
        d = app_dests.get(str(d).lstrip('/'))
    if d is None:
        return None
    return app_page_index.get(dest_array(d)[0].objgen)


def copy_item(item):
    i = app_target(item)
    new = OutlineItem(item.title, n_main + i if i is not None else n_main)
    new.children.extend(copy_item(c) for c in item.children)
    return new


with main.open_outline() as main_outline, app.open_outline() as app_outline:
    if app_outline.root:
        top = OutlineItem('Online Appendix', n_main)
        top.children.extend(copy_item(c) for c in app_outline.root)
        main_outline.root.append(top)

main.save(out_path)
print(f'{out_path}: {n_main} + {len(app.pages)} pages')
