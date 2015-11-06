/*
 * splay.c - splay tree implementation
 * @copyright (c) 2009-2010, Tohoku University.
 * @author UENO Katsuhiro
 */

#include <stdio.h>
#include <stdlib.h>
#include "smlsharp.h"
#include "sml_tree.h"

enum { RIGHT, LEFT };   /* sign bit of compare result */
#define OPP(dir) ((int)((unsigned int)(dir) ^ 1))
#define DIR(cmp) (cmp < 0 ? LEFT : RIGHT)

struct sml_tree_node {
	struct sml_tree_node *child[2];
	void *item;
	void *next;   /* for tree traversal */
};

typedef int (*cmp_fn)(void *, void *);

static int
splay(cmp_fn cmp, struct sml_tree_node **root, void *item)
{
	struct sml_tree_node *child[2], **hole[2];
	struct sml_tree_node *node = *root;
	struct sml_tree_node *tmp;
	int n, dir;

	if (node == NULL)
		return 1;

	hole[RIGHT] = &child[RIGHT];
	hole[LEFT] = &child[LEFT];
	n = cmp(item, node->item);
	while (n != 0) {
		dir = DIR(n);
		if (node->child[dir] == NULL)
			break;
		n = cmp(item, node->child[dir]->item);
		if (n != 0 && DIR(n) == dir) {
			/* zig-zig: rotation */
			tmp = node->child[dir];
			node->child[dir] = node->child[dir]->child[OPP(dir)];
			tmp->child[OPP(dir)] = node;
			node = tmp;
			if (node->child[dir] == NULL)
				break;
			n = cmp(item, node->child[dir]->item);
		}
		*hole[OPP(dir)] = node;
		hole[OPP(dir)] = &node->child[dir];
		node = node->child[dir];
	}
	*hole[RIGHT] = node->child[RIGHT];
	*hole[LEFT] = node->child[LEFT];
	node->child[RIGHT] = child[RIGHT];
	node->child[LEFT] = child[LEFT];

	*root = node;
	return n;
}

void *
sml_tree_find(sml_tree_t *tree, void *item)
{
	int n = splay(tree->cmp, &tree->root, item);
	return (n == 0) ? tree->root->item : NULL;
}

void
sml_tree_insert(sml_tree_t *tree, void *item)
{
	struct sml_tree_node *node;
	int n, dir;

	n = splay(tree->cmp, &tree->root, item);
	if (n == 0) {
		tree->root->item = item;
		return;
	}

	node = tree->alloc(sizeof(struct sml_tree_node));
	node->item = item;

	dir = DIR(n);
	node->child[OPP(dir)] = tree->root;
	if (tree->root) {
		node->child[dir] = tree->root->child[dir];
		tree->root->child[dir] = NULL;
	} else {
		node->child[dir] = NULL;
	}
	tree->root = node;
}

static struct sml_tree_node *
delete_root(cmp_fn cmp, struct sml_tree_node *root)
{
	struct sml_tree_node *newroot;

	if (root->child[LEFT] == NULL) {
		newroot = root->child[RIGHT];
	} else {
		newroot = root->child[LEFT];
		if (root->child[RIGHT] != NULL) {
			splay(cmp, &newroot, root->item);
			ASSERT(newroot->child[RIGHT] == NULL);
			newroot->child[RIGHT] = root->child[RIGHT];
		}
	}
	return newroot;
}

void
sml_tree_reject(sml_tree_t *tree, int(*f)(void *attr))
{
	cmp_fn cmp = tree->cmp;
	void (*free)(void *) = tree->free;
	struct sml_tree_node *node, *new, **cur, **top;

	if (tree->root == NULL)
		return;

	tree->root->next = NULL;
	top = &tree->root;
	do {
		cur = top;
		node = *cur;
		top = node->next;
		while (f(node->item)) {
			new = delete_root(cmp, node);
			*cur = new;
			if (free)
				free(node);
			node = new;
			if (node == NULL)
				break;
		}
		if (node) {
			if (node->child[RIGHT]) {
				node->child[RIGHT]->next = top;
				top = &node->child[RIGHT];
			}
			if (node->child[LEFT]) {
				node->child[LEFT]->next = top;
				top = &node->child[LEFT];
			}
		}
	} while (top);
}

#define TRAVERSE_NEXT(top, new) do { \
	new = top->next; \
	if (top->child[RIGHT]) \
		top->child[RIGHT]->next = new, new = top->child[RIGHT]; \
	if (top->child[LEFT]) \
		top->child[LEFT]->next = new, new = top->child[LEFT];	\
} while (0)

void
sml_tree_each(sml_tree_t *tree, void (*f)(void *, void *), void *data)
{
	struct sml_tree_node *top, *next;

	if (!tree->root)
		return;

	tree->root->next = NULL;
	for (top = tree->root; top; top = next) {
		TRAVERSE_NEXT(top, next);
		f(top->item, data);
	}
}
