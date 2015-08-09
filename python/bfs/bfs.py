#!/usr/bin/env python3

import collections


class Graph:
    __slots__ = ('nodes',)

    def __init__(self):
        self.nodes = []


class Node:
    __slots__ = ('adjacent', 'value', 'visited')

    def __init__(self, value):
        self.adjacent = []
        self.value = value
        self.visited = False

    def __repr__(self):
        return "<Node(value={}>".format(self.value)


def visit(node):
    print(repr(node))


def bfs(graph):
    queue = collections.deque(graph.nodes)

    # Ugh, implicit conversion to Boolean based on emptiness
    while queue:
        n = queue.pop()

        if not n.visited:
            visit(n)
            n.visited = True

        [queue.append(x) for x in n.adjacent if not x.visited]


if __name__ == "__main__":
    a = Node('a')
    b = Node('b')
    c = Node('c')
    d = Node('d')
    e = Node('e')

    a.adjacent = [b, c, d, e]
    b.adjacent = [a, c]
    c.adjacent = [a, b]

    e.adjacent = [a, d]
    d.adjacent = [e]

    g = Graph()
    g.nodes = [a]

    bfs(g)
