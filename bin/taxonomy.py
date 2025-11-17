class TaxTree:
    def __init__(self):
        self.nodes = {}
        self.roots = set()

    @classmethod
    def from_dict(Cls, d):
        'Builds a taxonomy tree based on a dictionary mapping child names to parent names'
        t = Cls()
        for ch, par in d.items():
            t.add_node(ch, par)
        t.update_roots()
        return t

    def __len__(self):
        return len(self.nodes)

    def add_node(self, name, par):
        try:
            par_node = self.nodes[par]
        except KeyError:
            par_node = TaxNode(par)
            self.nodes[par] = par_node
        try:
            node = self.nodes[name]
            node.par = par_node
        except KeyError:
            node = TaxNode(name, par_node)
            self.nodes[name] = node
        par_node.children.add(node)

    def update_roots(self):
        self.roots = set()
        for node in self.nodes.values():
            root_finder = node
            while root_finder.par:
                root_finder = root_finder.par
            self.roots.add(root_finder)

    def expand_set(self, s):
        'Given a set s of node names, returns a copy of the set expanded to include all ancestors and descendants of the nodes named in s. Names in s not found in the taxonomy tree are left in the output set'
        new_set = set()
        for name in s:
            try:
                node = self.nodes[name]
                node_set = node.get_descendants().union(node.get_ancestors())
                for setnode in node_set:
                    new_set.add(setnode.name)
            except KeyError:
                new_set.add(name)
        return new_set
    def __str__(self):
        s = ''
        for root in self.roots:
            s += root.print_indented()
        return s

class TaxNode:
    def __init__(self, name, par = None):
        self.name = name
        if par and par.name == name:
            par = None
        self.par = par
        self.children = set()
    def get_descendants(self):
        all_d = set([self])
        for ch in self.children:
            all_d.update(ch.get_descendants())
        return all_d
    def get_ancestors(self):
        all_a = set([self])
        if self.par:
            all_a.update(self.par.get_ancestors())
        return all_a
    def print_indented(self, indent = 0):
        s = '  '*indent+self.name + '\n'
        for ch in self.children:
            s += ch.print_indented(indent+1)
        return s
