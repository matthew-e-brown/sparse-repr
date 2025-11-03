use std::fmt::Display;

use indexmap::map::Entry;
use rayon::prelude::*;

type IndexMap<K, V> = indexmap::IndexMap<K, V, ahash::RandomState>;

/// Interface for creating a new [Graph].
#[derive(Debug)]
pub struct GraphBuilder {
    // The option in the CLI is for "undirected", since the default is "directed," but it feels weird to have the bool
    // be for "undirected or not."
    directed: bool,
    multiple: bool,
    include_zero: bool,
}

// A builder is nice because it is more library-friendly if we ever want to make this stuff usable from C++ over FFI.
impl GraphBuilder {
    pub fn new() -> Self {
        Self {
            directed: true,
            multiple: false,
            include_zero: true,
        }
    }

    /// Controls whether or not the resulting graph tracks directed edges or not. The default is `true`.
    pub fn directed(self, yes: bool) -> Self {
        Self { directed: yes, ..self }
    }

    /// Controls whether or not the resulting graph will track multiple copies of the same edge, or if it should ignore
    /// them. The default is `false`.
    pub fn multiple(self, yes: bool) -> Self {
        Self { multiple: yes, ..self }
    }

    /// Controls whether or not the resulting graph should output a final vertex array with an empty (zero) spot at the
    /// beginning. This causes vertices to be renumbered to one-indexed. The default is `true`.
    pub fn include_zero(self, yes: bool) -> Self {
        Self { include_zero: yes, ..self }
    }

    /// Create a graph from this builder.
    pub fn build<'a>(&self) -> Graph<'a> {
        Graph {
            map: Default::default(),
            degree_sum: 0,
            directed: self.directed,
            multiple: self.multiple,
            include_zero: self.include_zero,
        }
    }
}

/// A graph.
///
/// Supports both directed and undirected modes. Also supports multiple edges.
#[derive(Debug, Clone)]
pub struct Graph<'a> {
    /// An ordered map of all vertices found in the input graph and their destinations.
    ///
    /// Each vertex maps to another ordered map, whose keys are the destination vertex's index in the outer map, and
    /// whose values are the amount of times that specific edge has appeared (only applicable in multi-mode).
    map: IndexMap<Vertex<'a>, IndexMap<Vertex<'a>, usize>>,
    /// The total number of all edges in this edge-set.
    degree_sum: usize,
    /// Whether this graph should account for directions or not.
    directed: bool,
    /// Whether or not this graph should allow multiple edges or not.
    multiple: bool,
    /// Whether or not this graph should emit an empty spot at the front of the `N` array in its output.
    include_zero: bool,
}

impl<'a> Graph<'a> {
    /// Gets the *order* of this graph, or the number of vertices it contains.
    pub fn num_verts(&self) -> usize {
        self.map.len()
    }

    /// Get the degree sum of this graph.
    pub fn num_edges(&self) -> usize {
        self.degree_sum
    }

    /// Add a single vertex (without any edges) to this graph.
    pub fn add_vertex(&mut self, v: Vertex<'a>) {
        self.insert_single_vertex(v);
    }

    /// Add a single edge to this edge-set (graph).
    pub fn add_edge(&mut self, v1: Vertex<'a>, v2: Vertex<'a>) {
        self.insert_single_edge(v1, v2);
        if !self.directed {
            self.insert_single_edge(v2, v1);
        }
    }

    /// Adds a single vertex to this graph and returns its index. Does nothing if the vertex is already present in the
    /// graph.
    fn insert_single_vertex(&mut self, v: Vertex<'a>) -> usize {
        match self.map.entry(v) {
            Entry::Occupied(e) => e.index(),
            Entry::Vacant(e) => e
                .insert_entry(IndexMap::with_capacity_and_hasher(0, Default::default())) // 0 cap = won't allocate
                .index(),
        }
    }

    fn insert_single_edge(&mut self, v1: Vertex<'a>, v2: Vertex<'a>) {
        // Ensure both vertices have a spot in our map (need to insert v1 first to preserve order, but can't use any
        // `&mut`-returning methods to do so, since we need another `&mut` to insert v2; so instead we get its index to
        // quickly re-select afterwards):
        let v1_idx = self.insert_single_vertex(v1);
        self.insert_single_vertex(v2);

        // How many edges currently go from v1->v2? Don't want to increment unless it's zero, or if we're in multi-mode.
        let count = self.map[v1_idx].entry(v2).or_insert(0);
        if *count == 0 || self.multiple {
            *count += 1;
            self.degree_sum += 1;
        }
    }

    /// Sorts all vertices in his graph by their default sorting order.
    pub fn sort_vertices(&mut self) {
        // Sort our main list in addition to all their adjacency lists:
        self.map.par_sort_unstable_keys();
        self.map.par_iter_mut().for_each(|(_vertex, edges)| edges.sort_unstable_keys());
    }

    /* pub fn generate_csr<U>(&self) -> Result<(Vec<U>, Vec<U>), U::Error>
    where
        U: Sized + TryFrom<usize>, */

    pub fn generate_csr(&self) -> (Box<[usize]>, Box<[usize]>) {
        let has_zero = self.include_zero;

        // (N and F are the variable names used in the source paper)
        let mut n = Vec::<usize>::with_capacity(self.num_verts() + (has_zero as usize));
        let mut f = Vec::<usize>::with_capacity(self.num_edges());

        if has_zero {
            n.push(0);
        }

        // Assuming the vertices have been sorted (or left unsorted intentionally), the order they appear in the map is
        // the order we want to put them into N. We just need to count the out-degree as we go.
        let mut i = 0;
        for v_outs in self.map.values() {
            n.push(i);
            i += v_outs.values().sum::<usize>(); // Add the total edge-count for this vertex to `i`
        }

        // Ensure we've been counting correctly: after the last vertex's
        assert_eq!(i, self.num_edges(), "final index in N should match total edge-count");

        // The second part we can do in parallel: every index in F is one vertex; but it needs to be the *index* of the
        // vertex in N. That means we need to do a lookup for every vertex to find its index (we can't track their
        // indices in the edge maps because we need to ensure that we get the right indices even *after* sorting them).
        // Now that we've populated N, We can figure out each vertex's range in F by indexing into N.

        // For every vertex, its allocated region in F is given by `N[i+1]..N[i+2]` or `N[i]..N[i+1]` (exclusive)
        // depending on the `has_zero` flag, where `i` is that vertex's index in *our* map. Because `N` increases
        // monotonically, these regions of `F` will not overlap.

        (0..self.num_verts()).into_par_iter().for_each(|i| {
            // Find the region of F we want to index:
            let ni = i + (has_zero as usize);
            let f1 = n[ni];
            let f2 = n.get(ni + 1).copied().unwrap_or(self.num_edges());

            // Double check our assumptions and create a slice:
            debug_assert!(f1 <= self.num_edges(), "indices in N should fit within F");
            debug_assert!(f2 <= self.num_edges(), "indices in N should fit within F");

            // SAFETY:
            // - The length of F is the degree-sum of the graph, and N is a cumulative sum of vertex degrees up to that
            //   amount. Therefore all values of N are less than the length of F, so `f1` is within the bounds of `f`
            //   and `f_ptr.add(f1)` is within allocation bounds.
            // - `f1` and `f2` are both in bounds, so `f2 - f1` cannot be longer than the length of the slice.
            // - The i'th region of F we are accessing starts at `f1` and ends at `f2`; the i+1'th region will start at
            //   `f2`. Since N is monotonically increasing, either `f1 == f2` or `f1 < f2`. If `f1 < f2`, there is no
            //   overlap; if `f1 == f2`, then there is "overlap", but this region has size zero, and so no writes will
            //   be performed, and no reference/slice aliasing will occur.
            let f_ptr = f.as_ptr().cast_mut();
            let slice = unsafe { std::slice::from_raw_parts_mut(f_ptr.add(f1), f2 - f1) };

            // Loop through the outbound edges of the i'th vertex, find their destinations' indices, and write them to
            // our subslice of F:
            let edges = &self.map[i];
            let mut j = 0;
            for (dest, &count) in edges {
                let index = self.map.get_index_of(dest).unwrap();
                slice[j..j + count].fill(index + (has_zero as usize));
                j += count;
            }
        });

        // SAFETY: After the `par_iter` returns, each iteration will have written a total number of edges into F equal
        // to the sum of each vertex's out-degree. This is is the same length that was used to initialize F's capacity.
        unsafe {
            f.set_len(self.num_edges());
        }

        (n.into_boxed_slice(), f.into_boxed_slice())
    }

    /* pub fn generate_csr_u8(&self) -> Result<(Vec<u8>, Vec<u8>), TryFromIntError> {
        self.generate_csr::<u8>()
    }

    pub fn generate_csr_u16(&self) -> Result<(Vec<u16>, Vec<u16>), TryFromIntError> {
        self.generate_csr::<u16>()
    }

    pub fn generate_csr_u32(&self) -> Result<(Vec<u32>, Vec<u32>), TryFromIntError> {
        self.generate_csr::<u32>()
    }

    pub fn generate_csr_u64(&self) -> Result<(Vec<u64>, Vec<u64>), TryFromIntError> {
        self.generate_csr::<u64>()
    }

    pub fn generate_csr_usize(&self) -> (Vec<usize>, Vec<usize>) {
        self.generate_csr::<usize>().unwrap()
    } */

    /// Returns an iterator that yields the original vertex and its final index in the outputted CSR array(s).
    pub fn mappings(&self) -> impl Iterator<Item = (Vertex<'a>, usize)> {
        let o = self.include_zero as usize;
        self.map.keys().enumerate().map(move |(i, v)| (*v, i + o))
    }
}

/// A value which is either a string or a parsed number.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Vertex<'a>(VertexType<'a>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum VertexType<'a> {
    Label(&'a str),
    Number(usize),
}

impl<'a> Vertex<'a> {
    pub fn from_str(s: &'a str) -> Self {
        match s.parse() {
            Ok(n) => Vertex(VertexType::Number(n)),
            Err(_) => Vertex(VertexType::Label(s.trim())),
        }
    }
}

impl<'a> Display for Vertex<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            VertexType::Label(str) => <&str as Display>::fmt(&str, f),
            VertexType::Number(n) => <usize as Display>::fmt(&n, f),
        }
    }
}

impl<'a> PartialEq<str> for Vertex<'a> {
    fn eq(&self, other: &str) -> bool {
        match self.0 {
            VertexType::Label(str) => str == other,
            VertexType::Number(n) => other.parse::<usize>().is_ok_and(|x| x == n),
        }
    }
}

impl<'a> PartialEq<&str> for Vertex<'a> {
    fn eq(&self, other: &&str) -> bool {
        self == *other
    }
}

impl<'a> PartialEq<usize> for Vertex<'a> {
    fn eq(&self, other: &usize) -> bool {
        match &self.0 {
            VertexType::Label(_) => false,
            VertexType::Number(n) => *other == *n,
        }
    }
}

impl<'a> Ord for Vertex<'a> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (&self.0, &other.0) {
            (VertexType::Number(u), VertexType::Number(v)) => u.cmp(&v),
            (VertexType::Label(u), VertexType::Label(v)) => u.cmp(&v),
            (VertexType::Number(_), VertexType::Label(_)) => std::cmp::Ordering::Less,
            (VertexType::Label(_), VertexType::Number(_)) => std::cmp::Ordering::Greater,
        }
    }
}

impl<'a> PartialOrd for Vertex<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
