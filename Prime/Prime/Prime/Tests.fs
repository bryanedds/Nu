// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

module Prime.Tests
open Xunit

/// A simple LunTrie test.
let [<Fact>] lunTrieTest =
    let a = Lun.make "a"
    let ab = Lun.make "ab"
    let ccc = Lun.make "ccc"
    let trie = LunTrie.empty
    Assert.False (LunTrie.containsKey a trie)
    Assert.False (LunTrie.containsKey ab trie)
    Assert.False (LunTrie.containsKey ccc trie)
    let trie = LunTrie.add a a.LunHash trie
    Assert.True (LunTrie.containsKey a trie)
    Assert.Equal ((LunTrie.tryFind a trie).Value, a.LunHash)
    let trie = LunTrie.add ab ab.LunHash trie
    Assert.True (LunTrie.containsKey ab trie)
    Assert.Equal ((LunTrie.tryFind a trie).Value, a.LunHash)
    let trie = LunTrie.add ccc ccc.LunHash trie
    Assert.True (LunTrie.containsKey ccc trie)
    Assert.Equal ((LunTrie.tryFind a trie).Value, a.LunHash)