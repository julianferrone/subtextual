# Subtextual

**Subtextual** is a Haskell library for parsing [Subtext, a text-based, block-oriented hypertext format](https://github.com/subconsciousnetwork/subtext).

## Core block types for Subtext Extended

### Tag blocks

Tag blocks start with `!`. A tag can be made of any number of non-whitespace characters (e.g. alphanumerics, dashes, underscores, slashes).

### Key-value pair blocks

Key-value pair blocks start with `!`, with the key and value separated by whitespace characters.

A key is composed of any number of non-whitespace characters.

A value is composed of any number of characters, including whitespace.

### Triple blocks

Triple blocks start with `&`. The three parts of a triple---subject, predicate, and object---are delimited by spaces. 

The object is composed of any number of characters, including whitespace.

### Transclusion blocks

Transclusion blocks start with a `$`, and denote that the content from another document must be fetched and inserted into the block.

Transclusion blocks refer only to local Subtext Extended pages (whose names are akin to slashlinks).

There are 4 options to select what content to refer to in a document:

| Option                                          | Syntax            |
| ----------------------------------------------- | ----------------- |
| All the content of the referenced document      | `$ doc`           |
| The first `n` lines of the referenced document. | `$ doc \| n`      |
| `n` lines after line `m`. Lines are 0-indexed.  | `$ doc \| m n`    |
| The section under a given heading block.        | `$ doc # heading` |