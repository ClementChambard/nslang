package tree_sitter_nslang_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_nslang "github.com/tree-sitter/tree-sitter-nslang/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_nslang.Language())
	if language == nil {
		t.Errorf("Error loading ns lang grammar")
	}
}
