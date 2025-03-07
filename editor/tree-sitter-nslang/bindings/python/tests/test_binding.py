from unittest import TestCase

import tree_sitter
import tree_sitter_nslang


class TestLanguage(TestCase):
    def test_can_load_grammar(self):
        try:
            tree_sitter.Language(tree_sitter_nslang.language())
        except Exception:
            self.fail("Error loading ns lang grammar")
