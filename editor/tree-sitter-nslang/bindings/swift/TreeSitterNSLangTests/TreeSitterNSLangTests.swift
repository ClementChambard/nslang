import XCTest
import SwiftTreeSitter
import TreeSitterNslang

final class TreeSitterNslangTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_nslang())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading ns lang grammar")
    }
}
