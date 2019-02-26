import org.scalatest.{ FunSpec, Matchers }

import scala.collection.mutable.{ Map => MM }

class DictionarySpec extends FunSpec with Matchers {
  //Test Dictionary
  describe( "An empty dictionary" ) {
    val emptyDicionary = Dictionary()
    val newDictionary = emptyDicionary.add( "ben" )
    it("should add a new entry"){
      assert( newDictionary.index == MM( "b" -> List( "ben" ) ) )
    }
  }
  describe( "A non-empty dictionary" ) {
    describe("when word without a previous key"){
      val dict = Dictionary( MM( "a" -> List( "alpha" ) ) )
      describe("gets added to the index"){
        val newDict = dict.add( "ben" )
        it("should make a new entry"){
          assert( newDict.index == MM( "b" -> List( "ben" ), "a" -> List( "alpha" ) ) )
        }
      }
    }
    describe("when word with a previous key"){
      val dict = Dictionary( MM( "a" -> List( "alpha" ), "b" -> List( "ben" ) ) )
      describe("gets added to the index"){
        val newDict = dict.add( "adam" )
        it("should make a new entry"){
          assert( newDict.index == MM( "b" -> List( "ben" ), "a" -> List( "alpha", "adam" ) ) )
        }
      }
    }
  }
  describe("find"){
    it("should find the correct values for a key"){
      val dict = Dictionary( MM( "a" -> List( "alpha" ), "b" -> List( "ben" ) ) )
      val values = dict.find( "a" )
      assert( values == List("alpha"))
    }
  }
}


class ThreeStringKeyDictionarySpec extends FunSpec with Matchers {

  describe( "An empty dictionary" ) {
    val emptyDicionary = ThreeStringKeyDictionary()
    val newDictionary = emptyDicionary.add( "ben" )
    it("should add a new entry"){
      assert( newDictionary.index == MM( List( "be" ) -> List( "ben" ) ) )
    }
  }
  describe( "A non-empty dictionary" ) {
    describe("when word without a previous key"){
      val dict = ThreeStringKeyDictionary( MM( List( "a" ) -> List( "alpha" ) ) )
      describe("gets added to the index"){
        val newDict = dict.add( "ben" )
        it("should make a new entry"){
          assert( newDict.index == MM( List( "a", "be" ) -> List( "alpha", "ben" ) ) )
        }
      }
    }
    describe("when word with a previous key"){
      val dict = ThreeStringKeyDictionary( MM( List( "a", "ab", "ac" ) -> List( "alpha" ) ) )
      describe("gets added to the index"){
        val newDict = dict.add( "ben" )
        it("should make a new entry"){
          assert( newDict.index == MM( List( "a", "ab", "ac" ) -> List( "alpha" ), List( "be" ) -> List( "ben" ) ) )
        }
      }
    }
  }
  describe("find"){
    it("should find the correct values for a key"){
      val dict = ThreeStringKeyDictionary( MM(
        List( "al", "ac" ) -> List( "alpha" ),
        List( "ad", "as" ) -> List( "adam" )
      ) )
      val values = dict.find( "al" )
      assert( values == List("alpha"))
    }
  }
}

class NestedIndexSpec extends FunSpec with Matchers {

  describe("index contains"){
    describe("when the index keys contain a value that matches the start of the search term"){
      it("should return true"){
        val keys = List("aa", "ab", "ac")
        val searchTerm = "abe"
        assert(NestedIndex.indexKeysContains(keys, searchTerm) == true)
      }
    }
    describe("when the index keys don't contain a value that matches the start of the search term"){
      it("should return false"){
        val keys = List("aa", "ab", "ac")
        val searchTerm = "adam"
        assert(!NestedIndex.indexKeysContains(keys, searchTerm))
      }
    }
  }
  describe("find"){
    it("should find values that are keys that contain the search term"){
      val dict = NestedIndex(
        MM(
          List( "a", "b") ->
            NestedIndex(
              MM( List( "aab", "aac" ) -> NestedIndex( MM() ) )
            ),
          List( "c", "d") -> NestedIndex( MM() )
        )
      )
      val values = dict.find( "aa" )
      assert( values == List("aab", "aac"))
    }
    it("should not find values that are below"){
      val dict = NestedIndex(
        MM(
          List( "a", "b") ->
            NestedIndex(
              MM( List( "aab", "aac" ) ->
                NestedIndex(
                  MM(
                    List( "aabaa", "aacaa" ) -> NestedIndex( MM() )
                  )
                )
              )
            )
        )
      )
      val values = dict.find( "aa" )
      assert( values == List("aabaa", "aacaa"))
    }
  }
}