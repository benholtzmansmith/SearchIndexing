import org.scalatest.{ FunSpec, Matchers }

import scala.collection.mutable.{ Map => MMap }

class DictionarySpec extends FunSpec with Matchers {
  //Test Dictionary
  describe( "An empty dictionary" ) {
    val emptyDicionary = Dictionary()
    val newDictionary = emptyDicionary.add( "ben" )
    it("should add a new entry"){
      assert( newDictionary.index == MMap( "b" -> List( "ben" ) ) )
    }
  }
  describe( "A non-empty dictionary" ) {
    describe("when word without a previous key"){
      val dict = Dictionary( MMap( "a" -> List( "alpha" ) ) )
      describe("gets added to the index"){
        val newDict = dict.add( "ben" )
        it("should make a new entry"){
          assert( newDict.index == MMap( "b" -> List( "ben" ), "a" -> List( "alpha" ) ) )
        }
      }
    }
    describe("when word with a previous key"){
      val dict = Dictionary( MMap( "a" -> List( "alpha" ), "b" -> List( "ben" ) ) )
      describe("gets added to the index"){
        val newDict = dict.add( "adam" )
        it("should make a new entry"){
          assert( newDict.index == MMap( "b" -> List( "ben" ), "a" -> List( "alpha", "adam" ) ) )
        }
      }
    }
  }
  describe("find"){
    it("should find the correct values for a key"){
      val dict = Dictionary( MMap( "a" -> List( "alpha" ), "b" -> List( "ben" ) ) )
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
      assert( newDictionary.index == MMap( List( "be" ) -> List( "ben" ) ) )
    }
  }
  describe( "A non-empty dictionary" ) {
    describe("when word without a previous key"){
      val dict = ThreeStringKeyDictionary( MMap( List( "a" ) -> List( "alpha" ) ) )
      describe("gets added to the index"){
        val newDict = dict.add( "ben" )
        it("should make a new entry"){
          assert( newDict.index == MMap( List( "a", "be" ) -> List( "alpha", "ben" ) ) )
        }
      }
    }
    describe("when word with a previous key"){
      val dict = ThreeStringKeyDictionary( MMap( List( "a", "ab", "ac" ) -> List( "alpha" ) ) )
      describe("gets added to the index"){
        val newDict = dict.add( "ben" )
        it("should make a new entry"){
          assert( newDict.index == MMap( List( "a", "ab", "ac" ) -> List( "alpha" ), List( "be" ) -> List( "ben" ) ) )
        }
      }
    }
  }
  describe("find"){
    it("should find the correct values for a key"){
      val dict = ThreeStringKeyDictionary( MMap(
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
        MMap(
          List( "a", "b") ->
            NestedIndex(
              MMap( List( "aab", "aac" ) -> NestedIndex( MMap() ) )
            ),
          List( "c", "d") -> NestedIndex( MMap() )
        )
      )
      val values = dict.find( "aa" )
      assert( values == List("aab", "aac"))
    }
    it("should not find values that are below"){
      val dict = NestedIndex(
        MMap(
          List( "a", "b") ->
            NestedIndex(
              MMap( List( "aab", "aac" ) ->
                NestedIndex(
                  MMap(
                    List( "aabaa", "aacaa" ) -> NestedIndex( MMap() )
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
  describe("add"){
    it("should add values"){
      val dict = NestedIndex(
        MMap(
          List( "a", "b") ->
            NestedIndex(
              MMap( List( "ab" ) -> NestedIndex( MMap() ) )
            )
        )
      )

      val expected = NestedIndex(
        MMap(
          List( "a", "b") ->
            NestedIndex(
              MMap(
                List( "ab" ) -> NestedIndex( MMap() ),
                List( "ac" ) -> NestedIndex( MMap( List( "acd" ) -> NestedIndex( MMap() )) )
              )
            )
        )
      )
      dict.add("acd")
      assert(dict == expected)
    }
  }
}

class SortedListOfWordsSpec extends FunSpec with Matchers {
  describe("contains"){
    describe("if the list contains the word"){
      it("should return true"){
        assert(SortedListOfWords(List("alpha", "beta", "charlie")).contains("beta"))
      }
    }
    describe("if word is not contained"){
      it("should return false"){
        assert(!SortedListOfWords(List("alpha", "beta", "charlie")).contains("delta"))
      }
    }
    describe("if the list is empty"){
      it("should return false"){
        assert(!SortedListOfWords(List()).contains("delta"))
      }
    }
  }
}