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
}