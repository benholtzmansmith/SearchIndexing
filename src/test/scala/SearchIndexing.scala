import org.scalatest.{ FunSuite, Matchers }

import scala.collection.mutable.{ Map => MM }

class IndexSpec extends FunSuite with Matchers {
  //Test Dictionary
  test( "Dictionary empty dictionary gets new entry" ) {
    val emptyDicionary = Dictionary()

    val newDictionary = emptyDicionary.add( "ben" )
    assert( newDictionary.index == MM( "b" -> List( "ben" ) ) )
  }
  test( "Dictionary non empty dictionary word without a previous key gets added to the index" ) {
    val dict = Dictionary( MM( "a" -> List( "alpha" ) ) )

    val newDict = dict.add( "ben" )
    assert( newDict.index == MM( "b" -> List( "ben" ), "a" -> List( "alpha" ) ) )
  }
  test( "Dictionary non empty dictionary word with a previous key gets added to the index" ) {
    val dict = Dictionary( MM( "a" -> List( "alpha" ), "b" -> List( "ben" ) ) )

    val newDict = dict.add( "adam" )
    assert( newDict.index == MM( "b" -> List( "ben" ), "a" -> List( "alpha", "adam" ) ) )
  }
  //Test ThreeStringKeyDictionary
  test( "ThreeStringKeyDictionary empty dictionary gets new entry" ) {
    val emptyDicionary = ThreeStringKeyDictionary()

    val newDictionary = emptyDicionary.add( "ben" )
    assert( newDictionary.index == MM( List( "be" ) -> List( "ben" ) ) )
  }
  test( "ThreeStringKeyDictionary [incomplete key] non empty dictionary word without a previous key gets added to the index" ) {
    val dict = ThreeStringKeyDictionary( MM( List( "a" ) -> List( "alpha" ) ) )

    val newDict = dict.add( "ben" )
    assert( newDict.index == MM( List( "a", "be" ) -> List( "alpha", "ben" ) ) )
  }
  test( "ThreeStringKeyDictionary [complete key] non empty dictionary word without a previous key gets added to the index" ) {
    val dict = ThreeStringKeyDictionary( MM( List( "a", "ab", "ac" ) -> List( "alpha" ) ) )

    val newDict = dict.add( "ben" )
    assert( newDict.index == MM( List( "a", "ab", "ac" ) -> List( "alpha" ), List( "be" ) -> List( "ben" ) ) )
  }
}
