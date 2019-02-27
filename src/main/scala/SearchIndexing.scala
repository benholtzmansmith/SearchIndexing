import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}

/** [
  *   a: [ 'alpha', 'amanada'],
  *   b: [ 'beta', brian]
  *  ]
  *
  */
case class Dictionary( index: MMap[String, List[String]] ) {
  def find(searchTerm:String):List[String] = {
    index.find(_._1 == searchTerm).map(_._2).toList.flatten
  }
  def add( string: String ): Dictionary = {
    val firstLetter = string.slice( 0, 1 )
    val key = index.get( firstLetter )
    val update = key match {
      case Some( k ) => k :+ string
      case None      => List( string )
    }
    Dictionary( index ++ MMap( firstLetter -> update ) )
  }
}
object Dictionary {
  def apply(): Dictionary = new Dictionary( MMap() )
}


/** [
  *   [aa, ab, ac]: [ 'ace', 'abe'],
  *   [ad, ae, af]: [ 'adam', 'afro'],
  *  ]
  *
  */

case class ThreeStringKeyDictionary( index: MMap[List[String], List[String]] ) {
  def find(searchTerm:String):List[String] = {
    index.find(_._1.contains(searchTerm)).map(_._2).toList.flatten
  }
  def add( string: String ): ThreeStringKeyDictionary = {
    val firstTwo = string.slice( 0, 2 )
    val maybeKey: Option[List[String]] = index.keys.find( _.contains( firstTwo ) )
    val update: MMap[List[String], List[String]] = maybeKey match {
      case Some( key ) =>
        val valuesOpt = index.get( key )
        valuesOpt match {
          case Some( values ) => MMap( key -> { values :+ string } )
          case None           => MMap( key -> List( string ) )
        }
      case None => {
        val matchedKeys = index.keys.find( _.length < 3 )
        matchedKeys match {
          case Some( key ) =>
            val updatedValue = index.get( key ).toList.flatten :+ string
            val updatedKey = key :+ firstTwo
            index.remove( key )
            MMap( updatedKey -> updatedValue )
          case None =>
            MMap( List( firstTwo ) -> List( string ) )
        }
      }
    }
    ThreeStringKeyDictionary( index ++ update )
  }
}

object ThreeStringKeyDictionary {
  def apply(): ThreeStringKeyDictionary = new ThreeStringKeyDictionary( MMap() )
}
/** {
  *   ['aa', 'ab']: {
  *     ['aaa', 'aba']: {
  *       ['aaaa']: {}
  *     }
  *     ['aab', 'abb']:...
  *   }
  *   ['ac', 'ad']: ['ada'],
  * }
  *
  * if (value.isEmpty() ) return keys
  * else value.continueSearch()
  *
  */
//Recursive Data Structure
case class NestedIndex(index: MMap[List[String], NestedIndex] ) {

  /**
    * Returns deepest match in the index tree
    * So for search term "aaa"
    *
    * With index:
    *   ['a'] : { ['aa']: { ['aac': {} ]} }
    *
    * Only 'aac' is returned
    *
    * */
  @tailrec
  private def search(branch: NestedIndex, searchTerm:String, matchedKeys:List[String]): List[String] = {
    branch.index.find{ case (keys, _) => NestedIndex.indexKeysContains(keys, searchTerm) } match {
      case Some((keys, matched)) => search(matched, searchTerm, keys)
      case None => matchedKeys
    }
  }

  def find(searchTerm:String):List[String] = {
    search(this, searchTerm, Nil)
  }

  def searchToKey(searchTerm:String) = {
    searchTerm.slice(0,2).toLowerCase
  }

  private def addRecusive(branch:NestedIndex, searchTerm:String):NestedIndex = {
    branch.index.find{ case (keys, _) => NestedIndex.indexKeysContains(keys, searchTerm) } match {
      case Some((key, nextBranch)) => addRecusive(nextBranch, searchTerm)
      case None =>
        branch.index.update(
        List(searchToKey(searchTerm)), NestedIndex(MMap(List(searchTerm) -> NestedIndex()))
      ); branch
    }
  }

  def add(searchTerm:String) = {
    addRecusive(this, searchTerm)
  }
}
object NestedIndex{
  def apply(): NestedIndex = new NestedIndex( MMap() )

  def indexKeysContains(candidates:List[String], searchTerm:String):Boolean = {
    assert(
      candidates.forall( candidate => candidates.headOption.exists(_.length == candidate.length)),
      "all candidates need to be the same length"
    )
    candidates.headOption.map{_.length} match {
      case Some(candidateLength) =>
        val start = searchTerm.slice(0, candidateLength )
        candidates.exists( _.contains(start))
      case None => false
    }
  }
}