import scala.collection.mutable.{ Map => MM }

/** [
  *   a: [ 'alpha', 'amanada'],
  *   b: [ 'beta', brian]
  *  ]
  *
  */
object Dictionary {
  def apply(): Dictionary = new Dictionary( MM() )
}

case class Dictionary( index: MM[String, List[String]] ) {
  def find(key:String):List[String] = {
    index.find(_._1 == key).map(_._2).toList.flatten
  }
  def add( string: String ): Dictionary = {
    val firstLetter = string.slice( 0, 1 )
    val key = index.get( firstLetter )
    val update = key match {
      case Some( k ) => k :+ string
      case None      => List( string )
    }
    Dictionary( index ++ MM( firstLetter -> update ) )
  }
}

/** [
  *   [aa, ab, ac]: [ 'ace', 'abe'],
  *   [ad, ae, af]: [ 'adam', 'afro'],
  *  ]
  *
  */

object ThreeStringKeyDictionary {
  def apply(): ThreeStringKeyDictionary = new ThreeStringKeyDictionary( MM() )
}
case class ThreeStringKeyDictionary( index: MM[List[String], List[String]] ) {
  def find(key:String):List[String] = {
    index.find(_._1.contains(key)).map(_._2).toList.flatten
  }
  def add( string: String ): ThreeStringKeyDictionary = {
    val firstTwo = string.slice( 0, 2 )
    val maybeKey: Option[List[String]] = index.keys.find( _.contains( firstTwo ) )
    val update: MM[List[String], List[String]] = maybeKey match {
      case Some( key ) =>
        val valuesOpt = index.get( key )
        valuesOpt match {
          case Some( values ) => MM( key -> { values :+ string } )
          case None           => MM( key -> List( string ) )
        }
      case None => {
        val matchedKeys = index.keys.find( _.length < 3 )
        matchedKeys match {
          case Some( key ) =>
            val updatedValue = index.get( key ).toList.flatten :+ string
            val updatedKey = key :+ firstTwo
            index.remove( key )
            MM( updatedKey -> updatedValue )
          case None =>
            MM( List( firstTwo ) -> List( string ) )
        }
      }
    }
    ThreeStringKeyDictionary( index ++ update )
  }
}